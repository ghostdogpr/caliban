package caliban

import caliban.Configurator.ExecutionConfiguration
import caliban.HttpUtils.DeferMultipart
import caliban.ResponseValue.StreamValue
import caliban.interop.jsoniter.ValueJsoniter
import caliban.uploads.{ FileMeta, GraphQLUploadRequest, Uploads }
import caliban.wrappers.Caching
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import zio._
import zio.http._
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.stream.ZStream

import java.nio.charset.StandardCharsets.UTF_8
import scala.util.Try
import scala.util.control.NonFatal

final private class QuickRequestHandler[-R, E](interpreter: GraphQLInterpreter[R, E]) {
  import QuickRequestHandler._

  def configure(config: ExecutionConfiguration)(implicit trace: Trace): QuickRequestHandler[R, E] =
    new QuickRequestHandler[R, E](
      interpreter.wrapExecutionWith[R, E](Configurator.setWith(config)(_))
    )

  def configure[R1](configurator: QuickAdapter.Configurator[R1])(implicit
    trace: Trace
  ): QuickRequestHandler[R & R1, E] =
    new QuickRequestHandler[R & R1, E](
      interpreter.wrapExecutionWith[R & R1, E](exec => ZIO.scoped[R1 & R](configurator *> exec))
    )

  def handleHttpRequest(request: Request)(implicit trace: Trace): URIO[R, Response] =
    transformHttpRequest(request)
      .flatMap(executeRequest(request.method, _))
      .map(transformResponse(request, _))
      .merge

  def handleUploadRequest(request: Request)(implicit trace: Trace): URIO[R, Response] =
    transformUploadRequest(request).flatMap { case (req, fileHandle) =>
      executeRequest(request.method, req)
        .map(transformResponse(request, _))
        .provideSomeLayer[R](fileHandle)
    }.merge

  private def transformHttpRequest(httpReq: Request)(implicit trace: Trace): IO[Response, GraphQLRequest] = {
    val queryParams = httpReq.url.queryParams

    def extractFields(key: String): Either[Response, Option[Map[String, InputValue]]] =
      try Right(queryParams.get(key).map(readFromString[InputValue.ObjectValue](_).fields))
      catch { case NonFatal(_) => Left(badRequest(s"Invalid $key query param")) }

    def fromQueryParams: Either[Response, GraphQLRequest] =
      for {
        vars <- extractFields("variables")
        exts <- extractFields("extensions")
      } yield GraphQLRequest(
        query = queryParams.get("query"),
        operationName = queryParams.get("operationName"),
        variables = vars,
        extensions = exts
      )

    def isGqlJson =
      httpReq.header(Header.ContentType).exists { h =>
        h.mediaType.subType.equalsIgnoreCase("graphql") &&
        h.mediaType.mainType.equalsIgnoreCase("application")
      }

    def decodeApplicationGql() =
      httpReq.body.asString.mapBoth(_ => BodyDecodeErrorResponse, b => GraphQLRequest(Some(b)))

    def decodeJson() =
      httpReq.body.asArray
        .flatMap(arr => ZIO.attempt(readFromArray[GraphQLRequest](arr)))
        .orElseFail(BodyDecodeErrorResponse)

    val resp = {
      if (httpReq.method == Method.GET || queryParams.get("query").isDefined)
        ZIO.fromEither(fromQueryParams)
      else {
        val req = if (isGqlJson) decodeApplicationGql() else decodeJson()
        if (isFtv1Request(httpReq)) req.map(_.withFederatedTracing)
        else req
      }
    }

    resp.tap(r => if (r.isEmpty) ZIO.fail(badRequest("No GraphQL query to execute")) else ZIO.unit)
  }

  private def transformUploadRequest(
    request: Request
  )(implicit trace: Trace): IO[Response, (GraphQLRequest, ULayer[Uploads])] = {
    def extractField[A](
      partsMap: Map[String, FormField],
      key: String
    )(implicit jsonValueCodec: JsonValueCodec[A]): IO[Response, A] =
      ZIO
        .fromOption(partsMap.get(key))
        .flatMap(_.asChunk)
        .flatMap(v => ZIO.attempt(readFromArray[A](v.toArray)))
        .orElseFail(Response.badRequest)

    def parsePath(path: String): List[Either[String, Int]] =
      path.split('.').toList.map { segment =>
        try Right(segment.toInt)
        catch { case _: NumberFormatException => Left(segment) }
      }

    for {
      partsMap   <- request.body.asMultipartForm.mapBoth(_ => Response.internalServerError, _.map)
      gqlReq     <- extractField[GraphQLRequest](partsMap, "operations")
      rawMap     <- extractField[Map[String, List[String]]](partsMap, "map")
      filePaths   = rawMap.map { case (key, value) => (key, value.map(parsePath)) }.toList
                      .flatMap(kv => kv._2.map(kv._1 -> _))
      handler     = Uploads.handler(handle =>
                      (for {
                        uuid <- Random.nextUUID
                        fp   <- ZIO.fromOption(partsMap.get(handle))
                        body <- fp.asChunk
                      } yield FileMeta(
                        uuid.toString,
                        body.toArray,
                        Some(fp.contentType.fullType),
                        fp.filename.getOrElse(""),
                        body.length
                      )).option
                    )
      uploadQuery = GraphQLUploadRequest(gqlReq, filePaths, handler)
      query       = if (isFtv1Request(request)) uploadQuery.remap.withFederatedTracing else uploadQuery.remap
    } yield query -> ZLayer(uploadQuery.fileHandle)

  }

  private def executeRequest(method: Method, req: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R, Response, GraphQLResponse[E]] = {
    val calibanMethod = if (method == Method.GET) HttpRequestMethod.GET else HttpRequestMethod.POST
    HttpRequestMethod.setWith(calibanMethod)(interpreter.executeRequest(req))
  }

  private def responseHeaders(headers: Headers, cacheDirective: Option[String]): Headers =
    cacheDirective.fold(headers)(headers.addHeader(Header.CacheControl.name, _))

  private def transformResponse(httpReq: Request, resp: GraphQLResponse[E])(implicit trace: Trace): Response = {
    def acceptsGqlJson: Boolean =
      httpReq.header(Header.Accept).exists { h =>
        h.mimeTypes.exists { mt =>
          mt.mediaType.subType.equalsIgnoreCase("graphql-response+json") &&
          mt.mediaType.mainType.equalsIgnoreCase("application")
        }
      }

    val cacheDirective = HttpUtils.computeCacheDirective(resp.extensions)

    resp match {
      case resp @ GraphQLResponse(StreamValue(stream), _, _, _) =>
        Response(
          Status.Ok,
          headers = responseHeaders(ContentTypeMultipart, None),
          body = Body.fromStream(encodeMultipartMixedResponse(resp, stream))
        )
      case resp if acceptsGqlJson                               =>
        Response(
          status = resp.errors.collectFirst { case _: CalibanError.ParsingError | _: CalibanError.ValidationError =>
            Status.BadRequest
          }.getOrElse(Status.Ok),
          headers = responseHeaders(ContentTypeGql, cacheDirective),
          body = encodeSingleResponse(resp, keepDataOnErrors = false, hasCacheDirective = cacheDirective.isDefined)
        )
      case resp                                                 =>
        Response(
          status = resp.errors.collectFirst { case HttpRequestMethod.MutationOverGetError => Status.BadRequest }
            .getOrElse(Status.Ok),
          headers = responseHeaders(ContentTypeJson, cacheDirective),
          body = encodeSingleResponse(resp, keepDataOnErrors = true, hasCacheDirective = cacheDirective.isDefined)
        )
    }
  }

  private def encodeSingleResponse(
    resp: GraphQLResponse[E],
    keepDataOnErrors: Boolean,
    hasCacheDirective: Boolean
  ): Body = {
    val excludeExtensions = if (hasCacheDirective) Some(Set(Caching.DirectiveName)) else None
    Body.fromChunk(Chunk.fromArray(writeToArray(resp.toResponseValue(keepDataOnErrors, excludeExtensions))))
  }

  private def encodeMultipartMixedResponse(
    resp: GraphQLResponse[E],
    stream: ZStream[Any, Throwable, ResponseValue]
  )(implicit trace: Trace): ZStream[Any, Throwable, Byte] = {
    import HttpUtils.DeferMultipart._
    val pipeline = createPipeline(resp)

    stream
      .via(pipeline)
      .map(writeToArray(_))
      .intersperse(InnerBoundary.getBytes(UTF_8), InnerBoundary.getBytes(UTF_8), EndBoundary.getBytes(UTF_8))
      .mapConcatChunk(Chunk.fromArray)
  }

  private def isFtv1Request(req: Request) =
    req.headers
      .get(GraphQLRequest.`apollo-federation-include-trace`)
      .exists(_.equalsIgnoreCase(GraphQLRequest.ftv1))

}

object QuickRequestHandler {
  private def badRequest(msg: String) =
    Response(Status.BadRequest, body = Body.fromString(msg))

  private val ContentTypeJson =
    Headers(Header.ContentType(MediaType.application.json))

  private val ContentTypeGql =
    Headers(Header.ContentType(MediaType("application", "graphql-response+json")))

  private val ContentTypeMultipart =
    Headers(Header.ContentType(MediaType.multipart.mixed.copy(parameters = DeferMultipart.DeferHeaderParams)))

  private val BodyDecodeErrorResponse =
    badRequest("Failed to decode json body")

  private implicit val inputObjectCodec: JsonValueCodec[InputValue.ObjectValue] =
    new JsonValueCodec[InputValue.ObjectValue] {
      override def decodeValue(in: JsonReader, default: InputValue.ObjectValue): InputValue.ObjectValue =
        ValueJsoniter.inputValueCodec.decodeValue(in, default) match {
          case o: InputValue.ObjectValue => o
          case _                         => in.decodeError("expected json object")
        }
      override def encodeValue(x: InputValue.ObjectValue, out: JsonWriter): Unit                        =
        ValueJsoniter.inputValueCodec.encodeValue(x, out)
      override def nullValue: InputValue.ObjectValue                                                    =
        null.asInstanceOf[InputValue.ObjectValue]
    }

  private implicit val stringListCodec: JsonValueCodec[Map[String, List[String]]] = JsonCodecMaker.make
}
