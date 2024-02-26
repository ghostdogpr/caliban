package caliban

import caliban.Configurator.ExecutionConfiguration
import caliban.HttpUtils.DeferMultipart
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import caliban.interop.jsoniter.ValueJsoniter
import caliban.uploads.{ FileMeta, GraphQLUploadRequest, Uploads }
import caliban.wrappers.Caching
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import zio._
import zio.http.Header.ContentType
import zio.http._
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.stream.ZStream

import java.nio.charset.StandardCharsets.UTF_8
import scala.util.control.NonFatal

final private class QuickRequestHandler[-R](interpreter: GraphQLInterpreter[R, Any]) {
  import QuickRequestHandler._

  def configure(config: ExecutionConfiguration)(implicit trace: Trace): QuickRequestHandler[R] =
    new QuickRequestHandler[R](
      interpreter.wrapExecutionWith[R, Any](Configurator.setWith(config)(_))
    )

  def configure[R1](configurator: QuickAdapter.Configurator[R1])(implicit
    trace: Trace
  ): QuickRequestHandler[R & R1] =
    new QuickRequestHandler[R & R1](
      interpreter.wrapExecutionWith[R & R1, Any](exec => ZIO.scoped[R1 & R](configurator *> exec))
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

    def decodeQueryParams(queryParams: QueryParams): Either[Response, GraphQLRequest] = {
      def extractField(key: String) =
        try Right(queryParams.get(key).map(readFromString[InputValue.ObjectValue](_).fields))
        catch { case NonFatal(_) => Left(badRequest(s"Invalid $key query param")) }

      for {
        vars <- extractField("variables")
        exts <- extractField("extensions")
      } yield GraphQLRequest(
        query = queryParams.get("query"),
        operationName = queryParams.get("operationName"),
        variables = vars,
        extensions = exts
      )
    }

    def decodeBody(body: Body) = {
      def decodeApplicationGql() =
        body.asString.mapBoth(_ => BodyDecodeErrorResponse, b => GraphQLRequest(Some(b)))

      def decodeJson() =
        body.asArray
          .flatMap(arr => ZIO.attempt(readFromArray[GraphQLRequest](arr)))
          .orElseFail(BodyDecodeErrorResponse)

      val isApplicationGql =
        httpReq.headers.get(ContentType.name).exists { h =>
          h.length >= 19 && { // Length of "application/graphql"
            MediaType.forContentType(h).exists { mt =>
              mt.subType.equalsIgnoreCase("graphql") &&
              mt.mainType.equalsIgnoreCase("application")
            }
          }
        }

      if (isApplicationGql) decodeApplicationGql() else decodeJson()
    }

    val queryParams = httpReq.url.queryParams

    (if (httpReq.method == Method.GET || queryParams.get("query").isDefined) {
       ZIO.fromEither(decodeQueryParams(queryParams))
     } else {
       val req = decodeBody(httpReq.body)
       if (isFtv1Request(httpReq)) req.map(_.withFederatedTracing)
       else req
     }).tap(r => ZIO.fail(EmptyRequestErrorResponse).when(r.isEmpty))

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

    def parsePath(path: String): List[PathValue] = path.split('.').toList.map(PathValue.parse)

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
  ): ZIO[R, Response, GraphQLResponse[Any]] = {
    val calibanMethod = if (method == Method.GET) HttpRequestMethod.GET else HttpRequestMethod.POST
    HttpRequestMethod.setWith(calibanMethod)(interpreter.executeRequest(req))
  }

  private def responseHeaders(headers: Headers, cacheDirective: Option[String]): Headers =
    cacheDirective.fold(headers)(headers.addHeader(Header.CacheControl.name, _))

  private def transformResponse(httpReq: Request, resp: GraphQLResponse[Any])(implicit trace: Trace): Response = {
    val accepts        = new HttpUtils.AcceptsGqlEncodings(httpReq.headers.get(Header.Accept.name))
    val cacheDirective = HttpUtils.computeCacheDirective(resp.extensions)

    resp match {
      case resp @ GraphQLResponse(StreamValue(stream), _, _, _) =>
        Response(
          Status.Ok,
          headers = responseHeaders(ContentTypeMultipart, None),
          body = Body.fromStream(encodeMultipartMixedResponse(resp, stream))
        )
      case resp if accepts.serverSentEvents                     =>
        Response.fromServerSentEvents(encodeTextEventStream(resp))
      case resp if accepts.graphQLJson                          =>
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
    resp: GraphQLResponse[Any],
    keepDataOnErrors: Boolean,
    hasCacheDirective: Boolean
  ): Body = {
    val excludeExtensions = if (hasCacheDirective) Some(Set(Caching.DirectiveName)) else None
    Body.fromChunk(Chunk.fromArray(writeToArray(resp.toResponseValue(keepDataOnErrors, excludeExtensions))))
  }

  private def encodeMultipartMixedResponse(
    resp: GraphQLResponse[Any],
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

  private def encodeTextEventStream(resp: GraphQLResponse[Any])(implicit trace: Trace) = {
    val stream = (resp.data match {
      case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
        stream.either.map {
          case Right(r)  => GraphQLResponse(ObjectValue(List(fieldName -> r)), resp.errors)
          case Left(err) => GraphQLResponse(ObjectValue(List(fieldName -> NullValue)), List(err))
        }
      case _                                                    =>
        ZStream.succeed(resp)
    }).map(resp => ServerSentEvent(writeToString(resp.toResponseValue), eventType = Some("next")))

    stream ++ ZStream.succeed(CompleteSse)
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
    Headers(Header.ContentType(MediaType.application.json).untyped)

  private val ContentTypeGql =
    Headers(Header.ContentType(MediaType("application", "graphql-response+json")).untyped)

  private val ContentTypeMultipart =
    Headers(Header.ContentType(MediaType.multipart.mixed.copy(parameters = DeferMultipart.DeferHeaderParams)).untyped)

  private val CompleteSse = ServerSentEvent("", Some("complete"))

  private val BodyDecodeErrorResponse =
    badRequest("Failed to decode json body")

  private val EmptyRequestErrorResponse =
    badRequest("No GraphQL query to execute")

  private implicit val inputObjectCodec: JsonValueCodec[InputValue.ObjectValue] =
    new JsonValueCodec[InputValue.ObjectValue] {
      private val inputValueCodec = ValueJsoniter.inputValueCodec

      override def decodeValue(in: JsonReader, default: InputValue.ObjectValue): InputValue.ObjectValue =
        inputValueCodec.decodeValue(in, default) match {
          case o: InputValue.ObjectValue => o
          case _                         => in.decodeError("expected json object")
        }
      override def encodeValue(x: InputValue.ObjectValue, out: JsonWriter): Unit                        =
        inputValueCodec.encodeValue(x, out)
      override def nullValue: InputValue.ObjectValue                                                    =
        null.asInstanceOf[InputValue.ObjectValue]
    }

  private implicit val stringListCodec: JsonValueCodec[Map[String, List[String]]] = JsonCodecMaker.make
}
