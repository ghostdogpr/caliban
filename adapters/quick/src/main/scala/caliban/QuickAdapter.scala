package caliban

import caliban.Configurator.ExecutionConfiguration
import caliban.HttpUtils._
import caliban.ResponseValue.StreamValue
import caliban.interop.jsoniter.ValueJsoniter
import caliban.wrappers.Caching
import com.github.plokhotnyuk.jsoniter_scala.core._
import zio._
import zio.http._
import zio.stream.ZStream

import java.nio.charset.StandardCharsets.UTF_8
import scala.util.control.NonFatal

final class QuickAdapter[-R, E](interpreter: GraphQLInterpreter[R, E]) {
  import QuickAdapter._
  import caliban.interop.jsoniter.ValueJsoniter._

  def runServer(port: Int, apiPath: Path, graphiqlPath: Option[Path] = None): RIO[R, Nothing] =
    runServer(Server.Config.default.port(port), apiPath, graphiqlPath)

  def runServer(config: Server.Config, apiPath: Path, graphiqlPath: Option[Path]): RIO[R, Nothing] =
    Server.serve[R](app(apiPath, graphiqlPath)).provideSomeLayer[R](Server.defaultWith(_ => config))

  lazy val handler: RequestHandler[R, Response] =
    Handler.fromFunctionZIO[Request] { req =>
      transformRequest(req)
        .flatMap(executeRequest(req.method, _))
        .map(transformResponse(req, _))
    }

  private def app(apiPath: Path, graphiqlPath: Option[Path]): App[R] = {
    val apiApp = Http.collectHandler[Request] { case _ -> path if path == apiPath => handler }
    graphiqlPath match {
      case None         => apiApp
      case Some(uiPath) =>
        val uiHandler = GraphiQLAdapter.handler(apiPath, uiPath)
        apiApp ++ Http.collectHandler[Request] { case _ -> path if path == uiPath => uiHandler }
    }
  }

  def configure(config: ExecutionConfiguration): QuickAdapter[R, E] =
    new QuickAdapter[R, E](
      interpreter.wrapExecutionWith[R, E](Configurator.setWith(config)(_))
    )

  def configure[R1](configurator: QuickAdapter.Configurator[R1]): QuickAdapter[R & R1, E] =
    new QuickAdapter[R & R1, E](
      interpreter.wrapExecutionWith[R & R1, E](exec => ZIO.scoped[R1 & R](configurator *> exec))
    )

  private def badRequest(msg: String) = Response(Status.BadRequest, body = Body.fromString(msg))

  private def transformRequest(httpReq: Request): IO[Response, GraphQLRequest] = {
    val queryParams = httpReq.url.queryParams

    def extractFields(key: String): Either[Response, Option[Map[String, InputValue]]] =
      try Right(
        queryParams
          .get(key)
          .flatMap(_.headOption)
          .map(readFromString[InputValue.ObjectValue](_).fields)
      )
      catch { case NonFatal(_) => Left(badRequest(s"Invalid $key query param")) }

    def fromQueryParams: Either[Response, GraphQLRequest] =
      for {
        vars <- extractFields("variables")
        exts <- extractFields("extensions")
      } yield GraphQLRequest(
        query = queryParams.get("query").flatMap(_.headOption),
        operationName = queryParams.get("operationName").flatMap(_.headOption),
        variables = vars,
        extensions = exts
      )

    def isApplicationGql =
      httpReq.headers.get("content-type").fold(false)(_.startsWith("application/graphql"))

    val resp = httpReq.method match {
      case Method.GET  => ZIO.fromEither(fromQueryParams)
      case Method.POST =>
        val postReq =
          if (queryParams.get("query").isDefined)
            ZIO.fromEither(fromQueryParams)
          else if (isApplicationGql)
            httpReq.body.asString.mapBoth(_ => BodyDecodeErrorResponse, b => GraphQLRequest(Some(b)))
          else
            httpReq.body.asArray
              .flatMap(arr => ZIO.attempt(readFromArray[GraphQLRequest](arr)))
              .orElseFail(BodyDecodeErrorResponse)

        httpReq.headers
          .get(GraphQLRequest.`apollo-federation-include-trace`)
          .collect { case GraphQLRequest.ftv1 => postReq.map(_.withFederatedTracing) }
          .getOrElse(postReq)
      case _           => ZIO.fail(Response.status(Status.NotFound))
    }

    resp.tap(r => if (r.isEmpty) ZIO.fail(badRequest("No GraphQL query to execute")) else ZIO.unit)
  }

  private def executeRequest(method: Method, req: GraphQLRequest): ZIO[R, Response, GraphQLResponse[E]] = {
    val calibanMethod = if (method == Method.GET) HttpRequestMethod.GET else HttpRequestMethod.POST
    HttpRequestMethod.setWith(calibanMethod)(interpreter.executeRequest(req))
  }

  private def responseHeaders(headers: Headers, cacheDirective: Option[String]): Headers =
    cacheDirective.fold(headers)(headers.addHeader(Header.CacheControl.name, _))

  private def transformResponse(httpReq: Request, resp: GraphQLResponse[E]): Response = {

    def acceptsGqlJson: Boolean =
      httpReq.header(Header.Accept).fold(false) { h =>
        h.mimeTypes.exists(mt =>
          mt.mediaType.subType == "graphql-response+json" && mt.mediaType.mainType == "application"
        )
      }

    resp match {
      case resp @ GraphQLResponse(StreamValue(stream), _, _, _) =>
        Response(
          Status.Ok,
          headers = responseHeaders(ContentTypeMultipart, None),
          body = Body.fromStream(encodeMultipartMixedResponse(resp, stream))
        )
      case resp if acceptsGqlJson                               =>
        val cacheDirective = HttpUtils.computeCacheDirective(resp.extensions)
        Response(
          status = resp.errors.collectFirst { case _: CalibanError.ParsingError | _: CalibanError.ValidationError =>
            Status.BadRequest
          }.getOrElse(Status.Ok),
          headers = responseHeaders(ContentTypeGql, cacheDirective),
          body = encodeSingleResponse(
            resp,
            keepDataOnErrors = false,
            excludeExtensions = cacheDirective.map(_ => Set(Caching.DirectiveName))
          )
        )
      case resp                                                 =>
        val cacheDirective = HttpUtils.computeCacheDirective(resp.extensions)
        Response(
          status = resp.errors.collectFirst { case HttpRequestMethod.MutationOverGetError => Status.BadRequest }
            .getOrElse(Status.Ok),
          headers = responseHeaders(ContentTypeJson, cacheDirective),
          body = encodeSingleResponse(
            resp,
            keepDataOnErrors = true,
            excludeExtensions = cacheDirective.map(_ => Set(Caching.DirectiveName))
          )
        )
    }
  }

  private def encodeSingleResponse(
    resp: GraphQLResponse[E],
    keepDataOnErrors: Boolean,
    excludeExtensions: Option[Set[String]]
  ): Body =
    Body.fromChunk(Chunk.fromArray(writeToArray(resp.toResponseValue(keepDataOnErrors, excludeExtensions))))

  private def encodeMultipartMixedResponse(
    resp: GraphQLResponse[E],
    stream: ZStream[Any, Throwable, ResponseValue]
  ): ZStream[Any, Throwable, Byte] = {
    import HttpUtils.DeferMultipart._
    val pipeline = createPipeline(resp)

    stream
      .via(pipeline)
      .map(writeToArray(_))
      .intersperse(InnerBoundary.getBytes(UTF_8), InnerBoundary.getBytes(UTF_8), EndBoundary.getBytes(UTF_8))
      .mapConcatChunk(Chunk.fromArray)
  }
}

object QuickAdapter {
  type Configurator[-R] = URIO[R & Scope, Unit]

  def apply[R, E](interpreter: GraphQLInterpreter[R, E]): QuickAdapter[R, E] =
    new QuickAdapter(interpreter)

  /**
   * Runs the server using the default zio-http server configuration on the specified port.
   * This is meant as a convenience method for getting started quickly
   */
  def runServer[R](port: Int, api: Path, graphiql: Option[Path] = None)(implicit
    tag: Tag[R]
  ): RIO[GraphQL[R] & R, Nothing] =
    runServer(Server.Config.default.port(port), api, graphiql)

  def runServer[R](config: Server.Config, api: Path, graphiql: Option[Path])(implicit
    tag: Tag[R]
  ): RIO[GraphQL[R] & R, Nothing] =
    ZIO
      .serviceWithZIO[QuickAdapter[R, CalibanError]](_.runServer(config, api, graphiql))
      .provideSomeLayer[GraphQL[R] & R](default)

  def handler[R](implicit tag: Tag[R]): URIO[QuickAdapter[R, CalibanError], RequestHandler[R, Response]] =
    ZIO.serviceWith(_.handler)

  def default[R](implicit
    tag: Tag[R]
  ): ZLayer[GraphQL[R], CalibanError.ValidationError, QuickAdapter[R, CalibanError]] = ZLayer.fromZIO(
    ZIO.serviceWithZIO(_.interpreter.map(new QuickAdapter(_)))
  )

  def live[R](implicit
    tag: Tag[R]
  ): ZLayer[GraphQL[R] & ExecutionConfiguration, CalibanError.ValidationError, QuickAdapter[R, CalibanError]] =
    ZLayer.fromZIO(
      for {
        config      <- ZIO.service[ExecutionConfiguration]
        interpreter <- ZIO.serviceWithZIO[GraphQL[R]](_.interpreter)
      } yield QuickAdapter(interpreter).configure(config)
    )

  private val ContentTypeJson =
    Headers(Header.ContentType(MediaType.application.json))

  private val ContentTypeGql =
    Headers(Header.ContentType(MediaType("application", "graphql-response+json")))

  private val ContentTypeMultipart =
    Headers(Header.ContentType(MediaType.multipart.mixed.copy(parameters = DeferMultipart.DeferHeaderParams)))

  private val BodyDecodeErrorResponse =
    Response(Status.BadRequest, body = Body.fromString("Failed to decode json body"))

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
}
