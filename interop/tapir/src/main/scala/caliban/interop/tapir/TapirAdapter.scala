package caliban.interop.tapir

import java.nio.charset.StandardCharsets
import caliban._
import caliban.execution.QueryExecution
import caliban.interop.tapir.TapirAdapter._
import caliban.interop.tapir.ws.Protocol
import caliban.uploads.{ FileMeta, GraphQLUploadRequest, Uploads }
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.capabilities.zio.ZioStreams.Pipe
import sttp.model.{ headers => _, _ }
import sttp.monad.MonadError
import sttp.tapir.Codec.JsonCodec
import sttp.tapir._
import sttp.tapir.model.{ ServerRequest, UnsupportedWebSocketFrameException }
import sttp.tapir.server.ServerEndpoint
import sttp.ws.WebSocketFrame
import zio._

import scala.concurrent.Future
import scala.util.Try

trait HttpAdapter[-R, E] { self =>

  protected val endpoints: List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any]]

  def serverEndpoints[R1 <: R]: List[ServerEndpoint[Any, RIO[R1, *]]] = {
    def logic(request: (GraphQLRequest, ServerRequest)): RIO[R1, Either[TapirResponse, GraphQLResponse[E]]] = {
      val (graphQLRequest, serverRequest) = request
      executeRequest(graphQLRequest, serverRequest).either
    }
    endpoints.map(_.serverLogic(logic))
  }

  protected def executeRequest(
    graphQLRequest: GraphQLRequest,
    serverRequest: ServerRequest
  ): ZIO[R, TapirResponse, GraphQLResponse[E]]

  def intercept[R1](requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R]): HttpAdapter[R1, E] =
    new HttpAdapter[R1, E] {
      protected val endpoints
        : List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any]] =
        self.endpoints

      protected def executeRequest(
        graphQLRequest: GraphQLRequest,
        serverRequest: ServerRequest
      ): ZIO[R1, TapirResponse, GraphQLResponse[E]] =
        self
          .executeRequest(graphQLRequest, serverRequest)
          .provideSome[R1](ZLayer.succeed(serverRequest), requestInterceptor)
    }
}

object HttpAdapter {
  def apply[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit requestCodec: JsonCodec[GraphQLRequest], responseCodec: JsonCodec[GraphQLResponse[E]]): HttpAdapter[R, E] =
    new HttpAdapter[R, E] {
      val endpoints: List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any]] =
        makeHttpEndpoints

      def executeRequest(
        graphQLRequest: GraphQLRequest,
        serverRequest: ServerRequest
      ): ZIO[R, TapirResponse, GraphQLResponse[E]] =
        interpreter
          .executeRequest(
            graphQLRequest,
            skipValidation = skipValidation,
            enableIntrospection = enableIntrospection,
            queryExecution
          )
    }
}

object TapirAdapter {

  type CalibanPipe   = Pipe[GraphQLWSInput, Either[GraphQLWSClose, GraphQLWSOutput]]
  type UploadRequest = (Seq[Part[Array[Byte]]], ServerRequest)
  type ZioWebSockets = ZioStreams with WebSockets

  case class TapirResponse(
    code: StatusCode,
    body: String = "",
    headers: List[Header] = Nil
  ) {
    def withBody(body: String): TapirResponse =
      copy(body = body)

    def withHeader(key: String, value: String): TapirResponse =
      copy(headers = Header(key, value) :: headers)

    def withHeaders(_headers: List[Header]): TapirResponse =
      copy(headers = _headers ++ headers)
  }

  object TapirResponse {

    val ok                             = TapirResponse(StatusCode.Ok)
    def status(statusCode: StatusCode) = TapirResponse(statusCode)
  }

  private val responseMapping = Mapping.from[(StatusCode, String, List[Header]), TapirResponse](
    (TapirResponse.apply _).tupled
  )(resp => (resp.code, resp.body, resp.headers))

  private val errorBody = statusCode.and(stringBody).and(headers).map(responseMapping)

  def makeHttpEndpoints[E](implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): List[PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any]] = {
    def queryFromQueryParams(queryParams: QueryParams): DecodeResult[GraphQLRequest] =
      for {
        req <- requestCodec.decode(s"""{"query":"","variables":${queryParams
                 .get("variables")
                 .getOrElse("null")},"extensions":${queryParams
                 .get("extensions")
                 .getOrElse("null")}}""")

      } yield req.copy(query = queryParams.get("query"), operationName = queryParams.get("operationName"))

    val postEndpoint: PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any] =
      endpoint.post
        .in(
          (headers and stringBody and queryParams).mapDecode { case (headers, body, params) =>
            val getRequest =
              if (params.get("query").isDefined)
                queryFromQueryParams(params)
              else if (
                headers.exists(header =>
                  header.name.equalsIgnoreCase(HeaderNames.ContentType) &&
                    MediaType
                      .parse(header.value)
                      .exists(mediaType => mediaType.mainType == "application" && mediaType.subType == "graphql")
                )
              )
                DecodeResult.Value(GraphQLRequest(query = Some(body)))
              else requestCodec.decode(body)

            getRequest.map(request => headers.find(isFtv1Header).fold(request)(_ => request.withFederatedTracing))
          }(request => (Nil, requestCodec.encode(request), QueryParams()))
        )
        .in(extractFromRequest(identity))
        .out(customCodecJsonBody[GraphQLResponse[E]])
        .errorOut(errorBody)

    val getEndpoint: PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, GraphQLResponse[E], Any] =
      endpoint.get
        .in(
          queryParams.mapDecode(queryFromQueryParams)(request =>
            QueryParams.fromMap(
              Map(
                "query"         -> request.query.getOrElse(""),
                "operationName" -> request.operationName.getOrElse(""),
                "variables"     -> request.variables
                  .map(_.map { case (k, v) => s""""$k":${v.toInputString}""" }.mkString("{", ",", "}"))
                  .getOrElse(""),
                "extensions"    -> request.extensions
                  .map(_.map { case (k, v) => s""""$k":${v.toInputString}""" }.mkString("{", ",", "}"))
                  .getOrElse("")
              ).filter { case (_, v) => v.nonEmpty }
            )
          )
        )
        .in(extractFromRequest(identity))
        .out(customCodecJsonBody[GraphQLResponse[E]])
        .errorOut(errorBody)

    postEndpoint :: getEndpoint :: Nil
  }

  def makeHttpService[R1, R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R] = ZLayer.empty
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): List[ServerEndpoint[Any, RIO[R1, *]]] = {
    def logic(request: (GraphQLRequest, ServerRequest)): RIO[R1, Either[TapirResponse, GraphQLResponse[E]]] = {
      val (graphQLRequest, serverRequest) = request

      interpreter
        .executeRequest(
          graphQLRequest,
          skipValidation = skipValidation,
          enableIntrospection = enableIntrospection,
          queryExecution
        )
        .provideSome[R1](ZLayer.succeed(serverRequest), requestInterceptor)
        .either
    }

    makeHttpEndpoints.map(_.serverLogic(logic))
  }

  def makeHttpUploadEndpoint[E](implicit
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): PublicEndpoint[UploadRequest, TapirResponse, GraphQLResponse[E], Any] =
    endpoint.post
      .in(multipartBody)
      .in(extractFromRequest(identity))
      .out(customCodecJsonBody[GraphQLResponse[E]])
      .errorOut(errorBody)

  def makeHttpUploadService[R1, R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R] = ZLayer.empty
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): ServerEndpoint[Any, RIO[R1, *]] = {
    def logic(request: UploadRequest): RIO[R1, Either[TapirResponse, GraphQLResponse[E]]] = {
      val (parts, serverRequest) = request
      val partsMap               = parts.map(part => part.name -> part).toMap

      val io =
        for {
          rawOperations <- ZIO.fromOption(partsMap.get("operations")) orElseFail TapirResponse(StatusCode.BadRequest)
          request       <- requestCodec.rawDecode(new String(rawOperations.body, StandardCharsets.UTF_8)) match {
                             case _: DecodeResult.Failure => ZIO.fail(TapirResponse(StatusCode.BadRequest))
                             case DecodeResult.Value(v)   => ZIO.succeed(v)
                           }
          rawMap        <- ZIO.fromOption(partsMap.get("map")) orElseFail TapirResponse(StatusCode.BadRequest)
          map           <- mapCodec.rawDecode(new String(rawMap.body, StandardCharsets.UTF_8)) match {
                             case _: DecodeResult.Failure => ZIO.fail(TapirResponse(StatusCode.BadRequest))
                             case DecodeResult.Value(v)   => ZIO.succeed(v)
                           }
          filePaths      = map.map { case (key, value) => (key, value.map(parsePath).toList) }.toList
                             .flatMap(kv => kv._2.map(kv._1 -> _))
          handler        = Uploads.handler(handle =>
                             ZIO
                               .succeed(partsMap.get(handle))
                               .some
                               .flatMap(fp =>
                                 Random.nextUUID.asSomeError
                                   .map(uuid =>
                                     FileMeta(
                                       uuid.toString,
                                       fp.body,
                                       fp.contentType,
                                       fp.fileName.getOrElse(""),
                                       fp.body.length
                                     )
                                   )
                               )
                               .unsome
                           )
          uploadQuery    = GraphQLUploadRequest(request, filePaths, handler)
          query          = serverRequest.headers
                             .find(isFtv1Header)
                             .fold(uploadQuery.remap)(_ => uploadQuery.remap.withFederatedTracing)
          response      <- interpreter
                             .executeRequest(
                               query,
                               skipValidation = skipValidation,
                               enableIntrospection = enableIntrospection,
                               queryExecution
                             )
                             .provideSomeLayer[R](ZLayer(uploadQuery.fileHandle))
        } yield response

      io.provideSome[R1](ZLayer.succeed(serverRequest), requestInterceptor).either
    }

    makeHttpUploadEndpoint.serverLogic(logic)
  }

  /**
   * A codec which expects only text and close frames (all other frames cause a decoding error). Close frames correspond to a `Left`,
   * while text frames are handled using the given `stringCodec` and wrapped with a `Right`
   * @return
   */
  private implicit def textOrCloseWebSocketFrameEither[A, CF <: CodecFormat](implicit
    stringCodec: Codec[String, A, CF]
  ): Codec[WebSocketFrame, Either[GraphQLWSClose, A], CF] =
    Codec
      .id[WebSocketFrame, CF](stringCodec.format, Schema.string)
      .mapDecode {
        case WebSocketFrame.Text(s, _, _)       => stringCodec.decode(s).map(Right(_))
        case WebSocketFrame.Close(code, reason) => DecodeResult.Value(Left(GraphQLWSClose(code, reason)))
        case f                                  => DecodeResult.Error(f.toString, new UnsupportedWebSocketFrameException(f))
      } {
        case Left(value)  => WebSocketFrame.Close(value.code, value.reason)
        case Right(value) => WebSocketFrame.text(stringCodec.encode(value))
      }

  def makeWebSocketEndpoint(implicit
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): PublicEndpoint[(ServerRequest, String), TapirResponse, (String, CalibanPipe), ZioStreams with WebSockets] = {
    val protocolHeader: EndpointIO.Header[String] = header[String]("sec-websocket-protocol")
    endpoint
      .in(extractFromRequest(identity))
      .in(protocolHeader)
      .out(protocolHeader)
      .out(
        webSocketBody[GraphQLWSInput, CodecFormat.Json, Either[GraphQLWSClose, GraphQLWSOutput], CodecFormat.Json](
          ZioStreams
        )
      )
      .errorOut(errorBody)
  }

  def makeWebSocketService[R1, R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: ZLayer[R1 & ServerRequest, TapirResponse, R] = ZLayer.empty,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty[R, E]
  )(implicit
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): ServerEndpoint[ZioWebSockets, RIO[R1, *]] =
    makeWebSocketEndpoint.serverLogic[RIO[R1, *]] { case (serverRequest, protocol) =>
      Protocol
        .fromName(protocol)
        .make(
          interpreter,
          skipValidation,
          enableIntrospection,
          keepAliveTime,
          queryExecution,
          webSocketHooks
        )
        .map(res => Right((protocol, res)))
        .provideSome[R1](ZLayer.succeed(serverRequest), requestInterceptor)
        .catchAll(ZIO.left(_))
    }

  def convertHttpEndpointToFuture[R](
    endpoint: ServerEndpoint[Any, RIO[R, *]]
  )(implicit runtime: Runtime[R]): ServerEndpoint[Any, Future] =
    ServerEndpoint[
      endpoint.SECURITY_INPUT,
      endpoint.PRINCIPAL,
      endpoint.INPUT,
      endpoint.ERROR_OUTPUT,
      endpoint.OUTPUT,
      Any,
      Future
    ](
      endpoint.endpoint,
      _ =>
        a => Unsafe.unsafe(implicit u => runtime.unsafe.runToFuture(endpoint.securityLogic(zioMonadError)(a)).future),
      _ =>
        u =>
          req => Unsafe.unsafe(implicit un => runtime.unsafe.runToFuture(endpoint.logic(zioMonadError)(u)(req)).future)
    )

  def zioMonadError[R]: MonadError[RIO[R, *]] = new MonadError[RIO[R, *]] {
    override def unit[T](t: T): RIO[R, T]                                                                            = ZIO.succeed(t)
    override def map[T, T2](fa: RIO[R, T])(f: T => T2): RIO[R, T2]                                                   = fa.map(f)
    override def flatMap[T, T2](fa: RIO[R, T])(f: T => RIO[R, T2]): RIO[R, T2]                                       = fa.flatMap(f)
    override def error[T](t: Throwable): RIO[R, T]                                                                   = ZIO.fail(t)
    override protected def handleWrappedError[T](rt: RIO[R, T])(h: PartialFunction[Throwable, RIO[R, T]]): RIO[R, T] =
      rt.catchSome(h)
    override def eval[T](t: => T): RIO[R, T]                                                                         = ZIO.attempt(t)
    override def suspend[T](t: => RIO[R, T]): RIO[R, T]                                                              = ZIO.suspend(t)
    override def flatten[T](ffa: RIO[R, RIO[R, T]]): RIO[R, T]                                                       = ffa.flatten
    override def ensure[T](f: RIO[R, T], e: => RIO[R, Unit]): RIO[R, T]                                              = f.ensuring(e.ignore)
  }

  private def parsePath(path: String): List[Either[String, Int]] =
    path.split('.').map(c => Try(c.toInt).toEither.left.map(_ => c)).toList

  private def isFtv1Header(r: Header): Boolean =
    r.name == GraphQLRequest.`apollo-federation-include-trace` && r.value == GraphQLRequest.ftv1
}
