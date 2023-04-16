package caliban.interop.tapir

import caliban.ResponseValue.StreamValue

import java.nio.charset.StandardCharsets
import caliban._
import caliban.execution.QueryExecution
import caliban.interop.tapir.ws.Protocol
import caliban.uploads.{ FileMeta, GraphQLUploadRequest, Uploads }
import sttp.capabilities.{ Streams, WebSockets }
import sttp.capabilities.zio.ZioStreams
import sttp.capabilities.zio.ZioStreams.Pipe
import sttp.model.{ headers => _, _ }
import sttp.monad.MonadError
import sttp.tapir.Codec.{ mediaType, JsonCodec }
import sttp.tapir._
import sttp.tapir.model.{ ServerRequest, UnsupportedWebSocketFrameException }
import sttp.tapir.server.ServerEndpoint
import sttp.ws.WebSocketFrame
import zio._
import zio.stream.{ ZSink, ZStream }

import scala.concurrent.Future
import scala.util.Try

object TapirAdapter {

  type CalibanPipe   = Pipe[GraphQLWSInput, Either[GraphQLWSClose, GraphQLWSOutput]]
  type UploadRequest = (Seq[Part[Array[Byte]]], ServerRequest)
  type ZioWebSockets = ZioStreams with WebSockets

  object CalibanBody {
    type Single = Left[ResponseValue, Nothing]
    type Stream = Right[Nothing, ZStream[Any, Throwable, Byte]]

  }
  type CalibanBody     = Either[ResponseValue, ZStream[Any, Throwable, Byte]]
  type CalibanResponse = (MediaType, CalibanBody)

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

  private def outputBody(implicit codec: JsonCodec[ResponseValue]) = oneOf[CalibanBody](
    oneOfVariantValueMatcher[CalibanBody.Single](customCodecJsonBody[ResponseValue].map(Left(_)) { case Left(value) =>
      value
    }) { case Left(_) => true },
    oneOfVariantValueMatcher[CalibanBody.Stream](
      streamTextBody(ZioStreams)(CodecFormat.Json(), Some(StandardCharsets.UTF_8)).toEndpointIO
        .map(Right(_)) { case Right(value) => value }
    ) { case Right(_) => true }
  )

  def makeHttpEndpoints[E](implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[ResponseValue]
  ): List[
    PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse, ZioStreams]
  ] = {
    def queryFromQueryParams(queryParams: QueryParams): DecodeResult[GraphQLRequest] =
      for {
        req <- requestCodec.decode(s"""{"query":"","variables":${queryParams
                 .get("variables")
                 .getOrElse("null")},"extensions":${queryParams
                 .get("extensions")
                 .getOrElse("null")}}""")

      } yield req.copy(query = queryParams.get("query"), operationName = queryParams.get("operationName"))

    val postEndpoint: PublicEndpoint[
      (GraphQLRequest, ServerRequest),
      TapirResponse,
      CalibanResponse,
      ZioStreams
    ] =
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
        .out(header[MediaType](HeaderNames.ContentType))
        .out(outputBody)
        .errorOut(errorBody)

    val getEndpoint: PublicEndpoint[
      (GraphQLRequest, ServerRequest),
      TapirResponse,
      CalibanResponse,
      ZioStreams
    ] =
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
        .out(header[MediaType](HeaderNames.ContentType))
        .out(outputBody)
        .errorOut(errorBody)

    postEndpoint :: getEndpoint :: Nil
  }

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[ResponseValue]
  ): List[ServerEndpoint[ZioStreams, RIO[R, *]]] = {
    def logic(
      request: (GraphQLRequest, ServerRequest)
    ): RIO[R, Either[TapirResponse, CalibanResponse]] = {
      val (graphQLRequest, serverRequest) = request

      requestInterceptor(serverRequest)(
        interpreter
          .executeRequest(
            graphQLRequest,
            skipValidation = skipValidation,
            enableIntrospection = enableIntrospection,
            queryExecution
          )
      ).map(buildHttpResponse).either
    }

    makeHttpEndpoints.map(_.serverLogic(logic))
  }

  def makeHttpUploadEndpoint(implicit responseCodec: JsonCodec[ResponseValue]): PublicEndpoint[
    UploadRequest,
    TapirResponse,
    CalibanResponse,
    ZioStreams
  ] =
    endpoint.post
      .in(multipartBody)
      .in(extractFromRequest(identity))
      .out(header[MediaType](HeaderNames.ContentType))
      .out(outputBody)
      .errorOut(errorBody)

  def makeHttpUploadService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]],
    responseValueCodec: JsonCodec[ResponseValue]
  ): ServerEndpoint[ZioStreams, RIO[R, *]] = {
    def logic(
      request: UploadRequest
    ): RIO[R, Either[TapirResponse, CalibanResponse]] = {
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

      requestInterceptor(serverRequest)(io)
        .map(buildHttpResponse)
        .either
    }

    makeHttpUploadEndpoint.serverLogic(logic)
  }

  private def buildHttpResponse[E](response: GraphQLResponse[E])(implicit
    responseCodec: JsonCodec[ResponseValue]
  ) =
    response match {
      case resp @ GraphQLResponse(StreamValue(stream), _, _, _) =>
        (
          MediaType.MultipartMixed.copy(otherParameters = DeferMultipart.DeferHeaderParams),
          encodeMultipartMixedResponse(resp, stream)
        )
      case response                                             =>
        (MediaType.ApplicationJson, encodeSingleResponse(response))
    }

  private object DeferMultipart {
    private val Newline        = "\r\n"
    private val ContentType    = "Content-Type: application/json; charset=utf-8"
    private val SubHeader      = s"$Newline$ContentType$Newline$Newline"
    private val Boundary       = "---"
    private val BoundaryHeader = "-"
    private val DeferSpec      = "20220824"

    val InnerBoundary = s"$Newline$Boundary$SubHeader"
    val EndBoundary   = s"$Newline-----$Newline"

    val DeferHeaderParams = Map("boundary" -> BoundaryHeader, "deferSpec" -> DeferSpec)
  }

  private def encodeMultipartMixedResponse[E](
    resp: GraphQLResponse[E],
    stream: ZStream[Any, Throwable, ResponseValue]
  )(implicit responseCodec: JsonCodec[ResponseValue]): CalibanBody = {
    import DeferMultipart._

    Right(
      ZStream
        .unwrapScoped(
          for {
            initialAndSubsequent <- stream.peel(ZSink.head[ResponseValue])
            (initial, subsequent) = initialAndSubsequent
            payload               = ZStream.fromIterable(
                                      initial.map(value => resp.copy(data = value).toResponseValue)
                                    ) ++ subsequent
          } yield payload
            .map(responseCodec.encode)
            .intersperse(InnerBoundary, InnerBoundary, EndBoundary)
        )
        .mapConcat(_.getBytes(StandardCharsets.UTF_8))
    )

  }

  private def encodeSingleResponse[E](response: GraphQLResponse[E]) =
    Left(response.toResponseValue)

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

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty[R, E]
  )(implicit
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): ServerEndpoint[ZioWebSockets, RIO[R, *]] =
    makeWebSocketEndpoint.serverLogic[RIO[R, *]] { case (serverRequest, protocol) =>
      requestInterceptor(serverRequest)(
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
      ).catchAll(ZIO.left(_))
    }

  def convertHttpEndpointToFuture[R](
    endpoint: ServerEndpoint[ZioStreams, RIO[R, *]]
  )(implicit runtime: Runtime[R]): ServerEndpoint[ZioStreams, Future] =
    ServerEndpoint[
      endpoint.SECURITY_INPUT,
      endpoint.PRINCIPAL,
      endpoint.INPUT,
      endpoint.ERROR_OUTPUT,
      endpoint.OUTPUT,
      ZioStreams,
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
