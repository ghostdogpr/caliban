package caliban.interop.tapir

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.StringValue
import caliban._
import caliban.execution.{ DeferredGraphQLResponse, QueryExecution }
import caliban.uploads.{ FileMeta, GraphQLUploadRequest, Uploads }
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.capabilities.zio.ZioStreams.Pipe
import sttp.model.{ headers => _, _ }
import sttp.monad.MonadError
import sttp.tapir.Codec.{ mediaType, JsonCodec }
import sttp.tapir._
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import zio._
import zio.clock.Clock
import zio.duration.Duration
import zio.random.Random
import zio.stream._

import java.nio.charset.StandardCharsets
import scala.concurrent.Future
import scala.util.Try

object TapirAdapter {

  type CalibanPipe   = Pipe[GraphQLWSInput, GraphQLWSOutput]
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

  def makeHttpEndpoints[R, E](implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[ResponseValue]
  ): List[
    PublicEndpoint[(GraphQLRequest, ServerRequest), TapirResponse, (MediaType, ZioStreams.BinaryStream), ZioStreams]
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
      (MediaType, ZioStreams.BinaryStream),
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
        .out(streamTextBody(ZioStreams)(CodecFormat.Json(), Some(StandardCharsets.UTF_8)))
        .errorOut(errorBody)

    val getEndpoint: PublicEndpoint[
      (GraphQLRequest, ServerRequest),
      TapirResponse,
      (MediaType, ZioStreams.BinaryStream),
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
        .out(streamTextBody(ZioStreams)(CodecFormat.Json(), Some(StandardCharsets.UTF_8)))
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
    ): RIO[R, Either[TapirResponse, (MediaType, ZioStreams.BinaryStream)]] = {
      val (graphQLRequest, serverRequest) = request

      requestInterceptor(serverRequest)(
        interpreter
          .executeRequest(
            graphQLRequest,
            skipValidation = skipValidation,
            enableIntrospection = enableIntrospection,
            queryExecution
          )
      ).map(buildResponse).either
    }

    makeHttpEndpoints.map(_.serverLogic(logic))
  }

  private[caliban] def buildResponse[E](response: GraphQLResponse[E])(implicit
    responseCodec: JsonCodec[ResponseValue]
  ) =
    response match {
      case DeferredGraphQLResponse(head, tail) =>
        val Newline     = "\r\n"
        val ContentType = "Content-Type: application/json; charset=utf-8"
        val SubHeader   = s"$Newline$ContentType$Newline$Newline"
        val Boundary    = "---"
        val theBoundary = s"$Newline$Boundary$SubHeader"
        val endBoundary = s"$Newline-----$Newline"

        (
          MediaType.MultipartMixed.copy(otherParameters = Map("boundary" -> "-")),
          ((ZStream.succeed(head.toResponseValue) ++ tail)
            .map(responseCodec.encode)
            .intersperse(theBoundary, theBoundary, endBoundary))
            .mapConcat(_.getBytes(StandardCharsets.UTF_8))
        )

      case response =>
        val encodedResponse =
          ZStream.fromIterable(responseCodec.encode(response.toResponseValue).getBytes(StandardCharsets.UTF_8))
        (MediaType.ApplicationJson, encodedResponse)
    }

  def makeHttpUploadEndpoint[R, E](implicit
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]],
    responseCodec: JsonCodec[ResponseValue]
  ): PublicEndpoint[
    (Seq[Part[Array[Byte]]], ServerRequest),
    TapirResponse,
    (MediaType, ZioStreams.BinaryStream),
    ZioStreams
  ] =
    endpoint.post
      .in(multipartBody)
      .in(extractFromRequest(identity))
      .out(header[MediaType](HeaderNames.ContentType))
      .out(streamTextBody(ZioStreams)(CodecFormat.Json(), Some(StandardCharsets.UTF_8)))
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
    responseCodec: JsonCodec[ResponseValue]
  ): ServerEndpoint[ZioStreams, RIO[R with Random, *]] = {
    def logic(
      request: UploadRequest
    ): RIO[R with Random, Either[TapirResponse, (sttp.model.MediaType, ZioStreams.BinaryStream)]] = {
      val (parts, serverRequest) = request
      val partsMap               = parts.map(part => part.name -> part).toMap

      val io =
        for {
          rawOperations <- ZIO.fromOption(partsMap.get("operations")) orElseFail TapirResponse(StatusCode.BadRequest)
          request       <- requestCodec.rawDecode(new String(rawOperations.body, "utf-8")) match {
                             case _: DecodeResult.Failure => ZIO.fail(TapirResponse(StatusCode.BadRequest))
                             case DecodeResult.Value(v)   => UIO(v)
                           }
          rawMap        <- ZIO.fromOption(partsMap.get("map")) orElseFail TapirResponse(StatusCode.BadRequest)
          map           <- mapCodec.rawDecode(new String(rawMap.body, "utf-8")) match {
                             case _: DecodeResult.Failure => ZIO.fail(TapirResponse(StatusCode.BadRequest))
                             case DecodeResult.Value(v)   => UIO(v)
                           }
          filePaths      = map.map { case (key, value) => (key, value.map(parsePath).toList) }.toList
                             .flatMap(kv => kv._2.map(kv._1 -> _))
          random        <- ZIO.service[Random.Service]
          handler        = Uploads.handler(handle =>
                             UIO(partsMap.get(handle)).some
                               .flatMap(fp =>
                                 random.nextUUID.asSomeError
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
                               .optional
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
                             .provideSomeLayer[R with Random](uploadQuery.fileHandle.toLayerMany)
        } yield response

      requestInterceptor(serverRequest)(io)
        .map(buildResponse)
        .either
    }

    makeHttpUploadEndpoint.serverLogic(logic)
  }

  def makeWebSocketEndpoint[R, E](implicit
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): PublicEndpoint[ServerRequest, TapirResponse, CalibanPipe, ZioStreams with WebSockets] = {
    val protocolHeader = Header("Sec-WebSocket-Protocol", "graphql-ws")
    endpoint
      .in(header(protocolHeader))
      .in(extractFromRequest(identity))
      .out(header(protocolHeader))
      .out(webSocketBody[GraphQLWSInput, CodecFormat.Json, GraphQLWSOutput, CodecFormat.Json](ZioStreams))
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
  ): ServerEndpoint[ZioWebSockets, RIO[R, *]] = {

    val io: URIO[R, Either[Nothing, CalibanPipe]] =
      for {
        env           <- RIO.environment[R]
        subscriptions <- Ref.make(Map.empty[String, Promise[Any, Unit]])
        output        <- Queue.unbounded[GraphQLWSOutput]
        pipe          <- UIO.right[CalibanPipe] { input =>
                           ZStream
                             .bracket(
                               input.collectM {
                                 case GraphQLWSInput("connection_init", id, payload) =>
                                   val before   = ZIO.whenCase((webSocketHooks.beforeInit, payload)) {
                                     case (Some(beforeInit), Some(payload)) =>
                                       beforeInit(payload).catchAll(e => output.offer(makeError(id, e)))
                                   }
                                   val response = output.offer(connectionAck)
                                   val ka       = keepAlive(keepAliveTime).mapM(output.offer).runDrain.fork
                                   val after    = ZIO.whenCase(webSocketHooks.afterInit) { case Some(afterInit) =>
                                     afterInit.catchAll(e => output.offer(makeError(id, e)))
                                   }

                                   before *> response *> ka *> after
                                 case GraphQLWSInput("start", id, payload)           =>
                                   val request = payload.collect { case InputValue.ObjectValue(fields) =>
                                     val query         = fields.get("query").collect { case StringValue(v) => v }
                                     val operationName = fields.get("operationName").collect { case StringValue(v) => v }
                                     val variables     = fields.get("variables").collect { case InputValue.ObjectValue(v) => v }
                                     val extensions    = fields.get("extensions").collect { case InputValue.ObjectValue(v) => v }
                                     GraphQLRequest(query, operationName, variables, extensions)
                                   }
                                   request match {
                                     case Some(req) =>
                                       val stream = generateGraphQLResponse(
                                         req,
                                         id.getOrElse(""),
                                         interpreter,
                                         skipValidation,
                                         enableIntrospection,
                                         queryExecution,
                                         subscriptions
                                       )
                                       webSocketHooks.onMessage
                                         .map(_.transform(stream))
                                         .getOrElse(stream)
                                         .mapM(output.offer)
                                         .runDrain
                                         .catchAll(e => output.offer(makeError(id, e)))
                                         .fork
                                         .interruptible
                                         .unit

                                     case None => output.offer(connectionError)
                                   }
                                 case GraphQLWSInput("stop", id, _)                  =>
                                   removeSubscription(id, subscriptions)
                                 case GraphQLWSInput("connection_terminate", _, _)   =>
                                   ZIO.interrupt
                               }.runDrain.interruptible
                                 .catchAll(_ => output.offer(connectionError))
                                 .ensuring(subscriptions.get.flatMap(m => ZIO.foreach(m.values)(_.succeed(()))))
                                 .provide(env)
                                 .forkDaemon
                             )(_.interrupt) *> ZStream.fromQueueWithShutdown(output)
                         }
      } yield pipe

    makeWebSocketEndpoint.serverLogic[RIO[R, *]](serverRequest =>
      requestInterceptor(serverRequest)(io).catchAll(ZIO.left(_))
    )
  }

  def convertHttpEndpointToFuture[E, R](
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
      _ => a => runtime.unsafeRunToFuture(endpoint.securityLogic(zioMonadError)(a)).future,
      _ => u => req => runtime.unsafeRunToFuture(endpoint.logic(zioMonadError)(u)(req)).future
    )

  def zioMonadError[R]: MonadError[RIO[R, *]] = new MonadError[RIO[R, *]] {
    override def unit[T](t: T): RIO[R, T]                                                                            = URIO.succeed(t)
    override def map[T, T2](fa: RIO[R, T])(f: T => T2): RIO[R, T2]                                                   = fa.map(f)
    override def flatMap[T, T2](fa: RIO[R, T])(f: T => RIO[R, T2]): RIO[R, T2]                                       = fa.flatMap(f)
    override def error[T](t: Throwable): RIO[R, T]                                                                   = RIO.fail(t)
    override protected def handleWrappedError[T](rt: RIO[R, T])(h: PartialFunction[Throwable, RIO[R, T]]): RIO[R, T] =
      rt.catchSome(h)
    override def eval[T](t: => T): RIO[R, T]                                                                         = RIO.effect(t)
    override def suspend[T](t: => RIO[R, T]): RIO[R, T]                                                              = RIO.effectSuspend(t)
    override def flatten[T](ffa: RIO[R, RIO[R, T]]): RIO[R, T]                                                       = ffa.flatten
    override def ensure[T](f: RIO[R, T], e: => RIO[R, Unit]): RIO[R, T]                                              = f.ensuring(e.ignore)
  }

  private[caliban] def keepAlive(keepAlive: Option[Duration]): UStream[GraphQLWSOutput] =
    keepAlive match {
      case None           => ZStream.empty
      case Some(duration) =>
        ZStream
          .succeed(GraphQLWSOutput("ka", None, None))
          .repeat(Schedule.spaced(duration))
          .provideLayer(Clock.live)
    }

  private[caliban] val connectionError: GraphQLWSOutput = GraphQLWSOutput("connection_error", None, None)
  private[caliban] val connectionAck: GraphQLWSOutput   = GraphQLWSOutput("connection_ack", None, None)

  type Subscriptions = Ref[Map[String, Promise[Any, Unit]]]

  private[caliban] def generateGraphQLResponse[R, E](
    payload: GraphQLRequest,
    id: String,
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    queryExecution: QueryExecution,
    subscriptions: Subscriptions
  ): ZStream[R, E, GraphQLWSOutput] = {
    val resp =
      ZStream
        .fromEffect(interpreter.executeRequest(payload, skipValidation, enableIntrospection, queryExecution))
        .flatMap(res =>
          res.data match {
            case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
              trackSubscription(id, subscriptions).flatMap { p =>
                stream.map(toResponse(id, fieldName, _, res.errors)).interruptWhen(p)
              }
            case other                                                =>
              ZStream.succeed(toResponse(id, GraphQLResponse(other, res.errors)))
          }
        )

    (resp ++ complete(id)).catchAll(toStreamError(Option(id), _))
  }

  private def trackSubscription(id: String, subs: Subscriptions): UStream[Promise[Any, Unit]] =
    ZStream.fromEffect(Promise.make[Any, Unit].tap(p => subs.update(_.updated(id, p))))

  private[caliban] def removeSubscription(id: Option[String], subs: Subscriptions): UIO[Unit] =
    IO.whenCase(id) { case Some(id) =>
      subs.modify(map => (map.get(id), map - id)).flatMap { p =>
        IO.whenCase(p) { case Some(p) => p.succeed(()) }
      }
    }

  private[caliban] def toStreamError[E](id: Option[String], e: E): UStream[GraphQLWSOutput] =
    ZStream.succeed(makeError(id, e))

  private def makeError[E](id: Option[String], e: E): GraphQLWSOutput =
    GraphQLWSOutput(
      "error",
      id,
      Some(ResponseValue.ListValue(List(e match {
        case e: CalibanError => e.toResponseValue
        case e               => StringValue(e.toString)
      })))
    )

  private def complete(id: String): UStream[GraphQLWSOutput] =
    ZStream.succeed(GraphQLWSOutput("complete", Some(id), None))

  private def toResponse[E](id: String, fieldName: String, r: ResponseValue, errors: List[E]): GraphQLWSOutput =
    toResponse(id, GraphQLResponse(ObjectValue(List(fieldName -> r)), errors))

  private def toResponse[E](id: String, r: GraphQLResponse[E]): GraphQLWSOutput =
    GraphQLWSOutput("data", Some(id), Some(r.toResponseValue))

  private def parsePath(path: String): List[Either[String, Int]] =
    path.split('.').map(c => Try(c.toInt).toEither.left.map(_ => c)).toList

  private def isFtv1Header(r: Header): Boolean =
    r.name == GraphQLRequest.`apollo-federation-include-trace` && r.value == GraphQLRequest.ftv1
}
