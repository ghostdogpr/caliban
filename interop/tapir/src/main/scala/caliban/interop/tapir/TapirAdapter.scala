package caliban.interop.tapir

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.StringValue
import caliban._
import caliban.execution.QueryExecution
import caliban.uploads.{ FileMeta, GraphQLUploadRequest, Uploads }
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.capabilities.zio.ZioStreams.Pipe
import sttp.model.{ headers => _, _ }
import sttp.monad.MonadError
import sttp.tapir.Codec.JsonCodec
import sttp.tapir._
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import zio._
import zio.clock.Clock
import zio.duration.Duration
import zio.random.Random
import zio.stream._

import scala.concurrent.Future
import scala.util.Try

object TapirAdapter {

  type CalibanPipe   = Pipe[GraphQLWSInput, GraphQLWSOutput]
  type UploadRequest = (Seq[Part[Array[Byte]]], ServerRequest)
  type ZioWebSockets = ZioStreams with WebSockets

  def makeHttpEndpoints[R, E](implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): List[Endpoint[(GraphQLRequest, ServerRequest), StatusCode, GraphQLResponse[E], Any]] = {
    def queryFromQueryParams(queryParams: QueryParams): DecodeResult[GraphQLRequest] =
      for {
        req <- requestCodec.decode(s"""{"query":"","variables":${queryParams
                 .get("variables")
                 .getOrElse("null")},"extensions":${queryParams
                 .get("extensions")
                 .getOrElse("null")}}""")

      } yield req.copy(query = queryParams.get("query"), operationName = queryParams.get("operationName"))

    val postEndpoint: Endpoint[(GraphQLRequest, ServerRequest), StatusCode, GraphQLResponse[E], Any] =
      endpoint.post
        .in(
          (headers and stringBody and queryParams).mapDecode { case (headers, body, params) =>
            val getRequest =
              if (params.get("query").isDefined)
                queryFromQueryParams(params)
              else if (
                headers.exists(header =>
                  header.name == HeaderNames.ContentType &&
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
        .out(customJsonBody[GraphQLResponse[E]])
        .errorOut(statusCode)

    val getEndpoint: Endpoint[(GraphQLRequest, ServerRequest), StatusCode, GraphQLResponse[E], Any] =
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
        .out(customJsonBody[GraphQLResponse[E]])
        .errorOut(statusCode)

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
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): List[ServerEndpoint[(GraphQLRequest, ServerRequest), StatusCode, GraphQLResponse[E], Any, RIO[R, *]]] = {
    def logic(request: (GraphQLRequest, ServerRequest)): RIO[R, Either[StatusCode, GraphQLResponse[E]]] = {
      val (graphQLRequest, serverRequest) = request

      (requestInterceptor(serverRequest) *>
        interpreter
          .executeRequest(
            graphQLRequest,
            skipValidation = skipValidation,
            enableIntrospection = enableIntrospection,
            queryExecution
          )).either
    }

    makeHttpEndpoints.map(_.serverLogic(logic))
  }

  def makeHttpUploadEndpoint[R, E](implicit
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): Endpoint[(Seq[Part[Array[Byte]]], ServerRequest), StatusCode, GraphQLResponse[E], Any] =
    endpoint.post
      .in(multipartBody)
      .in(extractFromRequest(identity))
      .out(customJsonBody[GraphQLResponse[E]])
      .errorOut(statusCode)

  def makeHttpUploadService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]],
    responseCodec: JsonCodec[GraphQLResponse[E]]
  ): ServerEndpoint[UploadRequest, StatusCode, GraphQLResponse[E], Any, RIO[R with Random, *]] = {
    def logic(request: UploadRequest): RIO[R with Random, Either[StatusCode, GraphQLResponse[E]]] = {
      val (parts, serverRequest) = request
      val partsMap               = parts.map(part => part.name -> part).toMap

      val io =
        for {
          _             <- requestInterceptor(serverRequest)
          rawOperations <- ZIO.fromOption(partsMap.get("operations")) orElseFail StatusCode.BadRequest
          request       <- requestCodec.rawDecode(new String(rawOperations.body, "utf-8")) match {
                             case _: DecodeResult.Failure => ZIO.fail(StatusCode.BadRequest)
                             case DecodeResult.Value(v)   => UIO(v)
                           }
          rawMap        <- ZIO.fromOption(partsMap.get("map")) orElseFail StatusCode.BadRequest
          map           <- mapCodec.rawDecode(new String(rawMap.body, "utf-8")) match {
                             case _: DecodeResult.Failure => ZIO.fail(StatusCode.BadRequest)
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

      io.either
    }

    makeHttpUploadEndpoint.serverLogic(logic)
  }

  def makeWebSocketEndpoint[R, E](implicit
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): Endpoint[ServerRequest, StatusCode, CalibanPipe, ZioStreams with WebSockets] = {
    val protocolHeader = Header("Sec-WebSocket-Protocol", "graphql-ws")
    endpoint
      .in(header(protocolHeader))
      .in(extractFromRequest(identity))
      .out(header(protocolHeader))
      .out(webSocketBody[GraphQLWSInput, CodecFormat.Json, GraphQLWSOutput, CodecFormat.Json](ZioStreams))
      .errorOut(statusCode)
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
  ): ServerEndpoint[ServerRequest, StatusCode, CalibanPipe, ZioWebSockets, RIO[R, *]] = {

    val io: URIO[R, Either[Nothing, CalibanPipe]] =
      RIO
        .environment[R]
        .flatMap(env =>
          Ref
            .make(Map.empty[String, Promise[Any, Unit]])
            .flatMap(subscriptions =>
              UIO.right[CalibanPipe](
                _.collect {
                  case GraphQLWSInput("connection_init", id, payload) =>
                    val before = (webSocketHooks.beforeInit, payload) match {
                      case (Some(beforeInit), Some(payload)) =>
                        ZStream.fromEffect(beforeInit(payload)).drain.catchAll(toStreamError(id, _))
                      case _                                 => Stream.empty
                    }

                    val response = connectionAck ++ keepAlive(keepAliveTime)

                    val after = webSocketHooks.afterInit match {
                      case Some(afterInit) => ZStream.fromEffect(afterInit).drain.catchAll(toStreamError(id, _))
                      case _               => Stream.empty
                    }

                    before ++ ZStream.mergeAllUnbounded()(response, after)
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
                          .catchAll(toStreamError(id, _))

                      case None => connectionError
                    }
                  case GraphQLWSInput("stop", id, _)                  =>
                    removeSubscription(id, subscriptions) *> ZStream.empty
                  case GraphQLWSInput("connection_terminate", _, _)   =>
                    ZStream.fromEffect(ZIO.interrupt)
                }.flatten
                  .catchAll(_ => connectionError)
                  .ensuring(subscriptions.get.flatMap(m => ZIO.foreach(m.values)(_.succeed(()))))
                  .provide(env)
              )
            )
        )

    makeWebSocketEndpoint.serverLogic[RIO[R, *]](serverRequest =>
      requestInterceptor(serverRequest).foldM(statusCode => ZIO.left(statusCode), _ => io)
    )
  }

  def convertHttpEndpointToFuture[A, E, R](
    endpoint: ServerEndpoint[A, StatusCode, GraphQLResponse[E], Any, RIO[R, *]]
  )(implicit runtime: Runtime[R]): ServerEndpoint[A, StatusCode, GraphQLResponse[E], Any, Future] =
    ServerEndpoint[A, StatusCode, GraphQLResponse[E], Any, Future](
      endpoint.endpoint,
      _ => req => runtime.unsafeRunToFuture(endpoint.logic(zioMonadError)(req)).future
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

  private[caliban] val connectionError: UStream[GraphQLWSOutput] =
    ZStream.succeed(GraphQLWSOutput("connection_error", None, None))
  private[caliban] val connectionAck: UStream[GraphQLWSOutput]   =
    ZStream.succeed(GraphQLWSOutput("connection_ack", None, None))

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

  private[caliban] def trackSubscription(id: String, subs: Subscriptions): UStream[Promise[Any, Unit]] =
    ZStream.fromEffect(Promise.make[Any, Unit].tap(p => subs.update(_.updated(id, p))))

  private[caliban] def removeSubscription(id: Option[String], subs: Subscriptions): UStream[Unit] =
    ZStream
      .fromEffect(IO.whenCase(id) { case Some(id) =>
        subs.modify(map => (map.get(id), map - id)).flatMap { p =>
          IO.whenCase(p) { case Some(p) => p.succeed(()) }
        }
      })

  private[caliban] def toStreamError[E](id: Option[String], e: E): UStream[GraphQLWSOutput] =
    ZStream.succeed(
      GraphQLWSOutput(
        "error",
        id,
        Some(ResponseValue.ListValue(List(e match {
          case e: CalibanError => e.toResponseValue
          case e               => StringValue(e.toString)
        })))
      )
    )

  private[caliban] def complete(id: String): UStream[GraphQLWSOutput] =
    ZStream.succeed(GraphQLWSOutput("complete", Some(id), None))

  private[caliban] def toResponse[E](
    id: String,
    fieldName: String,
    r: ResponseValue,
    errors: List[E]
  ): GraphQLWSOutput =
    toResponse(id, GraphQLResponse(ObjectValue(List(fieldName -> r)), errors))

  private[caliban] def toResponse[E](id: String, r: GraphQLResponse[E]): GraphQLWSOutput =
    GraphQLWSOutput("data", Some(id), Some(r.toResponseValue))

  private def parsePath(path: String): List[Either[String, Int]] =
    path.split('.').map(c => Try(c.toInt).toEither.left.map(_ => c)).toList

  private def isFtv1Header(r: Header): Boolean =
    r.name == GraphQLRequest.`apollo-federation-include-trace` && r.value == GraphQLRequest.ftv1
}
