package caliban.interop.tapir

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.{ NullValue, StringValue }
import caliban.execution.QueryExecution
import caliban._
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.capabilities.zio.ZioStreams.Pipe
import sttp.model.{ Header, QueryParams, StatusCode }
import sttp.tapir.Codec.JsonCodec
import sttp.tapir._
import sttp.tapir.generic.auto._
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import zio._
import zio.clock.Clock
import zio.duration.Duration
import zio.stream._

object TapirAdapter {

  type CalibanPipe   = Pipe[GraphQLWSInput, GraphQLWSOutput]
  type ZioWebSockets = ZioStreams with WebSockets

  implicit val streamSchema: Schema[StreamValue]                     =
    Schema.schemaForUnit.map(_ => Some(StreamValue(ZStream(NullValue))))(_ => ())
  implicit val throwableSchema: Schema[Throwable]                    =
    Schema.schemaForString.map(s => Some(new Throwable(s)))(_.getMessage)
  implicit lazy val inputValueSchema: Schema[InputValue]             = Schema.derivedSchema
  implicit lazy val responseValueSchema: Schema[ResponseValue]       = Schema.derivedSchema
  implicit val calibanErrorSchema: Schema[CalibanError]              = Schema.derivedSchema
  implicit val requestSchema: Schema[GraphQLRequest]                 = Schema.derivedSchema
  implicit def responseSchema[E: Schema]: Schema[GraphQLResponse[E]] = Schema.derivedSchema
  implicit val wsInputSchema: Schema[GraphQLWSInput]                 = Schema.derivedSchema
  implicit val wsOutputSchema: Schema[GraphQLWSOutput]               = Schema.derivedSchema

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
              else if (headers.contains(Header("content/type", "application/graphql")))
                DecodeResult.Value(GraphQLRequest(query = Some(body)))
              else requestCodec.decode(body)

            getRequest.map(request =>
              headers
                .find(r => r.name == GraphQLRequest.`apollo-federation-include-trace` && r.value == GraphQLRequest.ftv1)
                .fold(request)(_ => request.withFederatedTracing)
            )
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

    postEndpoint.serverLogic(logic) :: getEndpoint.serverLogic(logic) :: Nil
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
    val protocolHeader = Header("Sec-WebSocket-Protocol", "graphql-ws")
    val wsEndpoint     =
      endpoint
        .in(header(protocolHeader))
        .in(extractFromRequest(identity))
        .out(header(protocolHeader))
        .out(webSocketBody[GraphQLWSInput, CodecFormat.Json, GraphQLWSOutput, CodecFormat.Json](ZioStreams))
        .errorOut(statusCode)

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

    wsEndpoint
      .serverLogic[RIO[R, *]](serverRequest =>
        requestInterceptor(serverRequest).foldM(statusCode => ZIO.left(statusCode), _ => io)
      )
  }

  private def keepAlive(keepAlive: Option[Duration]): UStream[GraphQLWSOutput] =
    keepAlive match {
      case None           => ZStream.empty
      case Some(duration) =>
        ZStream
          .succeed(GraphQLWSOutput("ka", None, None))
          .repeat(Schedule.spaced(duration))
          .provideLayer(Clock.live)
    }

  private val connectionError: UStream[GraphQLWSOutput] =
    ZStream.succeed(GraphQLWSOutput("connection_error", None, None))
  private val connectionAck: UStream[GraphQLWSOutput]   =
    ZStream.succeed(GraphQLWSOutput("connection_ack", None, None))

  type Subscriptions = Ref[Map[String, Promise[Any, Unit]]]

  private def generateGraphQLResponse[R, E](
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

  private def removeSubscription(id: Option[String], subs: Subscriptions): UStream[Unit] =
    ZStream
      .fromEffect(IO.whenCase(id) { case Some(id) =>
        subs.modify(map => (map.get(id), map - id)).flatMap { p =>
          IO.whenCase(p) { case Some(p) => p.succeed(()) }
        }
      })

  private def toStreamError[E](id: Option[String], e: E): UStream[GraphQLWSOutput] =
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

  private def complete(id: String): UStream[GraphQLWSOutput] =
    ZStream.succeed(GraphQLWSOutput("complete", Some(id), None))

  private def toResponse[E](id: String, fieldName: String, r: ResponseValue, errors: List[E]): GraphQLWSOutput =
    toResponse(id, GraphQLResponse(ObjectValue(List(fieldName -> r)), errors))

  private def toResponse[E](id: String, r: GraphQLResponse[E]): GraphQLWSOutput =
    GraphQLWSOutput("data", Some(id), Some(r.toResponseValue))
}
