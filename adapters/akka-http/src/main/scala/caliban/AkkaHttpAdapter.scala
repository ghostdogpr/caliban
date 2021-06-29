package caliban

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws.{ Message, TextMessage }
import akka.http.scaladsl.server.Directives.{ complete, extractRequestContext }
import akka.http.scaladsl.server.{ RequestContext, Route }
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.scaladsl.{ Flow, Keep, Sink, Source, SourceQueueWithComplete }
import akka.stream.{ Materializer, OverflowStrategy, QueueOfferResult }
import caliban.AkkaHttpAdapter.{ `application/graphql`, ContextWrapper }
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import zio.Exit.Failure
import zio._
import zio.clock.Clock
import zio.duration._

import scala.concurrent.duration.{ Duration => ScalaDuration }
import scala.concurrent.ExecutionContext
import caliban.execution.QueryExecution

/**
 * Akka-http adapter for caliban with pluggable json backend.
 * There are two ways to use it:
 * <br/>
 * <br/>
 * 1) Create the adapter manually (using [[AkkaHttpAdapter.apply]] and explicitly specify backend (recommended way):
 * {{{
 * val adapter = AkkaHttpAdapter(new CirceJsonBackend)
 * adapter.makeHttpService(interpreter)
 * }}}
 *
 * 2) Mix in an `all-included` trait, like [[caliban.interop.circe.AkkaHttpCirceAdapter]]:
 * {{{
 * class MyApi extends AkkaHttpCirceAdapter {
 *
 *   // adapter is provided by the mixin
 *   adapter.makeHttpService(interpreter)
 * }
 * }}}
 *
 * @note Since all json backend dependencies are optional,
 *       you have to explicitly specify a corresponding dependency in your build (see specific backends for details).
 */
trait AkkaHttpAdapter {

  def json: JsonBackend

  implicit def requestUnmarshaller: FromEntityUnmarshaller[GraphQLRequest] = json.reqUnmarshaller

  private def executeHttpResponse[R, E](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest,
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    queryExecution: QueryExecution
  ): URIO[R, HttpResponse] =
    interpreter
      .executeRequest(
        request,
        skipValidation = skipValidation,
        enableIntrospection = enableIntrospection,
        queryExecution = queryExecution
      )
      .foldCause(
        cause => json.encodeGraphQLResponse(GraphQLResponse(NullValue, cause.defects)),
        json.encodeGraphQLResponse
      )
      .map(gqlResult => HttpResponse(StatusCodes.OK, entity = HttpEntity(`application/json`, gqlResult)))

  private def supportFederatedTracing(context: RequestContext, request: GraphQLRequest): GraphQLRequest =
    if (
      context.request.headers
        .exists(h => h.is(GraphQLRequest.`apollo-federation-include-trace`) && h.value() == GraphQLRequest.ftv1)
    ) {
      request.withFederatedTracing
    } else request

  def completeRequest[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    contextWrapper: ContextWrapper[R, HttpResponse] = ContextWrapper.empty,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(request: GraphQLRequest)(implicit ec: ExecutionContext, runtime: Runtime[R]): Route =
    extractRequestContext { ctx =>
      complete(
        runtime
          .unsafeRunToFuture(
            contextWrapper(ctx) {
              executeHttpResponse(
                interpreter,
                supportFederatedTracing(ctx, request),
                skipValidation = skipValidation,
                enableIntrospection = enableIntrospection,
                queryExecution = queryExecution
              )
            }
          )
          .future
      )
    }

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    contextWrapper: ContextWrapper[R, HttpResponse] = ContextWrapper.empty,
    queryExecution: QueryExecution = QueryExecution.Parallel
  )(implicit ec: ExecutionContext, runtime: Runtime[R]): Route = {
    import akka.http.scaladsl.server.Directives._

    get {
      parameters(Symbol("query").?, Symbol("operationName").?, Symbol("variables").?, Symbol("extensions").?) {
        case (query, op, vars, ext) =>
          json
            .parseHttpRequest(query, op, vars, ext)
            .fold(
              failWith,
              completeRequest(
                interpreter,
                skipValidation = skipValidation,
                enableIntrospection = enableIntrospection,
                contextWrapper = contextWrapper,
                queryExecution = queryExecution
              )
            )
      }
    } ~
      post {
        extractRequestEntity { requestEntity =>
          parameters(Symbol("query").?, Symbol("operationName").?, Symbol("variables").?, Symbol("extensions").?) {
            case (query @ Some(_), op, vars, ext)                                  =>
              json
                .parseHttpRequest(query, op, vars, ext)
                .fold(
                  failWith,
                  completeRequest(
                    interpreter,
                    skipValidation = skipValidation,
                    enableIntrospection = enableIntrospection,
                    contextWrapper = contextWrapper
                  )
                )
            case _ if requestEntity.contentType.mediaType == `application/graphql` =>
              entity(as[String])(query =>
                completeRequest(
                  interpreter,
                  skipValidation = skipValidation,
                  enableIntrospection = enableIntrospection,
                  contextWrapper = contextWrapper
                )(GraphQLRequest(Some(query)))
              )
            case _                                                                 =>
              entity(as[GraphQLRequest])(
                completeRequest(
                  interpreter,
                  skipValidation = skipValidation,
                  enableIntrospection = enableIntrospection,
                  contextWrapper = contextWrapper
                )
              )
          }
        }
      }
  }

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    contextWrapper: ContextWrapper[R, GraphQLResponse[E]] = ContextWrapper.empty,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    wsChunkTimeout: Duration = 5.seconds
  )(implicit ec: ExecutionContext, runtime: Runtime[R], materializer: Materializer): Route = {

    def sendMessage(
      sendQueue: SourceQueueWithComplete[Message],
      id: String,
      data: ResponseValue,
      errors: List[E]
    ): Task[QueueOfferResult] =
      IO.fromFuture(_ => sendQueue.offer(TextMessage(json.encodeWSResponse(id, data, errors))))

    import akka.http.scaladsl.server.Directives._

    def startSubscription(
      messageId: String,
      request: GraphQLRequest,
      sendTo: SourceQueueWithComplete[Message],
      subscriptions: Ref[Map[Option[String], Fiber[Throwable, Unit]]],
      ctx: RequestContext
    ): RIO[R, Unit] =
      for {
        result <- contextWrapper(ctx)(
                    interpreter.executeRequest(
                      request,
                      skipValidation = skipValidation,
                      enableIntrospection = enableIntrospection,
                      queryExecution = queryExecution
                    )
                  )
        _      <- result.data match {
                    case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                      stream
                        .foreach(item => sendMessage(sendTo, messageId, ObjectValue(List(fieldName -> item)), result.errors))
                        .onExit {
                          case Failure(cause) if !cause.interrupted =>
                            IO.fromFuture(_ =>
                              sendTo.offer(TextMessage(json.encodeWSError(messageId, cause.squash.toString)))
                            ).ignore
                          case _                                    =>
                            IO.fromFuture(_ => sendTo.offer(TextMessage(s"""{"type":"complete","id":"$messageId"}""")))
                              .ignore
                        }
                        .forkDaemon
                        .flatMap(fiber => subscriptions.update(_.updated(Option(messageId), fiber)))
                    case other                                                =>
                      sendMessage(sendTo, messageId, other, result.errors) *>
                        IO.fromFuture(_ => sendTo.offer(TextMessage(s"""{"type":"complete","id":"$messageId"}""")))
                  }
      } yield ()

    get {
      extractRequestContext { ctx =>
        extractWebSocketUpgrade { upgrade =>
          val (queue, source) = Source.queue[Message](0, OverflowStrategy.fail).preMaterialize()
          val subscriptions   = runtime.unsafeRun(Ref.make(Map.empty[Option[String], Fiber[Throwable, Unit]]))
          val sink            = Flow[Message].collect { case tm: TextMessage => tm }
            .mapAsync(1)(_.toStrict(ScalaDuration.fromNanos(wsChunkTimeout.toNanos)).map(_.text))
            .toMat(Sink.foreach { text =>
              val io = for {
                msg    <- Task.fromEither(json.parseWSMessage(text))
                msgType = msg.messageType
                _      <- RIO.whenCase(msgType) {
                            case "connection_init"      =>
                              Task.fromFuture(_ => queue.offer(TextMessage("""{"type":"connection_ack"}"""))) *>
                                Task.whenCase(keepAliveTime) { case Some(time) =>
                                  // Save the keep-alive fiber with a key of None so that it's interrupted later
                                  IO.fromFuture(_ => queue.offer(TextMessage("""{"type":"ka"}""")))
                                    .repeat(Schedule.spaced(time))
                                    .provideLayer(Clock.live)
                                    .unit
                                    .forkDaemon
                                    .flatMap(keepAliveFiber => subscriptions.update(_.updated(None, keepAliveFiber)))
                                }
                            case "connection_terminate" =>
                              IO.effect(queue.complete())
                            case "start"                =>
                              RIO.whenCase(msg.request) { case Some(req) =>
                                startSubscription(msg.id, req, queue, subscriptions, ctx).catchAll(error =>
                                  IO.fromFuture(_ => queue.offer(TextMessage(json.encodeWSError(msg.id, error.toString))))
                                )
                              }
                            case "stop"                 =>
                              subscriptions
                                .modify(map => (map.get(Option(msg.id)), map - Option(msg.id)))
                                .flatMap(fiber =>
                                  IO.whenCase(fiber) { case Some(fiber) =>
                                    fiber.interrupt
                                  }
                                )
                          }
              } yield ()
              runtime.unsafeRun(io)
            })(Keep.right)

          val flow = Flow.fromSinkAndSourceCoupled(sink, source).watchTermination() { (_, f) =>
            f.onComplete(_ => runtime.unsafeRun(subscriptions.get.flatMap(m => IO.foreach(m.values)(_.interrupt).unit)))
          }

          complete(upgrade.handleMessages(flow, subprotocol = Some("graphql-ws")))
        }
      }
    }
  }
}

object AkkaHttpAdapter {

  val `application/graphql`: MediaType = MediaType.applicationWithFixedCharset("graphql", HttpCharsets.`UTF-8`)

  /**
   * ContextWrapper provides a way to pass context from http request into Caliban's query handling.
   */
  trait ContextWrapper[-R, +A] { self =>
    def apply[R1 <: R, A1 >: A](ctx: RequestContext)(e: URIO[R1, A1]): URIO[R1, A1]

    def |+|[R1 <: R, A1 >: A](that: ContextWrapper[R1, A1]): ContextWrapper[R1, A1] = new ContextWrapper[R1, A1] {
      override def apply[R2 <: R1, A2 >: A1](ctx: RequestContext)(e: URIO[R2, A2]): URIO[R2, A2] =
        that.apply[R2, A2](ctx)(self.apply[R2, A2](ctx)(e))
    }
  }

  object ContextWrapper {
    def empty: ContextWrapper[Any, Nothing] = new ContextWrapper[Any, Nothing] {
      override def apply[R, Nothing](ctx: RequestContext)(effect: URIO[R, Nothing]): URIO[R, Nothing] =
        effect
    }
  }

  /**
   * @see [[AkkaHttpAdapter]]
   */
  def apply(jsonBackend: JsonBackend): AkkaHttpAdapter = new AkkaHttpAdapter {
    val json: JsonBackend = jsonBackend
  }
}
