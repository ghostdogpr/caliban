package caliban

import scala.concurrent.ExecutionContext
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.ws.{ Message, TextMessage }
import akka.http.scaladsl.model.{ HttpEntity, HttpResponse, StatusCodes }
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.{ Route, StandardRoute }
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.scaladsl.{ Flow, Sink, Source, SourceQueueWithComplete }
import akka.stream.{ Materializer, OverflowStrategy, QueueOfferResult }
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import zio._
import zio.clock.Clock
import zio.duration._

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
    enableIntrospection: Boolean
  ): URIO[R, HttpResponse] =
    interpreter
      .executeRequest(request, skipValidation = skipValidation, enableIntrospection = enableIntrospection)
      .foldCause(
        cause => json.encodeGraphQLResponse(GraphQLResponse(NullValue, cause.defects)),
        json.encodeGraphQLResponse
      )
      .map(gqlResult => HttpResponse(StatusCodes.OK, entity = HttpEntity(`application/json`, gqlResult)))

  def completeRequest[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true
  )(request: GraphQLRequest)(implicit ec: ExecutionContext, runtime: Runtime[R]): StandardRoute =
    complete(
      runtime
        .unsafeRunToFuture(
          executeHttpResponse(
            interpreter,
            request,
            skipValidation = skipValidation,
            enableIntrospection = enableIntrospection
          )
        )
        .future
    )

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true
  )(implicit ec: ExecutionContext, runtime: Runtime[R]): Route = {
    import akka.http.scaladsl.server.Directives._

    get {
      parameters((Symbol("query").?, Symbol("operationName").?, Symbol("variables").?, Symbol("extensions").?)) {
        case (query, op, vars, ext) =>
          json
            .parseHttpRequest(query, op, vars, ext)
            .fold(
              failWith,
              completeRequest(interpreter, skipValidation = skipValidation, enableIntrospection = enableIntrospection)
            )
      }
    } ~
      post {
        entity(as[GraphQLRequest])(
          completeRequest(interpreter, skipValidation = skipValidation, enableIntrospection = enableIntrospection)
        )
      }
  }

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None
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
      subscriptions: Ref[Map[Option[String], Fiber[Throwable, Unit]]]
    ): RIO[R, Unit] =
      for {
        result <- interpreter.executeRequest(
                   request,
                   skipValidation = skipValidation,
                   enableIntrospection = enableIntrospection
                 )
        _ <- result.data match {
              case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                stream
                  .foreach(item => sendMessage(sendTo, messageId, ObjectValue(List(fieldName -> item)), result.errors))
                  .forkDaemon
                  .flatMap(fiber => subscriptions.update(_.updated(Option(messageId), fiber)))
              case other =>
                sendMessage(sendTo, messageId, other, result.errors) *>
                  IO.fromFuture(_ => sendTo.offer(TextMessage(s"""{"type":"complete","id":"$messageId"}""")))
            }
      } yield ()

    get {
      extractUpgradeToWebSocket { upgrade =>
        val (queue, source) = Source.queue[Message](0, OverflowStrategy.fail).preMaterialize()
        val subscriptions   = runtime.unsafeRun(Ref.make(Map.empty[Option[String], Fiber[Throwable, Unit]]))
        val sink = Sink.foreach[Message] {
          case TextMessage.Strict(text) =>
            val io = for {
              msg     <- Task.fromEither(json.parseWSMessage(text))
              msgType = msg.messageType
              _ <- IO.whenCase(msgType) {
                    case "connection_init" =>
                      Task.fromFuture(_ => queue.offer(TextMessage("""{"type":"connection_ack"}"""))) *>
                        Task.whenCase(keepAliveTime) {
                          case Some(time) =>
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
                    case "start" =>
                      Task.whenCase(msg.request) {
                        case Some(req) =>
                          startSubscription(msg.id, req, queue, subscriptions).catchAll(error =>
                            IO.fromFuture(_ => queue.offer(TextMessage(json.encodeWSError(msg.id, error))))
                          )
                      }
                    case "stop" =>
                      subscriptions
                        .modify(map => (map.get(Option(msg.id)), map - Option(msg.id)))
                        .flatMap(fiber =>
                          IO.whenCase(fiber) {
                            case Some(fiber) =>
                              fiber.interrupt *>
                                IO.fromFuture(_ =>
                                  queue.offer(TextMessage(s"""{"type":"complete","id":"${msg.id}"}"""))
                                )
                          }
                        )
                  }
            } yield ()
            runtime.unsafeRun(io)
          case _ => ()
        }

        val flow = Flow.fromSinkAndSource(sink, source).watchTermination() { (_, f) =>
          f.onComplete(_ => runtime.unsafeRun(subscriptions.get.flatMap(m => IO.foreach(m.values)(_.interrupt).unit)))
        }

        complete(upgrade.handleMessages(flow, subprotocol = Some("graphql-ws")))
      }
    }
  }
}

object AkkaHttpAdapter {

  /**
   * @see [[AkkaHttpAdapter]]
   */
  def apply(jsonBackend: JsonBackend): AkkaHttpAdapter = new AkkaHttpAdapter {
    val json: JsonBackend = jsonBackend
  }
}
