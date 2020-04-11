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
import zio.random.Random

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
    skipValidation: Boolean
  ): URIO[R, HttpResponse] =
    interpreter
      .executeRequest(request, skipValidation)
      .foldCause(
        cause => json.encodeGraphQLResponse(GraphQLResponse(NullValue, cause.defects)),
        json.encodeGraphQLResponse
      )
      .map(gqlResult => HttpResponse(StatusCodes.OK, entity = HttpEntity(`application/json`, gqlResult)))

  def completeRequest[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false
  )(request: GraphQLRequest)(implicit ec: ExecutionContext, runtime: Runtime[R]): StandardRoute =
    complete(runtime.unsafeRunToFuture(executeHttpResponse(interpreter, request, skipValidation)).future)

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false
  )(implicit ec: ExecutionContext, runtime: Runtime[R]): Route = {
    import akka.http.scaladsl.server.Directives._

    get {
      parameters((Symbol("query").as[String], Symbol("operationName").?, Symbol("variables").?, Symbol("extensions").?)) {
        case (query, op, vars, ext) =>
          json
            .parseHttpRequest(query, op, vars, ext)
            .fold(failWith, completeRequest(interpreter, skipValidation))
      }
    } ~
      post {
        entity(as[GraphQLRequest])(completeRequest(interpreter, skipValidation))
      }
  }

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    keepAliveTime: Option[Duration] = None
  )(implicit ec: ExecutionContext, runtime: Runtime[R with Clock with Random], materializer: Materializer): Route = {
    def sendMessage(
      sendQueue: SourceQueueWithComplete[Message],
      id: String,
      data: ResponseValue,
      errors: List[E]
    ): Task[QueueOfferResult] =
      IO.fromFuture(_ =>
        sendQueue.offer(
          TextMessage(json.encodeWSResponse(id, data, errors))
        )
      )

    import akka.http.scaladsl.server.Directives._

    def startSubscription(
      message: WSMessage,
      query: String,
      sendTo: SourceQueueWithComplete[Message],
      subscriptions: Ref[Map[String, Fiber[Throwable, Unit]]]
    ): RIO[R, Unit] =
      for {
        result <- interpreter.execute(query, message.operationName, skipValidation = skipValidation)
        _ <- result.data match {
              case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                stream
                  .foreach(item => sendMessage(sendTo, message.id, ObjectValue(List(fieldName -> item)), result.errors))
                  .forkDaemon
                  .flatMap(fiber => subscriptions.update(_.updated(message.id, fiber)))
              case other =>
                sendMessage(sendTo, message.id, other, result.errors) *> IO.fromFuture(_ =>
                  sendTo.offer(TextMessage(s"""{"type":"complete","id":"${message.id}"}"""))
                )
            }
      } yield ()

    get {
      extractUpgradeToWebSocket { upgrade =>
        val (queue: SourceQueueWithComplete[Message], source) =
          Source.queue[Message](0, OverflowStrategy.fail).preMaterialize()
        val subscriptions = runtime.unsafeRun(Ref.make(Map.empty[String, Fiber[Throwable, Unit]]))
        val sink = Sink.foreach[Message] {
          case TextMessage.Strict(text) =>
            val io = for {
              msg     <- Task.fromEither(json.parseWSMessage(text))
              msgType = msg.messageType
              _ <- IO.whenCase(msgType) {
                    case "connection_init" =>
                      val ack: ZIO[Clock with Random, Throwable, Any] =
                        IO.fromFuture(_ => queue.offer(TextMessage("""{"type":"connection_ack"}""")))
                      keepAliveTime.fold(ack) { time =>
                        ack
                          .flatMap(a =>
                            IO.fromFuture(_ =>
                                queue
                                  .offer(
                                    TextMessage("""{"type":"ka"}""")
                                  )
                              )
                              .repeat(Schedule.spaced(time).jittered)
                              .forkDaemon
                          )
                      }
                    case "connection_terminate" =>
                      IO.effect(queue.complete())
                    case "start" =>
                      Task.whenCase(msg.query) {
                        case Some(query) =>
                          startSubscription(msg, query, queue, subscriptions).catchAll(error =>
                            IO.fromFuture(_ =>
                              queue.offer(
                                TextMessage(json.encodeWSError(msg.id, error))
                              )
                            )
                          )
                      }
                    case "stop" =>
                      subscriptions
                        .modify(map => (map.get(msg.id), map - msg.id))
                        .flatMap(fiber => IO.whenCase(fiber) { case Some(fiber) => fiber.interrupt })
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
