package caliban

import scala.concurrent.{ ExecutionContext, Future, Promise }
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.ws.{ Message, TextMessage }
import akka.http.scaladsl.model.{ HttpEntity, HttpResponse, StatusCodes }
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.{ Route, StandardRoute }
import akka.stream.scaladsl.{ Flow, Sink, Source, SourceQueueWithComplete }
import akka.stream.{ Materializer, OverflowStrategy, QueueOfferResult }
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Decoder.Result
import io.circe.Json
import io.circe.parser._
import io.circe.syntax._
import zio.{ Exit, Fiber, IO, Ref, Runtime, Task, URIO, ZIO }

object AkkaHttpAdapter extends FailFastCirceSupport {

  private def execute[R, E](
    interpreter: GraphQLInterpreter[R, E],
    query: GraphQLRequest,
    skipValidation: Boolean
  ): URIO[R, GraphQLResponse[E]] =
    interpreter.execute(query.query, query.operationName, query.variables.getOrElse(Map()), skipValidation)

  private def executeHttpResponse[R, E](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest,
    skipValidation: Boolean
  ): URIO[R, HttpResponse] =
    execute(interpreter, request, skipValidation)
      .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
      .map(gqlResult => HttpResponse(StatusCodes.OK, entity = HttpEntity(`application/json`, gqlResult.toString())))

  def getGraphQLRequest(query: String, op: Option[String], vars: Option[String]): Result[GraphQLRequest] = {
    val variablesJs = vars.flatMap(parse(_).toOption)
    val fields = List("query" -> Json.fromString(query)) ++
      op.map(o => "operationName"       -> Json.fromString(o)) ++
      variablesJs.map(js => "variables" -> js)
    Json
      .fromFields(fields)
      .as[GraphQLRequest]
  }

  private def unsafeRunToFuture[R, E <: Throwable, A](zio: ZIO[R, E, A])(implicit runtime: Runtime[R]): Future[A] = {
    val p = Promise[A]()
    runtime.unsafeRunAsync(zio) {
      case Exit.Success(value) => p.success(value)
      case Exit.Failure(cause) => p.failure(cause.squashTrace)
    }
    p.future
  }

  def completeRequest[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false
  )(request: GraphQLRequest)(implicit ec: ExecutionContext, runtime: Runtime[R]): StandardRoute =
    complete(unsafeRunToFuture(executeHttpResponse(interpreter, request, skipValidation)))

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false
  )(implicit ec: ExecutionContext, runtime: Runtime[R]): Route = {
    import akka.http.scaladsl.server.Directives._

    get {
      parameters((Symbol("query").as[String], Symbol("operationName").?, Symbol("variables").?)) {
        case (query, op, vars) =>
          getGraphQLRequest(query, op, vars)
            .fold(failWith, completeRequest(interpreter, skipValidation))
      }
    } ~
      post {
        entity(as[GraphQLRequest])(completeRequest(interpreter, skipValidation))
      }
  }

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false
  )(implicit ec: ExecutionContext, runtime: Runtime[R], materializer: Materializer): Route = {
    def sendMessage(
      sendQueue: SourceQueueWithComplete[Message],
      id: String,
      data: ResponseValue,
      errors: List[E]
    ): Task[QueueOfferResult] =
      IO.fromFuture(_ =>
        sendQueue.offer(
          TextMessage(
            Json
              .obj(
                "id"      -> Json.fromString(id),
                "type"    -> Json.fromString("data"),
                "payload" -> GraphQLResponse(data, errors).asJson
              )
              .noSpaces
          )
        )
      )

    import akka.http.scaladsl.server.Directives._

    get {
      extractUpgradeToWebSocket { upgrade =>
        val (queue, source) = Source.queue[Message](0, OverflowStrategy.fail).preMaterialize()
        val subscriptions   = runtime.unsafeRun(Ref.make(Map.empty[String, Fiber[Throwable, Unit]]))
        val sink = Sink.foreach[Message] {
          case TextMessage.Strict(text) =>
            val io = for {
              msg     <- Task.fromEither(decode[Json](text))
              msgType = msg.hcursor.downField("type").success.flatMap(_.value.asString).getOrElse("")
              _ <- IO.whenCase(msgType) {
                    case "connection_init" =>
                      IO.fromFuture(_ => queue.offer(TextMessage("""{"type":"connection_ack"}""")))
                    case "connection_terminate" =>
                      IO.effect(queue.complete())
                    case "start" =>
                      val payload = msg.hcursor.downField("payload")
                      val id      = msg.hcursor.downField("id").success.flatMap(_.value.asString).getOrElse("")
                      Task.whenCase(payload.downField("query").success.flatMap(_.value.asString)) {
                        case Some(query) =>
                          val operationName = payload.downField("operationName").success.flatMap(_.value.asString)
                          (for {
                            result <- execute(interpreter, GraphQLRequest(query, operationName, None), skipValidation)
                            _ <- result.data match {
                                  case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
                                    stream.foreach { item =>
                                      sendMessage(queue, id, ObjectValue(List(fieldName -> item)), result.errors)
                                    }.fork.flatMap(fiber => subscriptions.update(_.updated(id, fiber)))
                                  case other =>
                                    sendMessage(queue, id, other, result.errors) *> IO.fromFuture(_ =>
                                      queue.offer(TextMessage(s"""{"type":"complete","id":"$id"}"""))
                                    )
                                }
                          } yield ()).catchAll(error =>
                            IO.fromFuture(_ =>
                              queue.offer(
                                TextMessage(
                                  Json
                                    .obj(
                                      "id"      -> Json.fromString(id),
                                      "type"    -> Json.fromString("complete"),
                                      "payload" -> Json.fromString(error.toString)
                                    )
                                    .noSpaces
                                )
                              )
                            )
                          )
                      }
                    case "stop" =>
                      val id = msg.hcursor.downField("id").success.flatMap(_.value.asString).getOrElse("")
                      subscriptions
                        .modify(map => (map.get(id), map - id))
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
