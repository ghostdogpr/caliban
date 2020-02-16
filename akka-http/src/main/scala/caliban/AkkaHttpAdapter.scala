package caliban

import caliban.ResponseValue._
import akka.NotUsed
import akka.actor.ActorRef
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.ws.TextMessage.Streamed
import akka.http.scaladsl.model.ws.{ BinaryMessage, Message, TextMessage }
import akka.http.scaladsl.model.{ HttpEntity, HttpResponse, StatusCodes }
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{ Route, StandardRoute }
import akka.stream.{ CompletionStrategy, Materializer, OverflowStrategy }
import akka.stream.scaladsl.{ Flow, Keep, Sink, Source }
import caliban.Value.NullValue
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Decoder.Result
import io.circe.{ ACursor, Json }
import io.circe.syntax._
import zio.{ Fiber, IO, RIO, Ref, Runtime, Task, UIO, URIO, ZIO }

import scala.concurrent.ExecutionContext

object AkkaHttpAdapter extends FailFastCirceSupport {
  private def execute[R, E](interpreter: GraphQLInterpreter[R, E], query: GraphQLRequest): URIO[R, GraphQLResponse[E]] =
    interpreter.execute(query.query, query.operationName, query.variables.getOrElse(Map()))

  private def executeHttpResponse[R, E](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest
  ): URIO[R, HttpResponse] =
    interpreter
      .execute(request.query, request.operationName, request.variables.getOrElse(Map()))
      .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
      .map(gqlResult => HttpResponse(StatusCodes.OK, entity = HttpEntity(`application/json`, gqlResult.toString())))

  def getGraphQLRequest(query: String, op: Option[String], vars: Option[String]): Result[GraphQLRequest] = {
    import io.circe.parser._
    val variablesJs = vars.flatMap(parse(_).toOption)
    val fields = List("query" -> Json.fromString(query)) ++
      op.map(o => "operationName"       -> Json.fromString(o)) ++
      variablesJs.map(js => "variables" -> js)
    Json
      .fromFields(fields)
      .as[GraphQLRequest]
  }

  def completeRequest[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(request: GraphQLRequest)(implicit ec: ExecutionContext, runtime: Runtime[R]): StandardRoute =
    complete(
      runtime
        .unsafeRunToFuture(executeHttpResponse(interpreter, request))
        .future
    )

  def httpRoute[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit ec: ExecutionContext, runtime: Runtime[R]): Route = {
    import akka.http.scaladsl.server.Directives._

    get {
      parameters((Symbol("query").as[String], Symbol("operationName").?, Symbol("variables").?)) {
        case (query, op, vars) =>
          getGraphQLRequest(query, op, vars)
            .fold(failWith, completeRequest(interpreter))
      }
    } ~
      post {
        entity(as[GraphQLRequest]) {
          completeRequest(interpreter)
        }
      }
  }

  def webSocketRoute[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit ec: ExecutionContext, materializer: Materializer, runtime: Runtime[R]): Route = {
    import io.circe.parser._
    sealed trait Protocol
    case object Complete                 extends Protocol
    final case class Fail(ex: Exception) extends Protocol

    def getFieldAsString(a: ACursor, field: String): Option[String] =
      a.downField(field).success.flatMap(_.value.asString)

    def getIdQueryOpName(cursor: ACursor): (String, Option[String], Option[String]) = {
      val payload = cursor.downField("payload")
      (
        getFieldAsString(cursor, "id").getOrElse(""),
        getFieldAsString(payload, "query"),
        getFieldAsString(payload, "operationName")
      )
    }

    def stringifyForData(id: String, data: ResponseValue, errors: List[E]): String =
      Json
        .obj(
          "id"      -> Json.fromString(id),
          "type"    -> Json.fromString("data"),
          "payload" -> GraphQLResponse(data, errors).asJson
        )
        .noSpaces

    def actorReply(actor: ActorRef): String => Task[Unit] = message => Task(actor ! TextMessage(message))

    def onGraphQLResponse(reply: String => Task[Unit],
                          subscriptions: Ref[Map[String, Fiber[Throwable, Unit]]],
                          id: String,
                          result: GraphQLResponse[E]): RIO[R, Any] =
      result.data match {
        case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
          stream.foreach { item =>
            val response =
              stringifyForData(id, ObjectValue(List(fieldName -> item)), result.errors)
            reply(response)
          }.fork.flatMap(fiber => subscriptions.update(_.updated(id, fiber)))
        case other =>
          val response = stringifyForData(id, other, result.errors)
          ZIO.foreach_(List(response, s"""{"type":"complete","id":"$id"}""")) { reply }
      }

    // http4s would use in place of actor a sendQueue: fs2.concurrent.Queue[RIO[R, *], WebSocketFrame], consider using a queue
    def processMessage(reply: String => Task[Unit],
                       subscriptions: Ref[Map[String, Fiber[Throwable, Unit]]],
                       text: String): RIO[R, Unit] =
      for {
        msgCursor <- Task.fromEither(decode[Json](text)).map(_.hcursor)
        msgType   <- getFieldAsString(msgCursor, "type").fold(???)(Task.succeed)
        _ <- IO.whenCase(msgType) {
              case "connection_init"      => reply("""{"type":"connection_ack"}""")
              case "connection_terminate" => Task.unit
              // reply("""{"type":"terminate"}""")
              // see what can be done instead. http4s does Task.fromEither(WebSocketFrame.Close(1000)) >>= sendQueue.enqueue1
              case "stop" =>
                val id = getFieldAsString(msgCursor, "id")
                subscriptions.get.flatMap(
                  map => IO.whenCase(id.flatMap(map.get)) { case Some(fiber) => fiber.interrupt }
                )
              case "start" =>
                val (id, query, operationName) = getIdQueryOpName(msgCursor)
                Task.whenCase(query) {
                  case Some(query) =>
                    (for {
                      result <- execute(interpreter, GraphQLRequest(query, operationName, None))
                      _      <- onGraphQLResponse(reply, subscriptions, id, result)
                    } yield ()).catchAll { err =>
                      val error = Json
                        .obj(
                          "id"      -> Json.fromString(id),
                          "type"    -> Json.fromString("complete"),
                          "payload" -> Json.fromString(err.toString)
                        )
                        .noSpaces
                      reply(error)
                    }
                }
            }
      } yield ()

    def createSink(reply: String => Task[Unit],
                   subscriptions: UIO[Ref[Map[String, Fiber[Throwable, Unit]]]]): Sink[Message, NotUsed] =
      Flow[Message].map {
        case TextMessage.Strict(message) =>
          val effect = Ref.make(Map.empty[String, Fiber[Throwable, Unit]]) flatMap { subscriptions =>
            processMessage(reply, subscriptions, message)
          }
          runtime.unsafeRunToFuture(effect)
        // http4s does receiver(sendQueue, subscriptions),
        case Streamed(textStream) =>
          // not exercised visibly so far.
          for {
            (message, subscriptions) <- textStream.runFold("")(_ + _) zip runtime
                                         .unsafeRunToFuture(subscriptions)
                                         .future
            _ <- runtime.unsafeRunToFuture(processMessage(reply, subscriptions, message)).future
          } yield ()
        case bm: BinaryMessage =>
          // ignore binary messages but drain content to avoid the stream being clogged
          bm.dataStream.runWith(Sink.ignore)
          Nil
      }.to(Sink.ignore)

    def createSource(): (Source[TextMessage.Strict, NotUsed], ActorRef) = {
      val (ref, publisher) = {
        val completionMatcher: PartialFunction[Any, CompletionStrategy] = {
          case Complete => CompletionStrategy.draining
          case Fail     => CompletionStrategy.draining
        }
        val failureMatcher: PartialFunction[Any, Throwable] = {
          case Fail(ex) => ex
        }
        Source
          .actorRef[TextMessage.Strict](
            completionMatcher,
            failureMatcher,
            bufferSize = Int.MaxValue,
            overflowStrategy = OverflowStrategy.fail
          )
          .toMat(Sink.asPublisher(true))(Keep.both)
          .run()
      }
      (Source.fromPublisher(publisher), ref)
    }

    get {
      val (source, actor)              = createSource
      val subscriptions                = Ref.make(Map.empty[String, Fiber[Throwable, Unit]])
      val sink: Sink[Message, NotUsed] = createSink(actorReply(actor), subscriptions)
      extractUpgradeToWebSocket { upgrade ⇒
        complete(upgrade.handleMessagesWithSinkSource(sink, source, subprotocol = Some("graphql-ws")))
      }
    }
  }

}
