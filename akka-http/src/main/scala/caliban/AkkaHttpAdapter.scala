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
import zio.{ Fiber, IO, RIO, Ref, Task, ZIO }
//import zio.{ Fiber, RIO, Ref, Runtime, URIO }
import zio.{ Runtime, URIO }
import java.time.Instant
import com.github.ghik.silencer.silent

import scala.concurrent.{ ExecutionContext, Future }

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

  def makeHttpService[R, E](
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

  trait Protocol

  case object Complete           extends Protocol
  case class Data(msg: String)   extends Protocol // not needed perhaps
  case class Fail(ex: Exception) extends Protocol

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit ec: ExecutionContext, materializer: Materializer, runtime: Runtime[R]): Route = {
    import io.circe.parser._

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

    @silent
    def handleMessage(actor: ActorRef, text: String): Unit =
      println(s"handleMessage got ws message '$text' from client")
    // should do something meaningful with Caliban presumably.
    // should also send something back to client
    def stringifyForData(id: String, data: ResponseValue, errors: List[E]): String =
      Json
        .obj(
          "id"      -> Json.fromString(id),
          "type"    -> Json.fromString("data"),
          "payload" -> GraphQLResponse(data, errors).asJson
        )
        .noSpaces

    def onGraphQLResponse(actor: ActorRef,
                          subscriptions: Ref[Map[String, Fiber[Throwable, Unit]]],
                          id: String,
                          result: GraphQLResponse[E]): RIO[R, Any] =
      result.data match {
        case ObjectValue((fieldName, StreamValue(stream)) :: Nil) =>
          stream.foreach { item =>
            val response =
              stringifyForData(id, ObjectValue(List(fieldName -> item)), result.errors)
            Task(actor ! TextMessage(response))
          //sendMessage(sendQueue)(response)
          }.fork.flatMap(fiber => subscriptions.update(_.updated(id, fiber)))
        case other =>
          val response = stringifyForData(id, other, result.errors)
          ZIO.foreach_(List(response, s"""{"type":"complete","id":"$id"}""")) { msg =>
            Task(actor ! TextMessage(msg))
          //sendMessage(sendQueue)(msg)
          }
      }

    @silent
    def processMessage(actor: ActorRef,
                       //sendQueue: fs2.concurrent.Queue[RIO[R, *], WebSocketFrame],
                       subscriptions: Ref[Map[String, Fiber[Throwable, Unit]]])(text: String): RIO[R, Unit] =
      for {
        msgCursor <- Task
                      .fromEither(decode[Json](text))
                      .map(_.hcursor)
                      .fold(e => {
                        println(s"got decode error $e")
                        ???
                      }, identity)
        _       = println(s"processMessage 2 got ws message '$text' from client")
        msgType <- getFieldAsString(msgCursor, "type").fold(???)(Task.succeed)
        _       = println(s"processMessage 3 got ws message '$text' from client")
        _ <- IO.whenCase(msgType) {
              case "connection_init" =>
                val data = TextMessage("""{"type":"connection_ack"}""")
                println(s"processMessage connection_init '$text' from client sending back $data")
                Task(actor ! data)
              //sendQueue.enqueue1(WebSocketFrame.Text("""{"type":"connection_ack"}"""))
              case "connection_terminate" =>
                Task(actor ! TextMessage("""{"type":"terminate"}"""))
              //Task.fromEither(WebSocketFrame.Close(1000)) >>= sendQueue.enqueue1
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
                      _      <- onGraphQLResponse(actor, subscriptions, id, result)
                    } yield ()).catchAll { err =>
                      val error = Json
                        .obj(
                          "id"      -> Json.fromString(id),
                          "type"    -> Json.fromString("complete"),
                          "payload" -> Json.fromString(err.toString)
                        )
                        .noSpaces
                      Task(actor ! TextMessage(error))
                    //sendMessage(sendQueue)(error)
                    }
                }

            }
      } yield ()

    def createFlow(actor: ActorRef): Sink[Message, NotUsed] =
      Flow[Message].map {
        case TextMessage.Strict(message) =>
          println(s"createFlow got message $message")
          val subscriptions = runtime.unsafeRun(Ref.make(Map.empty[String, Fiber[Throwable, Unit]]))
          //                       receiver(sendQueue, subscriptions),
          runtime.unsafeRun(processMessage(actor, subscriptions)(message))
          Nil
        case Streamed(textStream) =>
          val subscriptions = runtime.unsafeRun(Ref.make(Map.empty[String, Fiber[Throwable, Unit]]))
          textStream
            .runFold("")(_ + _)
            .map(message => runtime.unsafeRun(processMessage(actor, subscriptions)(message)))
            .flatMap(Future.successful)
        case bm: BinaryMessage =>
          println(s"createFlow got BinaryMessage ")
          // ignore binary messages but drain content to avoid the stream being clogged
          bm.dataStream.runWith(Sink.ignore)
          Nil
      }.to(Sink.ignore)

    def createSource(): (Source[TextMessage.Strict, NotUsed], ActorRef) = {
      println("createSource")
      val completionMatcher: PartialFunction[Any, CompletionStrategy] = {
        case Complete =>
          println("createSource got Complete")
          CompletionStrategy.draining
        case Fail(ex) =>
          println(s"createSource got Fail ${ex}")
          CompletionStrategy.draining
      }
      val failureMatcher: PartialFunction[Any, Throwable] = {
        case Fail(ex) =>
          println(s"createSource failureMatcher ${ex}")
          ex
      }
      val (ref, publisher) =
        Source
          .actorRef[TextMessage.Strict](
            completionMatcher,
            failureMatcher,
            bufferSize = Int.MaxValue,
            overflowStrategy = OverflowStrategy.fail
          )
          .toMat(Sink.asPublisher(true))(Keep.both)
          .run()

      (Source.fromPublisher(publisher), ref)
    }

    @silent
    def graphQLProtocol: Flow[Message, Message, Any] =
      // this Akka Stream is not lazy I think, it's running immediately. So we should suspend it?
      Flow[Message].mapConcat {
        case tm: TextMessage =>
          //val dur: FiniteDuration = FiniteDuration(1, java.util.concurrent.TimeUnit.SECONDS)
          // val tmTask                 = ZIO.fromFuture(_ => tm.toStrict(dur)) We presumably want to
          // do something similar  with tmTask as what we do on http4s.
          // toy response to show this method is active.
          TextMessage(Source.single("Hello ") ++ tm.textStream ++ Source.single("!")) :: Nil

        case bm: BinaryMessage =>
          bm.dataStream.runWith(Sink.ignore)
          Nil
      }

    get {
      val (source, actor)              = createSource
      val sink: Sink[Message, NotUsed] = createFlow(actor)
      extractUpgradeToWebSocket { upgrade ⇒
        val ts = Instant.now()
        println(s"makeWebSocketService upgraded at $ts")
        complete(upgrade.handleMessagesWithSinkSource(sink, source, subprotocol = Some("graphql-ws")))
      }
    }
  }

}
