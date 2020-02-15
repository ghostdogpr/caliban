package caliban

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
import io.circe.Json
import io.circe.syntax._
import zio.{ Runtime, URIO }

import scala.concurrent.{ ExecutionContext, Future }

object AkkaHttpAdapter extends FailFastCirceSupport {

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
        entity(as[GraphQLRequest]) { completeRequest(interpreter) }
      }
  }

  trait Protocol
  case object Complete           extends Protocol
  case class Fail(ex: Exception) extends Protocol

  def handleMessage(message: String, actor: ActorRef): Unit =
    println(s"handleMessage got ws message '$message' from client")
  // should do something meaningful with Caliban presumably.
  // should also send something back to client

  def createFlow(actor: ActorRef)(implicit ex: ExecutionContext, materializer: Materializer): Sink[Message, NotUsed] =
    Flow[Message].map {
      case TextMessage.Strict(message) =>
        println(s"createFlow got message $message")
        handleMessage(message, actor)
        Nil
      case Streamed(textStream) =>
        textStream.runFold("")(_ + _).map(message => handleMessage(message, actor)).flatMap(Future.successful)
      case bm: BinaryMessage =>
        println(s"createFlow got BinaryMessage ")
        // ignore binary messages but drain content to avoid the stream being clogged
        bm.dataStream.runWith(Sink.ignore)
        Nil
    }.to(Sink.ignore)

  def createSource(implicit ex: ExecutionContext,
                   materializer: Materializer): (Source[TextMessage.Strict, NotUsed], ActorRef) = {
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

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit ec: ExecutionContext, materializer: Materializer, runtime: Runtime[R]): Route =
    get {
      val (source, actor)              = createSource
      val sink: Sink[Message, NotUsed] = createFlow(actor)
      extractUpgradeToWebSocket { upgrade ⇒
        complete(upgrade.handleMessagesWithSinkSource(sink, source, subprotocol = Some("graphql-ws")))
      }
    }

  def graphQLProtocol(implicit ec: ExecutionContext, mat: Materializer): Flow[Message, Message, Any] =
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

}
