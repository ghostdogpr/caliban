package caliban

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.ws.{ BinaryMessage, Message, TextMessage }
import akka.http.scaladsl.model.{ HttpEntity, HttpResponse, StatusCodes }
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{ Route, StandardRoute }
import akka.stream.Materializer
import akka.stream.scaladsl.{ Flow, Sink, Source }
import caliban.Value.NullValue
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Decoder.Result
import io.circe.Json
import io.circe.syntax._
import zio.{ Runtime, URIO }

import scala.concurrent.{ ExecutionContext }
//import scala.concurrent.duration.FiniteDuration
// import zio.Task

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

  def makeHttpServiceM[R, E](
    interpreter: URIO[R, GraphQLInterpreter[R, E]]
  )(implicit ec: ExecutionContext, runtime: Runtime[R]): Route =
    makeHttpService(runtime.unsafeRun(interpreter))

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

  def makeWebSocketServiceM[R, E](
    interpreter: URIO[R, GraphQLInterpreter[R, E]]
  )(implicit ec: ExecutionContext, mat: Materializer, runtime: Runtime[R]): Route = {
    val unsafeInterpreter: GraphQLInterpreter[R, E] = runtime.unsafeRun(interpreter)
    makeWebSocketService(unsafeInterpreter)
  }

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit ec: ExecutionContext, mat: Materializer, runtime: Runtime[R]): Route =
    handleWebSocketMessages(graphQLProtocol)

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
