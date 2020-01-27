package caliban

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.{ HttpEntity, HttpResponse, StatusCodes }
import akka.http.scaladsl.server.{ Route, StandardRoute }
import caliban.Value.NullValue
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Decoder.Result
import io.circe.Json
import io.circe.syntax._
import zio.{ Runtime, URIO }

import scala.concurrent.ExecutionContext

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

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit ec: ExecutionContext, runtime: Runtime[R]): Route = {
    import akka.http.scaladsl.server.Directives._

    def completeRequest(request: GraphQLRequest)(implicit ec: ExecutionContext, runtime: Runtime[R]): StandardRoute =
      complete(
        runtime
          .unsafeRunToFuture(executeHttpResponse(interpreter, request))
          .future
      )

    get {
      parameters(('query.as[String], 'operationName.?, 'variables.?)) {
        case (query, op, vars) =>
          getGraphQLRequest(query, op, vars)
            .fold(decodingFail => failWith(decodingFail), completeRequest)
      }
    } ~
      post {
        entity(as[GraphQLRequest]) { completeRequest }
      }
  }
}
