package caliban

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.{ HttpEntity, HttpResponse }
import akka.http.scaladsl.server.{ Route, StandardRoute }
import caliban.Value.NullValue
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Decoder.Result
import io.circe.Json
import io.circe.syntax._
import zio.{ Runtime, URIO }

import scala.concurrent.ExecutionContext

object AkkaHttpAdapter extends FailFastCirceSupport {

  private def execute[R, E](
    interpreter: GraphQLInterpreter[R, E],
    query: GraphQLRequest
  ): URIO[R, GraphQLResponse[E]] =
    interpreter.execute(query.query, query.operationName, query.variables.getOrElse(Map()))

  private def executeHttpResponse[R, E](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest
  ): URIO[R, HttpResponse] =
    execute(interpreter, request)
      .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
      .map(gqlResult => HttpResponse(200, entity = HttpEntity(`application/json`, gqlResult.toString())))

  private def getGraphQLRequest(query: String, op: Option[String], vars: Option[String]): Result[GraphQLRequest] = {
    val variablesJs = vars
      .map(Json.fromString(_))
      .filterNot { _.asObject.isEmpty } // we don't want to keep in variables='{}'
    // as building the GraphQLRequest would fail with DecodingFailure([K, V]Map[K, V], List(DownField(variables)))
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

    concat(
      get {
        parameters(('query.as[String], 'operationName.?, 'variables.?)) { (query, op, vars) =>
          val request = getGraphQLRequest(query, op, vars)
          request.fold(decodingFail => failWith(decodingFail), completeRequest)
        }
      },
      post {
        entity(as[GraphQLRequest]) { completeRequest }
      }
    )
  }
}
