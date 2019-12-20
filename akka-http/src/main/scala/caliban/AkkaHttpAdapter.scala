package caliban

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import akka.http.scaladsl.server.Route
import caliban.Value.NullValue
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.syntax._
import zio.{Runtime, URIO}

import scala.concurrent.ExecutionContext

object AkkaHttpAdapter extends FailFastCirceSupport {


  private def execute[R, Q, M, S, E](
                                      interpreter: GraphQL[R, Q, M, S, E],
                                      query: GraphQLRequest
                                    ): URIO[R, GraphQLResponse[E]] =
    interpreter.execute(query.query, query.operationName, query.variables.getOrElse(Map()))

  def makeHttpService[R, Q, M, S, E](interpreter: GraphQL[R, Q, M, S, E])(implicit ec: ExecutionContext, runtime: Runtime[R]): Route = {
    import akka.http.scaladsl.server.Directives._
    post {
        entity(as[GraphQLRequest]) { request =>
          complete({
            runtime.unsafeRunToFuture(
              execute(interpreter, request)
                .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
              .map(gqlResult => HttpResponse(200, entity = HttpEntity(`application/json`, gqlResult.toString())) ))
              .future
          })
        }
      }
    }
}
