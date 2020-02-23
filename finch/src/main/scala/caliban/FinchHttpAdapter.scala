package caliban

import caliban.Value.NullValue
import io.circe.Json
import io.circe.syntax._
import zio.{ Runtime, Task, URIO }
import io.finch._
import io.finch.circe._
import zio.interop.catz._

object FinchHttpAdapter extends Endpoint.Module[Task] {

  /**
   * Create a finch HTTP endpoint, provided you have an interpreter and a runtime
   * https://finagle.github.io/finch/
   *
   * @param interpreter the graphql interpreter
   * @param runtime the zio runtime used to execute the query
   * @tparam R the environment the [[Runtime]] requires
   * @tparam E the error type that the interpreter can fail with
   * @return a Finch endpoint in [[Task]] returning a Json response that is the result of executing incoming graphql
   *         queries against the interpreter
   */
  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit runtime: Runtime[R]): Endpoint[Task, Json] =
    post(jsonBody[GraphQLRequest]) { request: GraphQLRequest =>
      runtime
        .unsafeRunToFuture(
          execute(interpreter, request)
            .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
            .map(gqlResult => Ok(gqlResult))
        )
        .future
    }

  private def execute[R, E](
    interpreter: GraphQLInterpreter[R, E],
    query: GraphQLRequest
  ): URIO[R, GraphQLResponse[E]] =
    interpreter.execute(query.query, query.operationName, query.variables.getOrElse(Map()))
}
