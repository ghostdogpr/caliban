package caliban

import caliban.Value.NullValue
import io.circe.Json
import io.circe.syntax._
import io.finch._
import io.finch.circe._
import zio.interop.catz._
import zio.{ Runtime, Task }

object FinchAdapter extends Endpoint.Module[Task] {

  /**
   * Create a finch HTTP endpoint, provided you have an interpreter and a runtime
   * https://finagle.github.io/finch/
   *
   * @param interpreter the graphql interpreter
   * @param runtime the zio runtime used to execute the query
   * @tparam R the environment the `Runtime` requires
   * @tparam E the error type that the interpreter can fail with
   * @return a Finch endpoint in `Task` returning a Json response that is the result of executing incoming graphql
   *         queries against the interpreter
   */
  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false
  )(implicit runtime: Runtime[R]): Endpoint[Task, Json] =
    post(jsonBody[GraphQLRequest]) { request: GraphQLRequest =>
      runtime
        .unsafeRunToFuture(
          interpreter
            .executeRequest(request, skipValidation)
            .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
            .map(gqlResult => Ok(gqlResult))
        )
        .future
    }
}
