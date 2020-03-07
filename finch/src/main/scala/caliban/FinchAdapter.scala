package caliban

import scala.concurrent.{ Future, Promise }
import caliban.Value.NullValue
import io.circe.Json
import io.circe.syntax._
import io.finch._
import io.finch.circe._
import zio.interop.catz._
import zio.{ Exit, Runtime, Task, URIO, ZIO }

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
      unsafeRunToFuture(
        execute(interpreter, request, skipValidation)
          .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
          .map(gqlResult => Ok(gqlResult))
      )
    }

  private def unsafeRunToFuture[R, E <: Throwable, A](zio: ZIO[R, E, A])(implicit runtime: Runtime[R]): Future[A] = {
    val p = Promise[A]()
    runtime.unsafeRunAsync(zio) {
      case Exit.Success(value) => p.success(value)
      case Exit.Failure(cause) => p.failure(cause.squashTrace)
    }
    p.future
  }

  private def execute[R, E](
    interpreter: GraphQLInterpreter[R, E],
    query: GraphQLRequest,
    skipValidation: Boolean
  ): URIO[R, GraphQLResponse[E]] =
    interpreter.execute(query.query, query.operationName, query.variables.getOrElse(Map()))
}
