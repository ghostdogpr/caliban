package caliban

import caliban.Value.NullValue
import io.circe.Json
import io.circe.syntax._
import zio.{ RIO, Runtime, Task, URIO, ZIO }
import io.finch._
import io.finch.circe._
import cats.effect.IO
import zio.interop.catz._
import zio.interop.catz.implicits._

import scala.concurrent.ExecutionContext

object FinchHttpAdapter extends Endpoint.Module[Task] {

  private def execute[R, E](
    interpreter: GraphQLInterpreter[R, E],
    query: GraphQLRequest
  ): URIO[R, GraphQLResponse[E]] =
    interpreter.execute(query.query, query.operationName, query.variables.getOrElse(Map()))

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E]
  )(implicit runtime: Runtime[R]): Endpoint[Task, Json] = {
    val api: Endpoint[Task, Json] = post(jsonBody[GraphQLRequest]) { request: GraphQLRequest =>
      runtime
        .unsafeRunToFuture(
          execute(interpreter, request)
            .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
            .map(gqlResult => Ok(gqlResult))
        )
        .future
    }

    api
  }
}
