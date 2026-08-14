package caliban.gateway

import caliban.{ CalibanError, GraphQLInterpreter, GraphQLRequest }
import zio.{ IO, Trace }

/**
 * An executable gateway created by [[Gateway.build]].
 *
 * A runtime may be shared across fibers and used anywhere a [[caliban.GraphQLInterpreter]] is
 * accepted. Its lifetime is bounded by the scope in which it was built.
 */
trait GatewayRuntime[-R] extends GraphQLInterpreter[R, CalibanError] {

  /**
   * Returns a deterministic semantic description of the executable plan for an operation.
   */
  def explain(request: GraphQLRequest)(implicit trace: Trace): IO[CalibanError, String]

  /**
   * Returns a deterministic semantic description for an operation without variables or extensions.
   */
  def explain(query: String, operationName: Option[String] = None)(implicit trace: Trace): IO[CalibanError, String] =
    explain(GraphQLRequest(query = Some(query), operationName = operationName))
}
