package caliban.gateway

import caliban._
import caliban.GraphQLResponseContext.ServerFailure
import caliban.gateway.internal.GatewayInterpreterImpl.requestShutdownResponse
import caliban.gateway.internal.execution.SubgraphExecutor
import sttp.model.Header
import zio.{ Trace, URIO, ZIO }
import zio.stream.ZStream

/**
 * An executable gateway created by [[Gateway.interpreter]] or [[Gateway.reloadable]].
 *
 * An interpreter may be shared across fibers and used anywhere a `GraphQLInterpreter` is
 * accepted. Its lifetime is bounded by the scope in which it was built.
 */
trait GatewayInterpreter[-R] extends GraphQLInterpreter[R, CalibanError] {

  private[gateway] def shutdownResponse(implicit trace: Trace): URIO[Any, GraphQLResponse[CalibanError]] =
    GraphQLResponseContext
      .markServerError(ServerFailure.Unavailable)
      .as(requestShutdownResponse)

  /**
   * Setup and resources belong to each consumption, not stream construction.
   */
  def executeStream(request: GraphQLRequest)(implicit
    trace: Trace
  ): ZStream[R, Throwable, GraphQLResponse[CalibanError]] =
    ZStream.unwrap(executeRequest(request).map(SubgraphExecutor.responses))

  def executeStream(request: GraphQLRequest, headers: List[Header])(implicit
    trace: Trace
  ): ZStream[R, Throwable, GraphQLResponse[CalibanError]] =
    ZStream.unwrapScoped(
      IncomingRequestHeaders
        .locallyScoped(headers.map(header => header.name -> header.value))
        .as(executeStream(request))
    )

  /**
   * Executes a request with incoming headers available to configured subgraph forwarding policies.
   */
  def executeRequest(request: GraphQLRequest, headers: List[Header])(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] =
    IncomingRequestHeaders.locally(headers.map(header => header.name -> header.value))(executeRequest(request))

  /**
   * Returns a deterministic semantic description of the executable plan for an operation.
   */
  def explain(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, String]

  /**
   * Returns a deterministic semantic description for an operation without variables or extensions.
   */
  def explain(query: String, operationName: Option[String] = None)(implicit
    trace: Trace
  ): ZIO[R, CalibanError, String] =
    explain(GraphQLRequest(query = Some(query), operationName = operationName))
}
