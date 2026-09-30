package caliban.gateway

import caliban._
import caliban.gateway.internal.execution.SubgraphExecutor
import zio.http.Header
import zio.{ Trace, UIO, URIO, ZIO }
import zio.stream.ZStream

/**
 * An executable gateway created by [[Gateway#interpreter]] or [[Gateway#reloadable]].
 *
 * An interpreter may be shared across fibers and used anywhere a `GraphQLInterpreter` is
 * accepted. Its lifetime is bounded by the scope in which it was built.
 */
trait GatewayInterpreter[-R] extends GraphQLInterpreter[R, CalibanError] {

  /**
   * Executes a request as a stream of responses: one per event for a subscription, a single response otherwise.
   * Setup and resources belong to each consumption, not stream construction.
   */
  def executeStream(request: GraphQLRequest)(implicit
    trace: Trace
  ): ZStream[R, Throwable, GraphQLResponse[CalibanError]] =
    ZStream.unwrap(executeRequest(request).map(SubgraphExecutor.subscriptionResponses))

  /**
   * Executes a request as a stream, with incoming headers available to configured subgraph forwarding policies.
   */
  def executeStream(request: GraphQLRequest, headers: List[Header])(implicit
    trace: Trace
  ): ZStream[R, Throwable, GraphQLResponse[CalibanError]] =
    ZStream.unwrapScoped(IncomingRequestHeaders.locallyScoped(headerValues(headers)).as(executeStream(request)))

  /**
   * Executes a request with incoming headers available to configured subgraph forwarding policies.
   */
  def executeRequest(request: GraphQLRequest, headers: List[Header])(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] =
    IncomingRequestHeaders
      .locally(headerValues(headers))(executeRequest(request))

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

  private def headerValues(headers: List[Header]): List[(String, String)] =
    headers.map(header => RemoteGraphQLConfig.headerName(header) -> header.renderedValue)
}

/**
 * A stable interpreter whose acquired schemas are refreshed within its owning scope.
 */
trait ReloadableGatewayInterpreter[-R] extends GatewayInterpreter[R] {

  /**
   * A bounded diagnostic for the latest failed refresh, cleared after a successful check.
   * Remote messages, schemas, response bodies and exception causes are never retained.
   */
  def lastReloadFailure(implicit trace: Trace): UIO[Option[String]]
}
