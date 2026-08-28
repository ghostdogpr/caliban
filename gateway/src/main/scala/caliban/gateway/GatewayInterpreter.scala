package caliban.gateway

import caliban.{ CalibanError, GraphQLInterpreter, GraphQLRequest, GraphQLResponse, IncomingRequestHeaders }
import sttp.model.Header
import zio.{ Trace, UIO, URIO, ZIO }

/**
 * An executable gateway created by [[Gateway.interpreter]] or [[Gateway.reloadable]].
 *
 * An interpreter may be shared across fibers and used anywhere a `GraphQLInterpreter` is
 * accepted. Its lifetime is bounded by the scope in which it was built.
 */
trait GatewayInterpreter[-R] extends GraphQLInterpreter[R, CalibanError] {

  /**
   * Returns a point-in-time view of bounded gateway work and operation-cache usage.
   */
  def status(implicit trace: Trace): UIO[GatewayInterpreter.Status]

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

object GatewayInterpreter {

  sealed trait LifecycleState
  object LifecycleState {
    case object Running  extends LifecycleState
    case object Draining extends LifecycleState
    case object Closed   extends LifecycleState
  }

  final case class LifecycleStatus(state: LifecycleState, active: Int, overdue: Int)

  final case class AdmissionStatus(limit: Int, active: Int, waiting: Int)

  final case class OperationCacheStatus(
    maxWeight: Long,
    weight: Long,
    entries: Int,
    hits: Long,
    misses: Long,
    evictions: Long,
    inFlight: Int
  )

  final case class Status(
    lifecycle: LifecycleStatus,
    requests: AdmissionStatus,
    subgraphs: Map[String, AdmissionStatus],
    operationCache: OperationCacheStatus
  )
}
