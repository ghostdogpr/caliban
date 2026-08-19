package caliban.gateway

import caliban.{ CalibanError, GraphQLInterpreter, GraphQLRequest, GraphQLResponse }
import sttp.model.Header
import zio.{ FiberRef, Trace, UIO, URIO, Unsafe, ZIO }

/**
 * An executable gateway created by [[Gateway.build]].
 *
 * A runtime may be shared across fibers and used anywhere a [[caliban.GraphQLInterpreter]] is
 * accepted. Its lifetime is bounded by the scope in which it was built.
 */
trait GatewayRuntime[-R] extends GraphQLInterpreter[R, CalibanError] {

  /**
   * Returns a point-in-time view of bounded runtime work and operation-cache usage.
   */
  def status(implicit trace: Trace): UIO[GatewayRuntime.Status]

  /**
   * Executes a request with incoming headers available to configured source forwarding policies.
   */
  def executeRequest(request: GraphQLRequest, headers: List[Header])(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] =
    IncomingRequestHeaders.locally(headers)(executeRequest(request))

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

object GatewayRuntime {

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
    requests: AdmissionStatus,
    sources: Map[String, AdmissionStatus],
    operationCache: OperationCacheStatus
  )
}

private[gateway] object IncomingRequestHeaders {
  private val current: FiberRef[List[Header]] =
    Unsafe.unsafe(implicit unsafe => FiberRef.unsafe.make(Nil))

  def get: UIO[List[Header]] = current.get

  def locally[R, E, A](headers: List[Header])(effect: ZIO[R, E, A]): ZIO[R, E, A] =
    current.locally(headers)(effect)
}
