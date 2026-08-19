package caliban.gateway

import caliban.execution.ExecutionRequest
import caliban.parsing.adt.Document
import caliban.GraphQLRequest
import zio.ZIO

/**
 * Allows or rejects an already validated gateway operation without changing its execution.
 */
final class OperationPolicy[-R] private[gateway] (
  private[gateway] val evaluate: OperationPolicy.ValidatedOperation => ZIO[R, Throwable, OperationPolicy.Decision],
  private[gateway] val cacheBehavior: OperationHookCacheBehavior
)

object OperationPolicy {

  /**
   * Immutable input supplied to an operation policy after Caliban validation and variable coercion.
   */
  final class ValidatedOperation private[gateway] (
    val request: GraphQLRequest,
    val document: Document,
    val executionRequest: ExecutionRequest
  )

  sealed trait Decision
  case object Allow  extends Decision
  case object Reject extends Decision

  /**
   * Creates a policy whose stable discriminator can participate in operation cache keys.
   */
  def stable[R](
    discriminator: String
  )(evaluate: ValidatedOperation => ZIO[R, Throwable, Decision]): OperationPolicy[R] =
    new OperationPolicy(evaluate, OperationHookCacheBehavior.Stable(discriminator))

  /**
   * Creates a policy whose operations must bypass operation caches.
   */
  def uncached[R](evaluate: ValidatedOperation => ZIO[R, Throwable, Decision]): OperationPolicy[R] =
    new OperationPolicy(evaluate, OperationHookCacheBehavior.Bypass)
}
