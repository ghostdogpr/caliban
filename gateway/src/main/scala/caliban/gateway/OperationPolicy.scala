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
    val executionRequest: ExecutionRequest,
    val securityRequirements: List[SecurityRequirement]
  )

  /**
   * A composed security requirement reached by one selected client field.
   *
   * Every requirement, directive, and runtime type condition is conjunctive. `responsePath` uses client response names,
   * including aliases. `fieldName = None` identifies a type-level directive application.
   */
  final case class SecurityRequirement(
    responsePath: List[String],
    typeName: String,
    fieldName: Option[String],
    runtimeTypeConditions: List[RuntimeTypeCondition],
    directives: List[SecurityDirective]
  )

  /**
   * Limits a requirement to values of `responsePath` whose runtime type is any member of `types`.
   *
   * Multiple runtime type conditions on a requirement are conjunctive, while the types within one condition are
   * disjunctive.
   */
  final case class RuntimeTypeCondition(responsePath: List[String], types: Set[String])

  sealed trait SecurityDirective

  object SecurityDirective {
    case object Authenticated extends SecurityDirective

    /**
     * Scope alternatives. The outer list is disjunctive and every inner list is conjunctive.
     */
    final case class RequiresScopes(scopes: List[List[String]]) extends SecurityDirective

    /**
     * Policy alternatives. The outer list is disjunctive and every inner list is conjunctive.
     */
    final case class Policy(policies: List[List[String]]) extends SecurityDirective
  }

  sealed trait Decision
  case object Allow extends Decision

  /**
   * Rejects an operation. Omit `reason` to use the default public message.
   */
  final case class Reject(reason: String = "Operation rejected by gateway policy.") extends Decision

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
