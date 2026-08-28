package caliban.gateway

import caliban.execution.ExecutionRequest
import caliban.parsing.adt.Document
import caliban.GraphQLRequest
import zio.ZIO

/**
 * Allows or rejects an already validated gateway operation without changing its execution.
 */
final class OperationPolicy[-R] private[gateway] (
  private[gateway] val evaluate: OperationPolicy.ValidatedOperation => ZIO[R, Throwable, OperationPolicy.Decision]
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
   * A composed security requirement reached by the selected fields.
   * Every requirement and directive must be satisfied. `fieldName = None` identifies a type-level application.
   */
  final case class SecurityRequirement(
    typeName: String,
    fieldName: Option[String],
    directives: List[SecurityDirective]
  )

  sealed trait SecurityDirective

  object SecurityDirective {
    case object Authenticated extends SecurityDirective

    /** A `@policy` application whose selected operations are always rejected by the gateway. */
    case object UnsupportedPolicy extends SecurityDirective

    /**
     * Scope alternatives. The outer list is disjunctive and every inner list is conjunctive.
     */
    final case class RequiresScopes(scopes: List[List[String]]) extends SecurityDirective

  }

  sealed trait Decision
  case object Allow extends Decision

  /**
   * Rejects an operation. Omit `reason` to use the default public message.
   */
  final case class Reject(reason: String = "Operation rejected by gateway policy.") extends Decision

  def apply[R](evaluate: ValidatedOperation => ZIO[R, Throwable, Decision]): OperationPolicy[R] =
    new OperationPolicy(evaluate)

  /**
   * Enforces `@authenticated` and `@requiresScopes` using claims already verified by the application.
   * `None` is anonymous; `Some` is authenticated even when `scopes` returns an empty set.
   *
   * Reads claims once per protected execution, including cache hits. Operations without security requirements
   * skip the lookup. Scope alternatives use outer OR / inner AND; empty outer or inner lists require only
   * authentication. All selected requirements must pass, including possible runtime branches.
   * Claims lookup failures and scope mapping defects use the gateway's policy failure handling.
   */
  def fromClaims[R, C](readClaims: ZIO[R, Throwable, Option[C]])(scopes: C => Set[String]): OperationPolicy[R] =
    OperationPolicy { operation =>
      if (operation.securityRequirements.isEmpty) ZIO.succeed(Allow)
      else
        readClaims.map {
          case None         => Reject()
          case Some(claims) =>
            val granted = scopes(claims)
            val allowed = operation.securityRequirements.forall(_.directives.forall {
              case SecurityDirective.UnsupportedPolicy            => false
              case SecurityDirective.Authenticated                => true
              case SecurityDirective.RequiresScopes(alternatives) =>
                alternatives.isEmpty || alternatives.exists(_.forall(granted.contains))
            })
            if (allowed) Allow else Reject()
        }
    }
}
