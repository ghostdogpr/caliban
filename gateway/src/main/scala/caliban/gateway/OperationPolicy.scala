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
  private[gateway] val supportsNamedPolicies: Boolean
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
    final case class Policy(policies: List[List[String]]) extends SecurityDirective {
      private[gateway] def isAuthenticationOnly: Boolean =
        policies.isEmpty || policies.exists(_.isEmpty)
    }
  }

  sealed trait Decision
  case object Allow extends Decision

  /**
   * Rejects an operation. Omit `reason` to use the default public message.
   */
  final case class Reject(reason: String = "Operation rejected by gateway policy.") extends Decision

  def apply[R](evaluate: ValidatedOperation => ZIO[R, Throwable, Decision]): OperationPolicy[R] =
    new OperationPolicy(evaluate, supportsNamedPolicies = true)

  /**
   * Enforces `@authenticated` and `@requiresScopes` using claims already verified by the application.
   * `None` is anonymous; `Some` is authenticated even when `scopes` returns an empty set.
   *
   * Reads claims once per protected execution, including cache hits. Operations without security requirements
   * skip the lookup. Scope alternatives use outer OR / inner AND; empty outer or inner lists require only
   * authentication. Nontrivial `@policy` expressions require the overload accepting a policy handler or a custom
   * policy; otherwise the gateway refuses to build. Authentication-only policy expressions need no handler.
   *
   * Checks all requirements, even those guarded by runtime type conditions: response types are not known before execution.
   * Any unmet requirement rejects the whole operation with the default public message. Claims lookup failures
   * and scope mapping defects are handled by the gateway's existing policy failure handling.
   */
  def fromClaims[R, C](readClaims: ZIO[R, Throwable, Option[C]])(scopes: C => Set[String]): OperationPolicy[R] =
    claimsPolicy(readClaims, scopes, None)

  /**
   * Also enforces `@policy` by passing verified claims and each policy name to `policyHandler`.
   * The handler should return `false` for unrecognized names. Alternatives use outer OR / inner AND,
   * evaluated sequentially with short-circuiting. An empty outer list or any empty alternative requires only
   * authentication and skips the handler, as with scopes.
   * Handler failures reject the operation through the gateway's generic policy failure handling.
   */
  def fromClaims[R, C](
    readClaims: ZIO[R, Throwable, Option[C]],
    policyHandler: (C, String) => ZIO[R, Throwable, Boolean]
  )(scopes: C => Set[String]): OperationPolicy[R] =
    claimsPolicy(readClaims, scopes, Some(policyHandler))

  private def claimsPolicy[R, C](
    readClaims: ZIO[R, Throwable, Option[C]],
    scopes: C => Set[String],
    policyHandler: Option[(C, String) => ZIO[R, Throwable, Boolean]]
  ): OperationPolicy[R] = {
    val authorize: (C, Set[String], List[SecurityRequirement]) => ZIO[R, Throwable, Boolean] =
      policyHandler match {
        case None          =>
          (_, granted, requirements) =>
            ZIO.succeed(requirements.forall(_.directives.forall(allowsWithoutHandler(_, granted))))
        case Some(handler) =>
          (claims, granted, requirements) =>
            ZIO.forall(requirements) { requirement =>
              ZIO.forall(requirement.directives) {
                case policy: SecurityDirective.Policy if !policy.isAuthenticationOnly =>
                  ZIO.exists(policy.policies)(names => ZIO.forall(names)(name => handler(claims, name)))
                case directive                                                        =>
                  ZIO.succeed(allowsWithoutHandler(directive, granted))
              }
            }
      }

    new OperationPolicy(
      operation =>
        if (operation.securityRequirements.isEmpty) ZIO.succeed(Allow)
        else
          readClaims.flatMap {
            case None         => ZIO.succeed(Reject())
            case Some(claims) =>
              authorize(claims, scopes(claims), operation.securityRequirements)
                .map(allowed => if (allowed) Allow else Reject())
          },
      supportsNamedPolicies = policyHandler.isDefined
    )
  }

  private def allowsWithoutHandler(directive: SecurityDirective, granted: Set[String]): Boolean =
    directive match {
      case SecurityDirective.Authenticated                => true
      case SecurityDirective.RequiresScopes(alternatives) =>
        alternatives.isEmpty || alternatives.exists(_.forall(granted.contains))
      case policy: SecurityDirective.Policy               => policy.isAuthenticationOnly
    }
}
