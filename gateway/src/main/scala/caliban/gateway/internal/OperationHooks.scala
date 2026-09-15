package caliban.gateway.internal

import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import caliban.execution.ExecutionRequest
import caliban.gateway.OperationPolicy._
import caliban.gateway.PhaseHooks.Event
import caliban.gateway.internal.composition.ComposedGraph.OverrideLabel
import caliban.gateway.internal.planning.OperationPlan
import caliban.gateway.{ OperationPolicy, OperationResolver, PhaseHooks }
import caliban.parsing.adt.Document
import caliban.{ CalibanError, GraphQLRequest }
import zio.{ Cause, Exit, Trace, ZIO }

private[gateway] final class OperationHooks[-R](
  securityRequirements: OperationPlan => List[SecurityRequirement],
  resolver: Option[OperationResolver[R]],
  policy: Option[OperationPolicy[R]],
  hooks: PhaseHooks[R]
) {

  val cacheable: Boolean = resolver.forall(_.cacheable)

  private[gateway] def resolve(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, GraphQLRequest] =
    resolver match {
      case Some(resolver) =>
        OperationHooks
          .run(resolver.resolve(request), OperationHooks.ResolutionFailure, allowRejection = true)
          .map(query => request.copy(query = Some(query)))
      case None           => ZIO.succeed(request)
    }

  private[gateway] def evaluatePolicy(
    request: GraphQLRequest,
    document: Document,
    executionRequest: ExecutionRequest,
    plan: OperationPlan
  )(implicit trace: Trace): ZIO[R, CalibanError, Unit] = {
    val requirements = securityRequirements(plan)
    if (requirements.exists(_.directives.contains(SecurityDirective.UnsupportedPolicy)))
      ZIO.fail(CalibanError.ValidationError("Operation selects fields guarded by unsupported @policy directives.", ""))
    else
      policy match {
        case Some(policy) =>
          val operation = new ValidatedOperation(
            request,
            document,
            executionRequest,
            requirements
          )
          OperationHooks
            .run(policy.evaluate(operation), OperationHooks.PolicyFailure)
            .flatMap {
              case Allow          => ZIO.unit
              case Reject(reason) => ZIO.fail(CalibanError.ValidationError(reason, ""))
            }
        case None         => ZIO.unit
      }
  }

  private[gateway] def resolveOverrideLabels(
    request: GraphQLRequest,
    labels: Set[OverrideLabel]
  )(implicit trace: Trace): ZIO[R, CalibanError, Set[OverrideLabel]] =
    if (labels.isEmpty) ZIO.succeed(Set.empty)
    else {
      val unresolved = labels.map(_.value)
      val hook       =
        if (!hooks.overrideLabels.enabled) Exit.succeed(Set.empty[String])
        else
          hooks.overrideLabels
            .runWith(Event.OverrideLabels(request, unresolved))(Exit.succeed)(_ => ())
            .map(_.active)
      OperationHooks
        .run(hook, OperationHooks.OverrideLabelResolutionFailure)
        .map(_.intersect(unresolved).map(OverrideLabel.apply))
    }
}

private[gateway] object OperationHooks {
  private val ResolutionFailure              = "Operation resolution failed."
  private val PolicyFailure                  = "Operation policy failed."
  private val OverrideLabelResolutionFailure = "Progressive override label resolution failed."

  def isInternalFailure(error: CalibanError): Boolean =
    error match {
      case CalibanError.ExecutionError(message, _, _, Some(_), _) =>
        message == ResolutionFailure || message == PolicyFailure || message == OverrideLabelResolutionFailure
      case _                                                      => false
    }

  private def run[R, A](
    effect: => ZIO[R, Throwable, A],
    failureMessage: String,
    allowRejection: Boolean = false
  )(implicit trace: Trace): ZIO[R, CalibanError, A] =
    ZIO
      .suspendSucceed(effect)
      .mapErrorCause(cause =>
        cause.interruptOption.fold[Cause[CalibanError]](
          cause.failures match {
            case (rejection: OperationResolver.Rejection) :: Nil if allowRejection && cause.defects.isEmpty =>
              Cause.fail(
                CalibanError.ExecutionError(
                  rejection.message,
                  extensions = Some(ObjectValue(List("code" -> StringValue(rejection.code))))
                )
              )
            case _                                                                                          =>
              Cause.fail(CalibanError.ExecutionError(failureMessage, innerThrowable = Some(cause.squash)))
          }
        )(fiberId => Cause.interrupt(fiberId))
      )
}
