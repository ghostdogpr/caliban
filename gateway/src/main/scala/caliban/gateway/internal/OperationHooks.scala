package caliban.gateway.internal

import caliban.execution.ExecutionRequest
import caliban.gateway.OperationHookCacheBehavior.{ Bypass => BypassCache, Stable }
import caliban.gateway.OperationPolicy.{ Allow, Reject, SecurityRequirement, ValidatedOperation }
import caliban.gateway.{ OperationPolicy, OperationResolver }
import caliban.parsing.adt.Document
import caliban.{ CalibanError, GraphQLRequest }
import zio.{ Cause, Trace, ZIO }

private[gateway] final class OperationHooks[-R](
  securityRequirements: ExecutionRequest => List[SecurityRequirement],
  resolver: Option[OperationResolver[R]],
  policy: Option[OperationPolicy[R]]
) {

  val cacheDirective: OperationCacheDirective =
    (resolver.map(_.cacheBehavior), policy.map(_.cacheBehavior)) match {
      case (Some(BypassCache), _) | (_, Some(BypassCache)) => OperationCacheDirective.Bypass
      case (resolverBehavior, policyBehavior)              =>
        OperationCacheDirective.Cacheable(
          resolverBehavior.collect { case Stable(value) => value },
          policyBehavior.collect { case Stable(value) => value }
        )
    }

  private[gateway] def resolve(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, GraphQLRequest] =
    resolver match {
      case Some(resolver) =>
        OperationHooks
          .run(resolver.resolve(request), OperationHooks.ResolutionFailure)
          .map(query => request.copy(query = Some(query)))
      case None           => ZIO.succeed(request)
    }

  private[gateway] def evaluatePolicy(
    request: GraphQLRequest,
    document: Document,
    executionRequest: ExecutionRequest
  )(implicit trace: Trace): ZIO[R, CalibanError, Unit] =
    policy match {
      case Some(policy) =>
        val operation = new ValidatedOperation(
          request,
          document,
          executionRequest,
          securityRequirements(executionRequest)
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

private[gateway] object OperationHooks {
  private val ResolutionFailure = "Operation resolution failed."
  private val PolicyFailure     = "Operation policy failed."

  private def run[R, A](
    effect: => ZIO[R, Throwable, A],
    failureMessage: String
  )(implicit trace: Trace): ZIO[R, CalibanError, A] =
    ZIO
      .suspendSucceed(effect)
      .foldCauseZIO(
        cause =>
          cause.interruptOption.fold(
            ZIO.fail(CalibanError.ExecutionError(failureMessage, innerThrowable = Some(cause.squash)))
          )(fiberId => ZIO.failCause(Cause.interrupt(fiberId))),
        ZIO.succeed(_)
      )
}

private[gateway] sealed trait OperationCacheDirective

private[gateway] object OperationCacheDirective {
  final case class Cacheable(
    resolver: Option[String],
    policy: Option[String]
  ) extends OperationCacheDirective
  case object Bypass extends OperationCacheDirective
}
