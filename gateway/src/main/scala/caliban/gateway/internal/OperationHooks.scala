package caliban.gateway.internal

import caliban.execution.{ ExecutionRequest, RequestPreparation }
import caliban.gateway.OperationHookCacheBehavior.{ Bypass => BypassCache, Stable }
import caliban.gateway.OperationPolicy.{ Allow, Reject, ValidatedOperation }
import caliban.gateway.{ OperationPolicy, OperationResolver }
import caliban.parsing.adt.Document
import caliban.schema.RootType
import caliban.{ CalibanError, GraphQLRequest }
import zio.{ Cause, Trace, ZIO }

private[gateway] final class OperationHooks[-R](
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

  def prepare(request: GraphQLRequest, rootType: RootType)(implicit
    trace: Trace
  ): ZIO[R, CalibanError, OperationHooks.Prepared] =
    for {
      resolved <- resolve(request)
      prepared <- RequestPreparation.prepareWithIntrospection(resolved, rootType)
      _        <- evaluatePolicy(resolved, prepared)
    } yield OperationHooks.Prepared(
      resolved,
      prepared.document,
      prepared.executionRequest,
      cacheDirective
    )

  private def resolve(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, GraphQLRequest] =
    resolver match {
      case Some(resolver) =>
        OperationHooks
          .run(resolver.resolve(request), OperationHooks.ResolutionFailure)
          .flatMap(query =>
            if (query eq null) ZIO.fail(OperationHooks.ResolutionFailure)
            else ZIO.succeed(request.copy(query = Some(query)))
          )
      case None           => ZIO.succeed(request)
    }

  private def evaluatePolicy(
    request: GraphQLRequest,
    prepared: RequestPreparation.Prepared
  )(implicit trace: Trace): ZIO[R, CalibanError, Unit] =
    policy match {
      case Some(policy) =>
        val operation = new ValidatedOperation(request, prepared.document, prepared.executionRequest)
        OperationHooks
          .run(policy.evaluate(operation), OperationHooks.PolicyFailure)
          .flatMap {
            case Allow  => ZIO.unit
            case Reject => ZIO.fail(OperationHooks.PolicyRejection)
            case null   => ZIO.fail(OperationHooks.PolicyFailure)
          }
      case None         => ZIO.unit
    }
}

private[gateway] object OperationHooks {
  final case class Prepared(
    request: GraphQLRequest,
    document: Document,
    executionRequest: ExecutionRequest,
    cacheDirective: OperationCacheDirective
  )

  private val ResolutionFailure = CalibanError.ValidationError("Operation resolution failed.", "")
  private val PolicyFailure     = CalibanError.ValidationError("Operation policy failed.", "")
  private val PolicyRejection   = CalibanError.ValidationError("Operation rejected by gateway policy.", "")

  private def run[R, A](
    effect: => ZIO[R, Throwable, A],
    failure: CalibanError
  )(implicit trace: Trace): ZIO[R, CalibanError, A] =
    ZIO
      .suspendSucceed(effect)
      .foldCauseZIO(
        cause => cause.interruptOption.fold(ZIO.fail(failure))(fiberId => ZIO.failCause(Cause.interrupt(fiberId))),
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
