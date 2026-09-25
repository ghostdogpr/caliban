package caliban.gateway.internal

import caliban.InputValue.VariableValue
import caliban.execution.{ ExecutionRequest, Field, RequestPreparation }
import caliban.gateway.PhaseHooks.{ Event, SecurityDirective }
import caliban.gateway.internal.OperationCache.Weighted
import caliban.gateway.internal.OperationPreparation._
import caliban.gateway.internal.composition.ComposedGraph.OverrideLabel
import caliban.gateway.internal.planning.{ OperationCost, OperationPlan, OperationPlanner, OperationSecurity }
import caliban.gateway.{ errorCode, isInclusionDirective, PhaseHooks }
import caliban.parsing.adt.{ Directive, Document }
import caliban.schema.RootType
import caliban.validation.Validator
import caliban._
import zio.{ Cause, Exit, IO, Random, Trace, UIO, ZIO }

import scala.util.control.NoStackTrace

private[gateway] final class OperationPreparation[-R] private (
  rootType: RootType,
  planner: OperationPlanner,
  security: OperationSecurity,
  cost: OperationCost,
  cache: OperationCache[CacheKey, CalibanError, CachedOperation, R],
  maxOperationCost: Option[Long],
  hooks: PhaseHooks[R]
) {

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    for {
      document <- RequestPreparation.parse(query)
      _        <- Validator.validate(document, rootType)
    } yield ()

  def prepare(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, ExecutableOperation] =
    for {
      resolved  <- resolveRequest(request)
      query      = resolved.request.query.getOrElse("")
      operation <- if (resolved.cacheable) withCache(resolved.request, query)
                   else withoutCache(resolved.request, query)
      _         <- enforceCost(operation)
      _         <- authorize(operation)
    } yield operation

  private def resolveRequest(
    request: GraphQLRequest
  )(implicit trace: Trace): ZIO[R, CalibanError, Event.Resolution] = {
    val event = Event.Resolution(request)
    if (!hooks.resolution.enabled) Exit.succeed(event)
    else
      runHook(
        hooks.resolution.runWith(event)(Exit.succeed)(_ => ()),
        ResolutionFailure,
        rejection = { case PhaseHooks.Rejection(message, code) =>
          CalibanError.ExecutionError(message, extensions = errorCode(code))
        }
      )
  }

  private def authorize(operation: ExecutableOperation)(implicit trace: Trace): ZIO[R, CalibanError, Unit] = {
    val requirements = security.requirements(operation.plan)
    if (requirements.exists(_.directives.contains(SecurityDirective.UnsupportedPolicy)))
      ZIO.fail(CalibanError.ValidationError("Operation selects fields guarded by unsupported @policy directives.", ""))
    else if (!hooks.authorization.enabled) ZIO.unit
    else
      runHook(
        hooks.authorization
          .run(Event.Authorization(operation.request, operation.document, operation.executionRequest, requirements))(
            ZIO.unit
          )(_ => ()),
        AuthorizationFailure,
        rejection = { case PhaseHooks.Denial(reason) => CalibanError.ValidationError(reason, "") }
      )
  }

  private def selectCustomOverrides(
    request: GraphQLRequest,
    labels: Set[OverrideLabel]
  )(implicit trace: Trace): ZIO[R, CalibanError, Set[OverrideLabel]] =
    if (labels.isEmpty || !hooks.overrideLabels.enabled) ZIO.succeed(Set.empty)
    else {
      val reachedLabels = labels.map(_.value)
      val hook          = hooks.overrideLabels
        .runWith(Event.OverrideLabels(request, reachedLabels))(Exit.succeed)(_ => ())
        .map(_.active)
      runHook(hook, OverrideLabelResolutionFailure)
        .map(_.intersect(reachedLabels).map(OverrideLabel.apply))
    }

  private def withCache(
    request: GraphQLRequest,
    query: String
  )(implicit trace: Trace): ZIO[R, CalibanError, ExecutableOperation] = Configurator.ref.get.flatMap { config =>
    val preparation = PreparationConfig.from(config)

    def lookup(parse: => IO[CalibanError, Document], activeOverrides: Set[OverrideLabel]) =
      cache
        .getOrCompute(
          CacheKey(query, request.operationName, request.isHttpGetRequest, preparation, activeOverrides)
        )(parse.flatMap(buildCacheEntry(request, query, _, preparation, activeOverrides)))
        .flatMap(bindCachedOperation(request, _, activeOverrides))

    if (planner.hasProgressiveOverrides)
      for {
        document        <- RequestPreparation.parse(query)
        activeOverrides <- selectOverrides(request, document)
        operation       <- lookup(Exit.succeed(document), activeOverrides)
      } yield operation
    else
      lookup(RequestPreparation.parse(query), Set.empty)
  }

  private def buildCacheEntry(
    request: GraphQLRequest,
    query: String,
    document: Document,
    preparation: PreparationConfig,
    activeOverrides: Set[OverrideLabel]
  )(implicit trace: Trace): IO[CalibanError, Weighted[CachedOperation]] =
    for {
      _      <- RequestPreparation.checkIntrospection(document, request.operationName)
      _      <- Validator.validate(document, rootType).unless(preparation.skipValidation)
      cached <- if (hasVariableInclusionDirective(document, request.operationName))
                  ZIO.succeed(
                    Weighted(
                      CachedOperation.DocumentOnly(document),
                      cacheWeight(query, None, request.operationName)
                    )
                  )
                else {
                  val variables = symbolicVariables(document, request.operationName)
                  for {
                    execution <-
                      RequestPreparation.prepareParsed(request, document, variables, rootType, skipValidation = true)
                    plan      <- buildPlan(document, execution, activeOverrides)
                  } yield {
                    val cached: CachedOperation =
                      if (variables.isEmpty && !plan.hasVariableReferences)
                        CachedOperation.Ready(document, execution, plan)
                      else CachedOperation.Planned(document, plan)
                    Weighted(cached, cacheWeight(query, Some(plan), request.operationName))
                  }
                }
    } yield cached

  private def withoutCache(
    request: GraphQLRequest,
    query: String
  )(implicit trace: Trace): ZIO[R, CalibanError, ExecutableOperation] =
    for {
      document        <- RequestPreparation.parse(query)
      activeOverrides <- selectOverrides(request, document)
      operation       <-
        buildOperation(request, document, None)((_, execution) => buildPlan(document, execution, activeOverrides))
    } yield operation

  private def bindCachedOperation(
    request: GraphQLRequest,
    cached: CachedOperation,
    activeOverrides: Set[OverrideLabel]
  )(implicit trace: Trace): IO[CalibanError, ExecutableOperation] =
    cached match {
      case CachedOperation.Ready(document, execution, plan) =>
        Exit.succeed(ExecutableOperation(request, document, execution, plan))
      case CachedOperation.Planned(document, plan)          =>
        buildOperation(request, document, VariableValidation) { (variables, _) =>
          Exit.succeed(plan.bind(variables))
        }
      case CachedOperation.DocumentOnly(document)           =>
        buildOperation(request, document, VariableValidation) { (_, execution) =>
          buildPlan(document, execution, activeOverrides)
        }
    }

  private def buildOperation(
    request: GraphQLRequest,
    document: Document,
    validations: Option[List[Validator.QueryValidation]]
  )(
    getPlan: (Map[String, InputValue], ExecutionRequest) => IO[CalibanError, OperationPlan]
  )(implicit trace: Trace): IO[CalibanError, ExecutableOperation] =
    for {
      variables <- RequestPreparation.coerceVariables(document, request, rootType)
      execution <- RequestPreparation.prepareParsed(
                     request,
                     document,
                     variables,
                     rootType,
                     skipValidation = false,
                     validations = validations
                   )
      plan      <- getPlan(variables, execution)
    } yield ExecutableOperation(request, document, execution, plan)

  private def buildPlan(document: Document, execution: ExecutionRequest, activeOverrides: Set[OverrideLabel])(implicit
    trace: Trace
  ): IO[CalibanError, OperationPlan] =
    ZIO
      .blocking(ZIO.fromEither(planner.plan(document, execution, activeOverrides)))
      .mapError(failure => CalibanError.ValidationError(failure.message, ""))

  private def selectOverrides(
    request: GraphQLRequest,
    document: Document
  )(implicit trace: Trace): ZIO[R, CalibanError, Set[OverrideLabel]] = {
    val overrides = planner.progressiveOverrides(document, request.operationName).toList.sortBy(_._1.value)
    val sampled   = overrides.collect { case (label, Some(percentage)) => label -> percentage }
    val custom    = overrides.collect { case (label, None) => label }.toSet
    for {
      active   <- ZIO.filter(sampled) { case (_, percentage) =>
                    if (percentage <= 0) ZIO.succeed(false)
                    else if (percentage >= 100) ZIO.succeed(true)
                    else Random.nextDouble.map(_ * 100d < percentage.toDouble)
                  }
      selected <- selectCustomOverrides(request, custom)
    } yield active.map(_._1).toSet ++ selected
  }

  private def enforceCost(operation: ExecutableOperation): IO[CalibanError.ValidationError, Unit] =
    maxOperationCost match {
      case Some(maximum) =>
        def reject(message: String, code: String) =
          ZIO.fail(CalibanError.ValidationError(message, "", extensions = errorCode(code)))

        cost.estimate(operation.executionRequest, operation.plan) match {
          case Left(error)                             => reject(error, "COST_QUERY_PARSE_FAILURE")
          case Right(estimated) if estimated > maximum =>
            reject(
              s"Operation cost $estimated exceeds the configured maximum of $maximum.",
              "COST_ESTIMATED_TOO_EXPENSIVE"
            )
          case Right(_)                                => ZIO.unit
        }
      case None          => ZIO.unit
    }

  private def cacheWeight(
    query: String,
    executionPlan: Option[OperationPlan],
    operationName: Option[String]
  ): Long = {
    def fieldWeight(values: List[Field]): Long =
      values.foldLeft(0L)((count, value) =>
        count + 1L + value.arguments.valuesIterator.map(_.toInputString.length.toLong).sum + fieldWeight(value.fields)
      )

    val planWeight = executionPlan
      .fold(0L)(value =>
        fieldWeight(value.fields) +
          value.roots.foldLeft(0L)((count, fetch) =>
            count + fieldWeight(fetch.client) + fieldWeight(fetch.downstream) + fieldWeight(fetch.contextRoots)
          ) +
          value.entities.foldLeft(0L)((count, fetch) => count + fieldWeight(fetch.fields)) +
          value.typenameSelections.size.toLong
      )

    query.length.toLong * 2L + operationName.fold(0)(_.length).toLong + planWeight + 1L
  }

  private def symbolicVariables(document: Document, operationName: Option[String]): Map[String, InputValue] =
    document
      .operationDefinition(operationName)
      .iterator
      .flatMap(_.variableDefinitions.iterator)
      .map(definition => definition.name -> VariableValue(definition.name))
      .toMap

  private def hasVariableInclusionDirective(document: Document, operationName: Option[String]): Boolean = {
    def isVariableInclusionDirective(directive: Directive): Boolean =
      isInclusionDirective(directive) && directive.arguments.values.exists {
        case _: VariableValue => true
        case _                => false
      }

    document.hasDirective(operationName)(isVariableInclusionDirective)
  }
}

private[gateway] object OperationPreparation {

  final case class ExecutableOperation(
    request: GraphQLRequest,
    document: Document,
    executionRequest: ExecutionRequest,
    plan: OperationPlan
  )

  private val VariableValidation: Option[List[Validator.QueryValidation]] = Some(List(Validator.validateVariables))

  def make[R](
    rootType: RootType,
    planner: OperationPlanner,
    security: OperationSecurity,
    cost: OperationCost,
    maxOperationCacheWeight: Long,
    maxOperationCost: Option[Long],
    hooks: PhaseHooks[R]
  )(implicit trace: Trace): UIO[OperationPreparation[R]] =
    OperationCache
      .make[CacheKey, CalibanError, CachedOperation, R](maxOperationCacheWeight, hooks)
      .map(new OperationPreparation(rootType, planner, security, cost, _, maxOperationCost, hooks))

  def isInternalFailure(error: CalibanError): Boolean =
    error match {
      case CalibanError.ExecutionError(_, _, _, Some(_: HookFailure), _) => true
      case _                                                             => false
    }

  private final class HookFailure(cause: Throwable) extends Exception(cause.getMessage, cause) with NoStackTrace

  private val ResolutionFailure              = "Operation resolution failed."
  private val AuthorizationFailure           = "Operation authorization failed."
  private val OverrideLabelResolutionFailure = "Progressive override label resolution failed."

  private def runHook[R, A](
    effect: => ZIO[R, Throwable, A],
    failureMessage: String,
    rejection: PartialFunction[Throwable, CalibanError] = PartialFunction.empty
  )(implicit trace: Trace): ZIO[R, CalibanError, A] =
    ZIO
      .suspendSucceed(effect)
      .mapErrorCause(cause =>
        cause.interruptOption.fold[Cause[CalibanError]](
          cause.failures match {
            case failure :: Nil if cause.defects.isEmpty && rejection.isDefinedAt(failure) =>
              Cause.fail(rejection(failure))
            case _                                                                         =>
              Cause
                .fail(CalibanError.ExecutionError(failureMessage, innerThrowable = Some(new HookFailure(cause.squash))))
          }
        )(fiberId => Cause.interrupt(fiberId))
      )

  private sealed trait CachedOperation

  private object CachedOperation {
    final case class DocumentOnly(document: Document)                                            extends CachedOperation
    final case class Planned(document: Document, plan: OperationPlan)                            extends CachedOperation
    final case class Ready(document: Document, execution: ExecutionRequest, plan: OperationPlan) extends CachedOperation
  }

  private final case class CacheKey(
    query: String,
    operationName: Option[String],
    isHttpGetRequest: Boolean,
    preparation: PreparationConfig,
    activeOverrides: Set[OverrideLabel]
  )

  private final case class PreparationConfig(
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    allowMutationsOverGetRequests: Boolean,
    // Reuse function instances across requests. Fresh lambdas cause misses; rebuilding only the list does not.
    validations: List[Validator.QueryValidation]
  )

  private object PreparationConfig {
    def from(config: Configurator.ExecutionConfiguration): PreparationConfig =
      PreparationConfig(
        config.skipValidation,
        config.enableIntrospection,
        config.allowMutationsOverGetRequests,
        config.validations
      )
  }

}
