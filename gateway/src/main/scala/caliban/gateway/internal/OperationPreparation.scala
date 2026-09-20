package caliban.gateway.internal

import caliban.InputValue.VariableValue
import caliban.execution.{ ExecutionRequest, Field, RequestPreparation }
import caliban.gateway.PhaseHooks.{ Event, SecurityDirective, SecurityRequirement }
import caliban.gateway.internal.OperationCache.Weighted
import caliban.gateway.internal.OperationPreparation._
import caliban.gateway.internal.composition.ComposedGraph.OverrideLabel
import caliban.gateway.internal.execution.PreparedPlan
import caliban.gateway.internal.planning.{ OperationPlan, OperationPlanner }
import caliban.gateway.{ errorCode, isInclusionDirective, GatewayConfig, PhaseHooks }
import caliban.parsing.adt.{ Directive, Document }
import caliban.schema.RootType
import caliban.validation.Validator
import caliban._
import zio.{ Cause, Exit, IO, Random, Trace, UIO, ZIO }

private[gateway] final class OperationPreparation[-R] private (
  rootType: RootType,
  planner: OperationPlanner,
  securityRequirements: OperationPlan => List[SecurityRequirement],
  cache: OperationCache[CacheKey, CalibanError, CachedOperation, R],
  maxOperationCost: Option[Long],
  hooks: PhaseHooks[R],
  estimateCost: (ExecutionRequest, OperationPlan) => Either[String, Long]
) {

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    for {
      document <- RequestPreparation.parse(query)
      _        <- Validator.validate(document, rootType)
    } yield ()

  def prepare(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, Prepared] =
    for {
      resolved   <- resolve(request)
      query       = resolved.request.query.getOrElse("")
      config     <- Configurator.ref.get
      preparation = PreparationConfig.from(config)
      prepared   <- if (resolved.cacheable) prepareCached(resolved.request, query, preparation)
                    else prepareUncached(resolved.request, query)
      _          <- enforceCost(prepared)
      _          <- authorize(
                      resolved.request,
                      prepared.document,
                      prepared.executionRequest,
                      prepared.plan.plan
                    )
    } yield prepared

  private def resolve(
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

  private def authorize(
    request: GraphQLRequest,
    document: Document,
    executionRequest: ExecutionRequest,
    plan: OperationPlan
  )(implicit trace: Trace): ZIO[R, CalibanError, Unit] = {
    val requirements = securityRequirements(plan)
    if (requirements.exists(_.directives.contains(SecurityDirective.UnsupportedPolicy)))
      ZIO.fail(CalibanError.ValidationError("Operation selects fields guarded by unsupported @policy directives.", ""))
    else if (!hooks.authorization.enabled) ZIO.unit
    else
      runHook(
        hooks.authorization
          .run(Event.Authorization(request, document, executionRequest, requirements))(ZIO.unit)(_ => ()),
        PolicyFailure,
        rejection = { case PhaseHooks.Denial(reason) => CalibanError.ValidationError(reason, "") }
      )
  }

  private def resolveOverrideLabels(
    request: GraphQLRequest,
    labels: Set[OverrideLabel]
  )(implicit trace: Trace): ZIO[R, CalibanError, Set[OverrideLabel]] =
    if (labels.isEmpty || !hooks.overrideLabels.enabled) ZIO.succeed(Set.empty)
    else {
      val unresolved = labels.map(_.value)
      val hook       = hooks.overrideLabels
        .runWith(Event.OverrideLabels(request, unresolved))(Exit.succeed)(_ => ())
        .map(_.active)
      runHook(hook, OverrideLabelResolutionFailure)
        .map(_.intersect(unresolved).map(OverrideLabel.apply))
    }

  private def prepareCached(
    request: GraphQLRequest,
    query: String,
    preparation: PreparationConfig
  )(implicit trace: Trace): ZIO[R, CalibanError, Prepared] = {
    def cached(parse: => IO[CalibanError, Document], activeOverrides: Set[OverrideLabel]) =
      cache
        .getOrCompute(
          CacheKey(query, request.operationName, request.isHttpGetRequest, preparation, activeOverrides)
        )(parse.flatMap(computeCached(request, _, preparation, activeOverrides)))
        .flatMap(materialize(request, _, activeOverrides))

    if (planner.hasProgressiveOverrides)
      for {
        document        <- RequestPreparation.parse(query)
        activeOverrides <- resolveProgressiveOverrides(request, document)
        prepared        <- cached(Exit.succeed(document), activeOverrides)
      } yield prepared
    else
      cached(RequestPreparation.parse(query), Set.empty)
  }

  private def computeCached(
    request: GraphQLRequest,
    document: Document,
    preparation: PreparationConfig,
    activeOverrides: Set[OverrideLabel]
  )(implicit trace: Trace): IO[CalibanError, Weighted[CachedOperation]] =
    for {
      _        <- RequestPreparation.checkIntrospection(document, request.operationName)
      _        <- Validator.validate(document, rootType).unless(preparation.skipValidation)
      variables = symbolicVariables(document)
      planned  <-
        if (hasVariableCondition(document, request.operationName)) ZIO.none
        else
          for {
            execution <- RequestPreparation.prepareParsed(request, document, variables, rootType, skipValidation = true)
            plan      <- preparePlan(document, execution, activeOverrides)
          } yield Some((execution, plan))
    } yield {
      val cached = planned match {
        case None                    => CachedOperation.DocumentOnly(document)
        case Some((execution, plan)) =>
          if (variables.isEmpty && !plan.hasVariableReferences) CachedOperation.Ready(document, execution, plan)
          else CachedOperation.Planned(document, plan)
      }
      Weighted(cached, operationWeight(request.query.getOrElse(""), planned.map(_._2), request.operationName))
    }

  private def prepareUncached(
    request: GraphQLRequest,
    query: String
  )(implicit trace: Trace): ZIO[R, CalibanError, Prepared] =
    for {
      document        <- RequestPreparation.parse(query)
      activeOverrides <- resolveProgressiveOverrides(request, document)
      prepared        <-
        prepareOperation(request, document, None)((_, execution) => preparePlan(document, execution, activeOverrides))
    } yield prepared

  private def materialize(
    request: GraphQLRequest,
    cached: CachedOperation,
    activeOverrides: Set[OverrideLabel]
  )(implicit trace: Trace): IO[CalibanError, Prepared] =
    cached match {
      case CachedOperation.Ready(document, execution, plan) =>
        Exit.succeed(Prepared(request, document, execution, plan))
      case CachedOperation.Planned(document, plan)          =>
        prepareOperation(request, document, VariableValidation) { (variables, _) =>
          Exit.succeed(if (plan.hasVariableReferences) plan.bind(variables) else plan)
        }
      case CachedOperation.DocumentOnly(document)           =>
        prepareOperation(request, document, VariableValidation) { (_, execution) =>
          preparePlan(document, execution, activeOverrides)
        }
    }

  private def prepareOperation(
    request: GraphQLRequest,
    document: Document,
    validations: Option[List[Validator.QueryValidation]]
  )(
    plan: (Map[String, InputValue], ExecutionRequest) => IO[CalibanError, PreparedPlan]
  )(implicit trace: Trace): IO[CalibanError, Prepared] =
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
      prepared  <- plan(variables, execution)
    } yield Prepared(request, document, execution, prepared)

  private def preparePlan(
    document: Document,
    execution: ExecutionRequest,
    activeOverrides: Set[OverrideLabel]
  )(implicit
    trace: Trace
  ): IO[CalibanError, PreparedPlan] =
    ZIO
      .blocking(ZIO.fromEither(planner.plan(document, execution, activeOverrides)))
      .mapError(failure => CalibanError.ValidationError(failure.message, ""))
      .map(PreparedPlan(_))

  private def resolveProgressiveOverrides(
    request: GraphQLRequest,
    document: Document
  )(implicit trace: Trace): ZIO[R, CalibanError, Set[OverrideLabel]] = {
    val overrides = planner.progressiveOverrides(document, request.operationName).toList.sortBy(_._1.value)
    val custom    = overrides.collect { case (label, None) => label }.toSet
    for {
      sampled  <- ZIO.foreach(overrides) {
                    case (label, Some(percentage)) =>
                      if (percentage <= 0) ZIO.none
                      else if (percentage >= 100) ZIO.some(label)
                      else
                        Random.nextDouble.map(value => if (value * 100d < percentage.toDouble) Some(label) else None)
                    case (_, None)                 => ZIO.none
                  }
      resolved <- resolveOverrideLabels(request, custom)
    } yield sampled.flatten.toSet ++ resolved
  }

  private def enforceCost(prepared: Prepared): IO[CalibanError.ValidationError, Unit] =
    maxOperationCost match {
      case Some(maximum) =>
        def reject(message: String, code: String) =
          ZIO.fail(CalibanError.ValidationError(message, "", extensions = errorCode(code)))

        estimateCost(prepared.executionRequest, prepared.plan.plan) match {
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

  private def operationWeight(
    query: String,
    executionPlan: Option[PreparedPlan],
    operationName: Option[String]
  ): Long = {
    def fieldWeight(values: List[Field]): Long =
      values.foldLeft(0L)((count, value) =>
        count + 1L + value.arguments.valuesIterator.map(_.toInputString.length.toLong).sum + fieldWeight(value.fields)
      )

    val planWeight = executionPlan
      .map(_.plan)
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

  private def symbolicVariables(document: Document): Map[String, InputValue] =
    document.operationDefinitions.iterator
      .flatMap(_.variableDefinitions.iterator)
      .map(definition => definition.name -> VariableValue(definition.name))
      .toMap

  private def hasVariableCondition(document: Document, operationName: Option[String]): Boolean = {
    def isVariableCondition(directive: Directive): Boolean =
      isInclusionDirective(directive) && directive.arguments.values.exists {
        case _: VariableValue => true
        case _                => false
      }

    document.hasDirective(operationName)(isVariableCondition)
  }
}

private[gateway] object OperationPreparation {

  final case class Prepared(
    request: GraphQLRequest,
    document: Document,
    executionRequest: ExecutionRequest,
    plan: PreparedPlan
  )

  private val VariableValidation: Option[List[Validator.QueryValidation]] = Some(List(Validator.validateVariables))

  def make[R](
    rootType: RootType,
    planner: OperationPlanner,
    securityRequirements: OperationPlan => List[SecurityRequirement],
    config: GatewayConfig,
    phaseHooks: PhaseHooks[R],
    estimateCost: (ExecutionRequest, OperationPlan) => Either[String, Long]
  )(implicit trace: Trace): UIO[OperationPreparation[R]] =
    OperationCache
      .make[CacheKey, CalibanError, CachedOperation, R](config.maxOperationCacheWeight, phaseHooks)
      .map(cache =>
        new OperationPreparation(
          rootType,
          planner,
          securityRequirements,
          cache,
          config.maxOperationCost,
          phaseHooks,
          estimateCost
        )
      )

  def isInternalFailure(error: CalibanError): Boolean =
    error match {
      case CalibanError.ExecutionError(message, _, _, Some(_), _) =>
        message == ResolutionFailure || message == PolicyFailure || message == OverrideLabelResolutionFailure
      case _                                                      => false
    }

  private val ResolutionFailure              = "Operation resolution failed."
  private val PolicyFailure                  = "Operation policy failed."
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
              Cause.fail(CalibanError.ExecutionError(failureMessage, innerThrowable = Some(cause.squash)))
          }
        )(fiberId => Cause.interrupt(fiberId))
      )

  private sealed trait CachedOperation

  private object CachedOperation {
    final case class DocumentOnly(document: Document)                                           extends CachedOperation
    final case class Planned(document: Document, plan: PreparedPlan)                            extends CachedOperation
    final case class Ready(document: Document, execution: ExecutionRequest, plan: PreparedPlan) extends CachedOperation
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
