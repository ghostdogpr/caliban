package caliban.gateway.internal

import caliban.{ CalibanError, Configurator, GraphQLRequest, InputValue, ResponseValue, Value }
import caliban.execution.{ ExecutionRequest, Field, RequestPreparation }
import caliban.gateway.{ GatewayConfig, GatewayWrapper }
import caliban.gateway.internal.execution.PreparedPlan
import caliban.gateway.internal.OperationCache.Weighted
import caliban.gateway.internal.OperationPreparation._
import caliban.gateway.internal.composition.ComposedGraph.OverrideLabel
import caliban.gateway.internal.planning.CandidateSearch.PlanningFailure
import caliban.gateway.internal.planning.{ OperationPlan, OperationPlanner }
import caliban.InputValue.VariableValue
import caliban.parsing.adt.{ Directive, Document }
import caliban.schema.RootType
import caliban.validation.Validator
import zio.{ Exit, IO, Random, Trace, UIO, ZIO }

private[gateway] final class OperationPreparation[-R] private (
  rootType: RootType,
  planner: OperationPlanner,
  hooks: OperationHooks[R],
  cache: OperationCache[CacheKey, CalibanError, CachedOperation, R],
  maxOperationCost: Option[Long],
  estimateCost: (ExecutionRequest, OperationPlan) => Either[String, Long]
) {

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    for {
      document <- RequestPreparation.parse(query)
      _        <- Validator.validate(document, rootType)
    } yield ()

  def prepare(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, Prepared] =
    for {
      resolved   <- hooks.resolve(request)
      query       = resolved.query.getOrElse("")
      config     <- Configurator.ref.get
      preparation = PreparationConfig.from(config)
      prepared   <- if (hooks.cacheable) prepareCached(resolved, query, preparation)
                    else prepareUncached(resolved, query)
      _          <- enforceCost(prepared)
      _          <- hooks.evaluatePolicy(resolved, prepared.document, prepared.executionRequest, prepared.plan.plan)
    } yield prepared

  private def enforceCost(prepared: Prepared): IO[CalibanError.ValidationError, Unit] =
    maxOperationCost match {
      case Some(maximum) =>
        def reject(message: String, code: String) =
          ZIO.fail(
            CalibanError.ValidationError(
              message,
              "",
              extensions = Some(ResponseValue.ObjectValue(List("code" -> Value.StringValue(code))))
            )
          )

        estimateCost(prepared.executionRequest, prepared.plan.plan) match {
          case Left(error)      => reject(error, "COST_QUERY_PARSE_FAILURE")
          case Right(estimated) =>
            if (estimated > maximum)
              reject(
                s"Operation cost $estimated exceeds the configured maximum of $maximum.",
                "COST_ESTIMATED_TOO_EXPENSIVE"
              )
            else ZIO.unit
        }
      case None          => ZIO.unit
    }

  private def prepareCached(
    request: GraphQLRequest,
    query: String,
    preparation: PreparationConfig
  )(implicit trace: Trace): ZIO[R, CalibanError, Prepared] = {
    def cached(parse: => IO[CalibanError, Document], activeOverrides: Set[OverrideLabel]) =
      cache
        .getOrCompute(
          CacheKey(
            query,
            request.operationName,
            request.isHttpGetRequest,
            preparation,
            activeOverrides
          )
        )(parse.flatMap(computeCached(request, _, preparation, activeOverrides)))
        .flatMap(materialize(request, _, activeOverrides))

    if (planner.hasProgressiveOverrides)
      for {
        document        <- RequestPreparation.parse(query)
        activeOverrides <- resolveProgressiveOverrides(request, document, request.operationName)
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
      _        <- Validator.validate(document, rootType).unless(preparation.skipValidation)
      variables = symbolicVariables(document)
      planned  <-
        if (hasVariableCondition(document, request.operationName)) ZIO.none
        else
          for {
            execution <- RequestPreparation.prepareParsed(
                           request,
                           document,
                           variables,
                           rootType,
                           skipValidation = true
                         )
            plan      <- preparePlan(document, execution, activeOverrides)
          } yield Some((execution, plan))
    } yield {
      val execution =
        if (variables.isEmpty && !planned.exists(_._2.hasVariableReferences)) planned.map(_._1)
        else None
      val cached    = CachedOperation(document, planned.map(_._2), execution)
      Weighted(cached, operationWeight(request.query.getOrElse(""), cached.executionPlan, request.operationName))
    }

  private def prepareUncached(
    request: GraphQLRequest,
    query: String
  )(implicit trace: Trace): ZIO[R, CalibanError, Prepared] =
    for {
      document  <- RequestPreparation.parse(query)
      overrides <- resolveProgressiveOverrides(request, document, request.operationName)
      variables <- RequestPreparation.coerceVariables(document, request, rootType)
      execution <- RequestPreparation.prepareParsed(
                     request,
                     document,
                     variables,
                     rootType,
                     skipValidation = false
                   )
      plan      <- preparePlan(document, execution, overrides)
    } yield Prepared(request, document, execution, plan)

  private def materialize(
    request: GraphQLRequest,
    cached: CachedOperation,
    activeOverrides: Set[OverrideLabel]
  )(implicit trace: Trace): IO[CalibanError, Prepared] =
    (cached.execution, cached.executionPlan) match {
      case (Some(execution), Some(plan)) =>
        Exit.succeed(Prepared(request, cached.document, execution, plan))
      case _                             =>
        for {
          variables <- RequestPreparation.coerceVariables(cached.document, request, rootType)
          execution <- RequestPreparation.prepareParsed(
                         request,
                         cached.document,
                         variables,
                         rootType,
                         skipValidation = false,
                         validations = Some(List(Validator.validateVariables))
                       )
          plan      <- cached.executionPlan match {
                         case Some(value) if !value.hasVariableReferences => Exit.succeed(value)
                         case Some(value)                                 => Exit.succeed(value.bind(variables))
                         case None                                        => preparePlan(cached.document, execution, activeOverrides)
                       }
        } yield Prepared(request, cached.document, execution, plan)
    }

  private def preparePlan(
    document: Document,
    execution: ExecutionRequest,
    activeOverrides: Set[OverrideLabel]
  )(implicit
    trace: Trace
  ): IO[CalibanError, PreparedPlan] =
    ZIO
      .blocking(ZIO.fromEither(planner.plan(document, execution, activeOverrides)))
      .mapError(planningFailure)
      .map(new PreparedPlan(_))

  private def resolveProgressiveOverrides(
    request: GraphQLRequest,
    document: Document,
    operationName: Option[String]
  )(implicit trace: Trace): ZIO[R, CalibanError, Set[OverrideLabel]] = {
    val overrides = planner.progressiveOverrides(document, operationName).toList.sortBy(_._1.value)
    val custom    = overrides.collect { case (label, None) => label }.toSet
    for {
      percentages <- ZIO.foreach(overrides) {
                       case (label, Some(percentage)) =>
                         if (percentage <= 0) ZIO.none
                         else if (percentage >= 100) ZIO.some(label)
                         else
                           Random.nextDouble.map(value => if (value * 100d < percentage.toDouble) Some(label) else None)
                       case (_, None)                 => ZIO.none
                     }
      resolved    <- hooks.resolveOverrideLabels(request, custom)
    } yield percentages.flatten.toSet ++ resolved
  }

  private def operationWeight(
    query: String,
    executionPlan: Option[PreparedPlan],
    operationName: Option[String]
  ): Long = {
    def fields(values: List[Field]): Long =
      values.foldLeft(0L)((count, value) =>
        count + 1L + value.arguments.valuesIterator.map(_.toInputString.length.toLong).sum + fields(value.fields)
      )

    val planWeight = executionPlan
      .map(_.plan)
      .fold(0L)(value =>
        fields(value.fields) +
          value.roots.foldLeft(0L)((count, fetch) => count + fields(fetch.client) + fields(fetch.selections)) +
          value.entities.foldLeft(0L)((count, fetch) => count + fields(fetch.fields)) +
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
      (directive.name == "skip" || directive.name == "include") && directive.arguments.values.exists {
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

  private final case class CachedOperation(
    document: Document,
    executionPlan: Option[PreparedPlan],
    execution: Option[ExecutionRequest]
  )

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

  def make[R](
    rootType: RootType,
    planner: OperationPlanner,
    hooks: OperationHooks[R],
    config: GatewayConfig,
    wrapper: GatewayWrapper[R],
    estimateCost: (ExecutionRequest, OperationPlan) => Either[String, Long]
  )(implicit trace: Trace): UIO[OperationPreparation[R]] =
    OperationCache
      .make[CacheKey, CalibanError, CachedOperation, R](config.maxOperationCacheWeight, wrapper)
      .map(cache =>
        new OperationPreparation(
          rootType,
          planner,
          hooks,
          cache,
          config.maxOperationCost,
          estimateCost
        )
      )

  private def planningFailure(failure: PlanningFailure): CalibanError.ValidationError =
    CalibanError.ValidationError(failure.message, "")
}
