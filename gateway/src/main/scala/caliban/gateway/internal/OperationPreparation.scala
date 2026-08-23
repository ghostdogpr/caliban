package caliban.gateway.internal

import caliban.InputValue.VariableValue
import caliban.execution.{ ExecutionRequest, Field, RequestPreparation }
import caliban.gateway.GatewayRuntime.OperationCacheStatus
import caliban.gateway.{ GatewayConfig, GatewayWrapper }
import caliban.gateway.internal.OperationCache.Weighted
import caliban.gateway.internal.OperationCacheDirective.{ Bypass, Cacheable }
import caliban.gateway.internal.OperationPreparation._
import caliban.gateway.internal.OperationPlanner.{ OperationPlan, PlanningFailure }
import caliban.parsing.adt.{ Directive, Document, Selection }
import caliban.schema.RootType
import caliban.validation.Validator
import caliban.validation.Validator.{ AllValidations, QueryValidation }
import caliban.{ CalibanError, Configurator, GraphQLRequest, InputValue }
import zio.{ Exit, IO, Trace, UIO, ZIO }

private[gateway] final class OperationPreparation[-R] private (
  rootType: RootType,
  planner: OperationPlanner,
  hooks: OperationHooks[R],
  limits: OperationLimits,
  cache: OperationCache[CacheKey, CalibanError, CachedOperation, R]
) {

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    for {
      _        <- ZIO.fromEither(limits.textWeight(query).left.map(limitFailure))
      document <- RequestPreparation.parse(query)
      _        <- ZIO.fromEither(limits.documentWeight(document).left.map(limitFailure))
      _        <- Validator.validate(document, rootType)
    } yield ()

  def prepare(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, Prepared] =
    for {
      resolved    <- hooks.resolve(request)
      query        = resolved.query.getOrElse("")
      preparation <- Configurator.ref.get.map(PreparationConfig.from)
      prepared    <- hooks.cacheDirective match {
                       case Bypass                                               => checkedUncached(resolved, query)
                       case Cacheable(resolver, policy) if preparation.cacheable =>
                         cache
                           .getOrCompute(
                             CacheKey(
                               query,
                               resolved.operationName,
                               resolved.isHttpGetRequest,
                               resolver,
                               policy,
                               preparation
                             )
                           )(
                             ZIO
                               .fromEither(limits.textWeight(query).left.map(limitFailure))
                               .flatMap(computeWeighted(resolved, _, preparation))
                           )
                           .flatMap(materialize(resolved, _))
                       case _: Cacheable                                         => checkedUncached(resolved, query)
                     }
      _           <- hooks.evaluatePolicy(resolved, prepared.document, prepared.executionRequest)
    } yield prepared

  private def checkedUncached(resolved: GraphQLRequest, query: String)(implicit
    trace: Trace
  ): IO[CalibanError, Prepared] =
    ZIO.fromEither(limits.textWeight(query).left.map(limitFailure)) *> prepareUncached(resolved)

  def cacheStatus(implicit trace: Trace): UIO[OperationCacheStatus] = cache.status

  private def computeWeighted(
    request: GraphQLRequest,
    textWeight: Int,
    preparation: PreparationConfig
  )(implicit trace: Trace): IO[CalibanError, Weighted[CachedOperation]] =
    computeCached(request, preparation).map { case (cached, nodes) =>
      Weighted(cached, operationWeight(textWeight, nodes, cached.plan, request.operationName))
    }

  private def computeCached(
    request: GraphQLRequest,
    preparation: PreparationConfig
  )(implicit trace: Trace): IO[CalibanError, (CachedOperation, Int)] =
    for {
      document <- RequestPreparation.parse(request.query.getOrElse(""))
      nodes    <- ZIO.fromEither(limits.documentWeight(document).left.map(limitFailure))
      _        <- Validator.validate(document, rootType).unless(preparation.skipValidation)
      planned  <-
        if (hasVariableCondition(document, request.operationName)) ZIO.none
        else
          for {
            execution <- RequestPreparation.prepareParsedWithVariables(
                           request,
                           document,
                           symbolicVariables(document),
                           rootType,
                           documentIsValid = true
                         )
            plan      <- ZIO.fromEither(planner.plan(document, execution)).mapError(planningFailure)
          } yield Some((execution, plan))
    } yield {
      val execution =
        if (symbolicVariables(document).isEmpty && !planned.exists(_._2.hasVariableReferences)) planned.map(_._1)
        else None
      CachedOperation(document, planned.map(_._2), execution) -> nodes
    }

  private def prepareUncached(request: GraphQLRequest)(implicit trace: Trace): IO[CalibanError, Prepared] =
    for {
      document <- RequestPreparation.parse(request.query.getOrElse(""))
      _        <- ZIO.fromEither(limits.documentWeight(document).left.map(limitFailure))
      prepared <- RequestPreparation.prepareParsedWithIntrospection(request, document, rootType)
      plan     <- ZIO.fromEither(planner.plan(document, prepared.executionRequest)).mapError(planningFailure)
    } yield Prepared(request, document, prepared.executionRequest, plan)

  private def materialize(
    request: GraphQLRequest,
    cached: CachedOperation
  )(implicit trace: Trace): IO[CalibanError, Prepared] =
    (cached.execution, cached.plan) match {
      case (Some(execution), Some(plan)) =>
        Exit.succeed(Prepared(request, cached.document, execution, plan))
      case _                             =>
        for {
          variables <- RequestPreparation.coerceVariables(cached.document, request, rootType)
          execution <- RequestPreparation.prepareParsedWithVariableValidation(
                         request,
                         cached.document,
                         variables,
                         rootType
                       )
          plan      <- cached.plan match {
                         case Some(value) if !value.hasVariableReferences => Exit.succeed(value)
                         case Some(value)                                 => Exit.succeed(value.bind(variables))
                         case None                                        =>
                           Exit.fromEither(planner.plan(cached.document, execution)).mapError(planningFailure)
                       }
        } yield Prepared(request, cached.document, execution, plan)
    }

  private def operationWeight(
    text: Int,
    nodes: Int,
    plan: Option[OperationPlan],
    operationName: Option[String]
  ): Long = {
    def fields(values: List[Field]): Long =
      values.foldLeft(0L)((count, value) =>
        count + 1L + value.arguments.valuesIterator.map(_.toInputString.length.toLong).sum + fields(value.fields)
      )

    val planWeight = plan.fold(0L)(value =>
      fields(value.fields) +
        value.roots.foldLeft(0L)((count, route) => count + fields(route.client) + fields(route.downstream)) +
        value.entities.foldLeft(0L)((count, route) => count + fields(route.fields)) +
        value.runtimeTypes.size.toLong
    )

    text.toLong * 2L + nodes.toLong + operationName.fold(0)(_.length).toLong + planWeight + 1L
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
    plan: OperationPlan
  )

  private final case class CachedOperation(
    document: Document,
    plan: Option[OperationPlan],
    execution: Option[ExecutionRequest]
  )

  private final case class CacheKey(
    query: String,
    operationName: Option[String],
    isHttpGetRequest: Boolean,
    resolver: Option[String],
    policy: Option[String],
    preparation: PreparationConfig
  )

  private final case class PreparationConfig(
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    allowMutationsOverGetRequests: Boolean,
    validations: List[QueryValidation]
  ) {
    val cacheable: Boolean = skipValidation || validations == AllValidations
  }

  private object PreparationConfig {
    def from(config: Configurator.ExecutionConfiguration): PreparationConfig =
      PreparationConfig(
        config.skipValidation,
        config.enableIntrospection,
        config.allowMutationsOverGetRequests,
        if (config.skipValidation) Nil else config.validations
      )
  }

  def make[R](
    rootType: RootType,
    planner: OperationPlanner,
    hooks: OperationHooks[R],
    config: GatewayConfig,
    wrapper: GatewayWrapper[R]
  )(implicit trace: Trace): UIO[OperationPreparation[R]] =
    OperationCache
      .make[CacheKey, CalibanError, CachedOperation, R](config.maxOperationCacheWeight, wrapper)
      .map(cache =>
        new OperationPreparation(
          rootType,
          planner,
          hooks,
          new OperationLimits(
            config.maxOperationTextBytes,
            config.maxOperationNesting,
            config.maxParsedOperationNodes
          ),
          cache
        )
      )

  private def limitFailure(failure: OperationLimits.Failure): CalibanError.ValidationError =
    failure match {
      case OperationLimits.TextTooLarge   =>
        CalibanError.ValidationError("Operation text exceeded the configured byte limit.", "")
      case OperationLimits.NestingTooDeep =>
        CalibanError.ValidationError("Operation nesting exceeded the configured limit.", "")
      case OperationLimits.TooManyNodes   =>
        CalibanError.ValidationError("Operation structure exceeded the configured node limit.", "")
    }

  private def planningFailure(failure: PlanningFailure): CalibanError.ValidationError =
    CalibanError.ValidationError(failure.message, "")
}
