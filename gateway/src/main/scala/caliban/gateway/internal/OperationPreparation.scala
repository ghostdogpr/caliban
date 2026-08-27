package caliban.gateway.internal

import caliban.InputValue.VariableValue
import caliban.execution.{ ExecutionRequest, Field, RequestPreparation }
import caliban.gateway.GatewayInterpreter.OperationCacheStatus
import caliban.gateway.{ GatewayConfig, GatewayWrapper }
import caliban.gateway.internal.OperationCache.Weighted
import caliban.gateway.internal.OperationCacheMode.Cacheable
import caliban.gateway.internal.OperationPreparation._
import caliban.gateway.internal.OperationPlanner.{ OperationPlan, PlanningFailure }
import caliban.parsing.adt.{ Directive, Document }
import caliban.schema.RootType
import caliban.validation.Validator
import caliban.validation.Validator.AllValidations
import caliban.{ CalibanError, Configurator, GraphQLRequest, InputValue }
import zio.{ Exit, IO, Trace, UIO, ZIO }

private[gateway] final class OperationPreparation[-R] private (
  rootType: RootType,
  planner: OperationPlanner,
  hooks: OperationHooks[R],
  limits: OperationParsingLimits,
  cache: OperationCache[CacheKey, CalibanError, CachedOperation, R]
) {

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    for {
      parsed  <- parseWithinLimits(query)
      document = parsed.document
      _       <- Validator.validate(document, rootType)
    } yield ()

  def prepare(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, Prepared] =
    for {
      resolved   <- hooks.resolve(request)
      query       = resolved.query.getOrElse("")
      config     <- Configurator.ref.get
      preparation = PreparationConfig.from(config)
      cacheable   = config.skipValidation || config.validations == AllValidations
      prepared   <- hooks.cacheMode match {
                      case Cacheable if cacheable =>
                        cache
                          .getOrCompute(
                            CacheKey(
                              query,
                              resolved.operationName,
                              resolved.isHttpGetRequest,
                              preparation
                            )
                          )(parseWithinLimits(query).flatMap(parsed => computeCached(resolved, parsed, preparation)))
                          .flatMap(materialize(resolved, _))
                      case _                      =>
                        prepareUncached(resolved, query)
                    }
      _          <- hooks.evaluatePolicy(resolved, prepared.document, prepared.executionRequest)
    } yield prepared

  def cacheStatus(implicit trace: Trace): UIO[OperationCacheStatus] = cache.status

  private def parseWithinLimits(query: String)(implicit trace: Trace): IO[CalibanError, ParsedWithinLimits] =
    for {
      textBytes <- ZIO.fromEither(limits.textBytes(query).left.map(limitFailure))
      document  <- RequestPreparation.parse(query)
      nodeCount <- ZIO.fromEither(limits.documentNodes(document).left.map(limitFailure))
    } yield ParsedWithinLimits(document, textBytes, nodeCount)

  private def computeCached(
    request: GraphQLRequest,
    parsed: ParsedWithinLimits,
    preparation: PreparationConfig
  )(implicit trace: Trace): IO[CalibanError, Weighted[CachedOperation]] = {
    val document = parsed.document
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
            plan      <- plan(document, execution)
          } yield Some((execution, plan))
    } yield {
      val execution =
        if (variables.isEmpty && !planned.exists(_._2.hasVariableReferences)) planned.map(_._1)
        else None
      val cached    = CachedOperation(document, planned.map(_._2), execution)
      Weighted(cached, operationWeight(parsed.textBytes, parsed.nodeCount, cached.plan, request.operationName))
    }
  }

  private def prepareUncached(
    request: GraphQLRequest,
    query: String
  )(implicit trace: Trace): IO[CalibanError, Prepared] =
    for {
      parsed    <- parseWithinLimits(query)
      document   = parsed.document
      variables <- RequestPreparation.coerceVariables(document, request, rootType)
      execution <- RequestPreparation.prepareParsed(
                     request,
                     document,
                     variables,
                     rootType,
                     skipValidation = false
                   )
      plan      <- plan(document, execution)
    } yield Prepared(request, document, execution, plan)

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
          execution <- RequestPreparation.prepareParsed(
                         request,
                         cached.document,
                         variables,
                         rootType,
                         skipValidation = false,
                         validations = Some(List(Validator.validateVariables))
                       )
          plan      <- cached.plan match {
                         case Some(value) if !value.hasVariableReferences => Exit.succeed(value)
                         case Some(value)                                 => Exit.succeed(value.bind(variables))
                         case None                                        => plan(cached.document, execution)
                       }
        } yield Prepared(request, cached.document, execution, plan)
    }

  private def plan(document: Document, execution: ExecutionRequest)(implicit
    trace: Trace
  ): IO[CalibanError, OperationPlan] =
    ZIO.blocking(ZIO.fromEither(planner.plan(document, execution))).mapError(planningFailure)

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
        value.typenameSelections.size.toLong
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

  private final case class ParsedWithinLimits(document: Document, textBytes: Int, nodeCount: Int)

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
    preparation: PreparationConfig
  )

  private final case class PreparationConfig(
    skipValidation: Boolean,
    enableIntrospection: Boolean,
    allowMutationsOverGetRequests: Boolean
  )

  private object PreparationConfig {
    def from(config: Configurator.ExecutionConfiguration): PreparationConfig =
      PreparationConfig(
        config.skipValidation,
        config.enableIntrospection,
        config.allowMutationsOverGetRequests
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
          new OperationParsingLimits(
            config.maxOperationTextBytes,
            config.maxOperationNesting,
            config.maxParsedOperationNodes
          ),
          cache
        )
      )

  private def limitFailure(failure: OperationParsingLimits.Failure): CalibanError.ValidationError =
    failure match {
      case OperationParsingLimits.TextTooLarge   =>
        CalibanError.ValidationError("Operation text exceeded the configured byte limit.", "")
      case OperationParsingLimits.NestingTooDeep =>
        CalibanError.ValidationError("Operation nesting exceeded the configured limit.", "")
      case OperationParsingLimits.TooManyNodes   =>
        CalibanError.ValidationError("Operation structure exceeded the configured node limit.", "")
    }

  private def planningFailure(failure: PlanningFailure): CalibanError.ValidationError =
    CalibanError.ValidationError(failure.message, "")
}
