package caliban.gateway.internal

import caliban.GraphQLResponseContext.{ markRequestError, markServerError, ServerFailure }
import caliban.InputValue.VariableValue
import caliban.execution.{ ExecutionRequest, Field, RequestPreparation }
import caliban.gateway.PhaseHooks.{ Event, Outcome }
import caliban.gateway.internal.OperationCache.Weighted
import caliban.gateway.internal.OperationPreparation._
import caliban.gateway.internal.composition.ComposedGraph.OverrideLabel
import caliban.gateway.internal.planning.{ OperationCost, OperationPlan, OperationPlanner, OperationSecurity }
import caliban.gateway.{ errorCode, isInclusionDirective, PhaseHandler, PhaseHooks }
import caliban.parsing.adt.{ Directive, Document }
import caliban.schema.RootType
import caliban.validation.Validator
import caliban._
import zio.query.Cache
import zio.{ Cause, Exit, IO, Random, Trace, UIO, ZIO }

private[gateway] final class OperationPreparation[-R] private (
  rootType: RootType,
  planner: OperationPlanner,
  security: OperationSecurity,
  cache: OperationCache[CacheKey, CalibanError, CachedOperation, R],
  costLimit: Option[(OperationCost, Long)],
  hooks: PhaseHooks[R]
) {

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    for {
      document <- RequestPreparation.parse(query)
      _        <- Validator.validate(document, rootType)
    } yield ()

  def prepare(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, Failure, ExecutableOperation] =
    for {
      resolved  <- resolveRequest(request)
      operation <- prepareResolved(resolved.request, resolved.cacheable)
      _         <- enforceCost(operation).mapError(Rejected(_))
      _         <- authorize(operation)
    } yield operation

  private def resolveRequest(
    request: GraphQLRequest
  )(implicit trace: Trace): ZIO[R, Failure, Event.Resolution] =
    runHook(hooks.resolution, Event.Resolution(request), ResolutionFailure) {
      case PhaseHooks.Rejection(message, code) => CalibanError.ExecutionError(message, extensions = errorCode(code))
    }

  private def authorize(operation: ExecutableOperation)(implicit trace: Trace): ZIO[R, Failure, Unit] =
    security.requirements(operation.plan) match {
      case None               => ZIO.fail(Rejected(CalibanError.ValidationError(UnsupportedPolicyFailure, "")))
      case Some(requirements) =>
        runHook(
          hooks.authorization,
          Event.Authorization(operation.request, operation.document, operation.executionRequest, requirements),
          AuthorizationFailure
        ) { case PhaseHooks.Denial(reason) => CalibanError.ValidationError(reason, "") }.unit
    }

  private def prepareResolved(
    request: GraphQLRequest,
    cacheable: Boolean
  )(implicit trace: Trace): ZIO[R, Failure, ExecutableOperation] = Configurator.ref.get.flatMap { config =>
    val query    = request.query.getOrElse("")
    val template = request.copy(variables = None, extensions = None)

    def lookup(parse: => IO[CalibanError, Document], activeOverrides: Set[OverrideLabel]) = {
      val key                                                  = CacheKey(template, config.copy(queryCache = UnusedQueryCache), activeOverrides)
      val operation: ZIO[R, CalibanError, ExecutableOperation] =
        if (cacheable) cache.getOrCompute(key)(parse.flatMap(buildCacheEntry(key, _))).flatMap(_(request))
        else prepareUncached(request, parse, activeOverrides)
      operation.mapError(Rejected(_))
    }

    if (planner.hasProgressiveOverrides)
      for {
        document        <- RequestPreparation.parse(query).mapError(Rejected(_))
        activeOverrides <- selectOverrides(request, document)
        operation       <- lookup(Exit.succeed(document), activeOverrides)
      } yield operation
    else
      lookup(RequestPreparation.parse(query), Set.empty)
  }

  private def prepareUncached(
    request: GraphQLRequest,
    parse: IO[CalibanError, Document],
    activeOverrides: Set[OverrideLabel]
  )(implicit trace: Trace): IO[CalibanError, ExecutableOperation] =
    parse.flatMap { document =>
      RequestPreparation.checkIntrospection(document, request.operationName) *>
        buildOperation(request, document, documentValidated = false)((_, execution) =>
          buildPlan(document, execution, activeOverrides)
        )
    }

  private def buildCacheEntry(key: CacheKey, document: Document)(implicit
    trace: Trace
  ): IO[CalibanError, Weighted[CachedOperation]] = {
    val request = key.request
    for {
      _      <- RequestPreparation.checkIntrospection(document, request.operationName)
      _      <- validateDocument(document)
      cached <-
        if (hasVariableInclusionDirective(document, request.operationName))
          ZIO.succeed(
            Weighted[CachedOperation](
              incoming =>
                buildOperation(incoming, document, documentValidated = true)((_, execution) =>
                  buildPlan(document, execution, key.activeOverrides)
                ),
              cacheWeight(key)
            )
          )
        else {
          val variables = symbolicVariables(document, request.operationName)
          for {
            execution <-
              RequestPreparation.prepareParsed(request, document, variables, rootType, skipValidation = true)
            plan      <- buildPlan(document, execution, key.activeOverrides)
          } yield {
            val cached: CachedOperation =
              if (variables.isEmpty && !plan.hasVariableReferences)
                incoming => Exit.succeed(ExecutableOperation(incoming, document, execution, plan))
              else
                incoming =>
                  buildOperation(incoming, document, documentValidated = true)((values, _) =>
                    Exit.succeed(plan.bind(values))
                  )
            Weighted(cached, cacheWeight(key) + planWeight(plan))
          }
        }
    } yield cached
  }

  /**
   * Cached operations validated their document without variables, so binding checks only the variables. Otherwise the
   * configured validations run once with the coerced variables, and document errors still take precedence over
   * variable errors.
   */
  private def buildOperation(request: GraphQLRequest, document: Document, documentValidated: Boolean)(
    getPlan: (Map[String, InputValue], ExecutionRequest) => IO[CalibanError, OperationPlan]
  )(implicit trace: Trace): IO[CalibanError, ExecutableOperation] =
    for {
      variables <- RequestPreparation
                     .coerceVariables(document, request, rootType)
                     .tapError(_ => validateDocument(document).unless(documentValidated))
      execution <- RequestPreparation.prepareParsed(
                     request,
                     document,
                     variables,
                     rootType,
                     skipValidation = false,
                     validations = if (documentValidated) VariableValidation else None
                   )
      plan      <- getPlan(variables, execution)
    } yield ExecutableOperation(request, document, execution, plan)

  private def validateDocument(document: Document)(implicit trace: Trace): IO[CalibanError, Unit] =
    Configurator.ref.getWith(config => if (config.skipValidation) Exit.unit else Validator.validate(document, rootType))

  private def buildPlan(document: Document, execution: ExecutionRequest, activeOverrides: Set[OverrideLabel])(implicit
    trace: Trace
  ): IO[CalibanError, OperationPlan] =
    ZIO
      .blocking(ZIO.fromEither(planner.plan(document, execution, activeOverrides)))
      .mapError(failure => CalibanError.ValidationError(failure.message, ""))

  private def selectOverrides(
    request: GraphQLRequest,
    document: Document
  )(implicit trace: Trace): ZIO[R, Failure, Set[OverrideLabel]] = {
    val overrides = planner.progressiveOverrides(document, request.operationName).toList.sortBy(_.value)
    val reached   = overrides.collect { case OverrideLabel.Custom(label) => label }.toSet
    for {
      active <- ZIO.filter(overrides.collect { case label: OverrideLabel.Percent => label })(label =>
                  Random.nextDouble.map(_ * 100d < label.percentage.toDouble)
                )
      custom <-
        if (reached.isEmpty) Exit.succeed(Set.empty[String])
        else
          runHook(hooks.overrideLabels, Event.OverrideLabels(request, reached), OverrideLabelResolutionFailure)(
            PartialFunction.empty
          ).map(_.active.intersect(reached))
    } yield active.toSet ++ custom.map(OverrideLabel.Custom(_))
  }

  private def enforceCost(operation: ExecutableOperation): IO[CalibanError.ValidationError, Unit] =
    costLimit.fold[IO[CalibanError.ValidationError, Unit]](ZIO.unit) { case (cost, maximum) =>
      def reject(message: String, code: String) =
        ZIO.fail(CalibanError.ValidationError(message, "", extensions = errorCode(code)))

      cost.estimate(operation.plan) match {
        case Left(error)                             => reject(error, "COST_QUERY_PARSE_FAILURE")
        case Right(estimated) if estimated > maximum =>
          reject(
            s"Operation cost $estimated exceeds the configured maximum of $maximum.",
            "COST_ESTIMATED_TOO_EXPENSIVE"
          )
        case Right(_)                                => ZIO.unit
      }
    }

  private def cacheWeight(key: CacheKey): Long =
    key.request.query.fold(0)(_.length).toLong * 2L + key.request.operationName.fold(0)(_.length).toLong + 1L

  private def planWeight(plan: OperationPlan): Long = {
    def fieldWeight(values: List[Field]): Long =
      values.foldLeft(0L)((count, value) =>
        count + 1L + value.arguments.valuesIterator.map(_.toInputString.length.toLong).sum + fieldWeight(value.fields)
      )

    fieldWeight(plan.fields) +
      plan.roots.foldLeft(0L)((count, fetch) => count + fieldWeight(fetch.client) + fieldWeight(fetch.downstream)) +
      plan.entities.foldLeft(0L)((count, fetch) => count + fieldWeight(fetch.fields)) +
      plan.typenameSelections.size.toLong
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
    maxOperationCacheWeight: Long,
    costLimit: Option[(OperationCost, Long)],
    hooks: PhaseHooks[R]
  )(implicit trace: Trace): UIO[OperationPreparation[R]] =
    OperationCache
      .make[CacheKey, CalibanError, CachedOperation, R](maxOperationCacheWeight, hooks)
      .map(new OperationPreparation(rootType, planner, security, _, costLimit, hooks))

  sealed abstract class Failure(val outcome: Outcome, val mark: UIO[Unit]) { def error: CalibanError }
  final case class Rejected(error: CalibanError) extends Failure(Outcome.RequestError, markRequestError(error))
  final case class Internal(error: CalibanError) extends Failure(Outcome.InternalError, InternalMark)

  private val InternalMark = markServerError(ServerFailure.Internal)

  private val ResolutionFailure              = "Operation resolution failed."
  private val AuthorizationFailure           = "Operation authorization failed."
  private val OverrideLabelResolutionFailure = "Progressive override label resolution failed."
  private val UnsupportedPolicyFailure       = "Operation selects fields guarded by unsupported @policy directives."

  private def runHook[R, Ev](handler: PhaseHandler[R, Ev, Throwable, Any], event: Ev, failureMessage: String)(
    rejection: PartialFunction[Throwable, CalibanError]
  )(implicit trace: Trace): ZIO[R, Failure, Ev] =
    if (!handler.enabled) Exit.succeed(event)
    else
      ZIO
        .suspendSucceed(handler.runWith(event)(Exit.succeed)(_ => ()))
        .mapErrorCause { cause =>
          def internal: Failure =
            Internal(CalibanError.ExecutionError(failureMessage, innerThrowable = Some(cause.squash)))
          cause.interruptOption.fold[Cause[Failure]](Cause.fail(cause.failures match {
            case failure :: Nil if cause.defects.isEmpty =>
              rejection.andThen(Rejected(_)).applyOrElse(failure, (_: Throwable) => internal)
            case _                                       => internal
          }))(Cause.interrupt(_))
        }

  private type CachedOperation = GraphQLRequest => IO[CalibanError, ExecutableOperation]

  private final case class CacheKey(
    request: GraphQLRequest,
    // Reuse function instances across requests. Fresh lambdas cause misses; rebuilding only the list does not.
    config: Configurator.ExecutionConfiguration,
    activeOverrides: Set[OverrideLabel]
  )

  // Cache entries never read the query cache, so the key does not depend on it.
  private val UnusedQueryCache: UIO[Cache] = Cache.empty(Trace.empty)

}
