package caliban.gateway.internal

import caliban.GraphQLResponseContext.{ markRequestError, markServerError, ServerFailure }
import caliban.InputValue.VariableValue
import caliban.execution.{ ExecutionRequest, Field, RequestPreparation }
import caliban.gateway.PhaseHooks.{ Event, Outcome }
import caliban.gateway.internal.OperationCache.Weighted
import caliban.gateway.internal.OperationPreparation._
import caliban.gateway.internal.composition.ComposedGraph.OverrideLabel
import caliban.gateway.internal.planning._
import caliban.gateway.{ errorCode, isInclusionDirective, PhaseHooks }
import caliban.parsing.adt.{ Directive, Directives, Document }
import caliban.schema.RootType
import caliban.validation.Validator
import caliban.wrappers.IncrementalDelivery
import caliban._
import zio.query.Cache
import zio.{ Cause, Exit, IO, Random, Trace, UIO, ZIO }

private[gateway] final class OperationPreparation[-R] private (
  rootType: RootType,
  planner: OperationPlanner,
  security: OperationSecurity,
  cache: OperationCache[CacheKey, CalibanError, CachedOperation[R], R],
  costLimit: Option[(OperationCost, Long)],
  hooks: PhaseHooks[R]
) {

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    withDeferValidations(for {
      document <- RequestPreparation.parse(query)
      _        <- Validator.validate(document, rootType)
    } yield ())

  def prepare(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, Failure, ExecutableOperation] =
    for {
      resolved  <- resolveRequest(request)
      operation <- withDeferValidations(prepareResolved(resolved.request, resolved.cacheable))
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
          hooks.authorization.map(authorize => (event: Event.Authorization) => authorize(event).as(event)),
          Event.Authorization(operation.request, operation.document, operation.executionRequest, requirements),
          AuthorizationFailure
        ) { case PhaseHooks.Denial(reason) => CalibanError.ValidationError(reason, "") }.unit
    }

  private def prepareResolved(
    request: GraphQLRequest,
    cacheable: Boolean
  )(implicit trace: Trace): ZIO[R, Failure, ExecutableOperation] = Configurator.ref.get.flatMap { config =>
    def parse = RequestPreparation.parse(request.query.getOrElse(""))
    if (cacheable) {
      val template = request.copy(variables = None, extensions = None)
      val key      = CacheKey(template, config.copy(queryCache = UnusedQueryCache), None)
      cache.getOrCompute(key)(parse.flatMap(buildCacheEntry(key, _))).mapError(Rejected(_)).flatMap(_(request))
    } else
      parse.tap(RequestPreparation.checkIntrospection(_, request.operationName)).mapError(Rejected(_)).flatMap {
        document =>
          def build(validated: Option[Executable]): ZIO[R, Failure, ExecutableOperation] =
            buildOperation(request, document, validated)((_, execution) => selectAndPlan(request, document, execution))
          pushedDown(document, request.operationName)
            .fold(build(None))(pushed => validateDocument(document).mapError(Rejected(_)) *> build(Some(pushed)))
      }
  }

  private def buildCacheEntry(key: CacheKey, document: Document)(implicit
    trace: Trace
  ): IO[CalibanError, Weighted[CachedOperation[R]]] = {
    val request    = key.request
    val executable = pushedDown(document, request.operationName).getOrElse((_: Map[String, InputValue]) => document)
    for {
      _      <- RequestPreparation.checkIntrospection(document, request.operationName)
      _      <- validateDocument(document)
      cached <-
        if (hasVariableInclusionDirective(document, request.operationName))
          ZIO.succeed(
            Weighted[CachedOperation[R]](
              incoming =>
                buildOperation(incoming, document, Some(executable))((_, execution) =>
                  selectAndPlan(incoming, document, execution)
                ),
              cacheWeight(key)
            )
          )
        else {
          val variables = symbolicVariables(document, request.operationName)
          val executed  = executable(variables)
          val validated = Some((_: Map[String, InputValue]) => executed)

          def planned(execution: ExecutionRequest, activeOverrides: Set[OverrideLabel]) =
            buildPlan(document, execution, activeOverrides).map { plan =>
              val cached: CachedOperation[R] =
                if (variables.isEmpty && !plan.hasVariableReferences)
                  incoming => Exit.succeed(ExecutableOperation(incoming, document, execution, plan))
                else
                  incoming =>
                    buildOperation(incoming, document, validated)((values, _) => Exit.succeed(plan.bind(values)))
              Weighted(cached, cacheWeight(key) + planWeight(plan))
            }

          RequestPreparation
            .prepareParsed(request, executed, variables, rootType, validations = Some(Nil))
            .flatMap { execution =>
              val reached = planner.progressiveOverrides(execution.field.fields)
              if (reached.isEmpty) planned(execution, Set.empty)
              else
                Exit.succeed(
                  Weighted[CachedOperation[R]](
                    incoming =>
                      selectOverrides(incoming, reached).flatMap { active =>
                        cache
                          .getOrCompute(key.copy(activeOverrides = Some(active)))(planned(execution, active))
                          .mapError(Rejected(_))
                          .flatMap(_(incoming))
                      },
                    cacheWeight(key)
                  )
                )
            }
        }
    } yield cached
  }

  /**
   * Cached operations validated their document without variables, so binding checks only the variables. Otherwise the
   * configured validations run once with the coerced variables, and document errors still take precedence over
   * variable errors. With `@defer`, the caller validates the client's document as a cached one is, and the execution
   * request comes from the document that `validated` pushes down with the coerced variables.
   */
  private def buildOperation[R1 <: R](request: GraphQLRequest, document: Document, validated: Option[Executable])(
    getPlan: (Map[String, InputValue], ExecutionRequest) => ZIO[R1, Failure, OperationPlan]
  )(implicit trace: Trace): ZIO[R1, Failure, ExecutableOperation] =
    for {
      variables <- RequestPreparation
                     .coerceVariables(document, request, rootType)
                     .tapError(_ => validateDocument(document).when(validated.isEmpty))
                     .mapError(Rejected(_))
      execution <- RequestPreparation
                     .prepareParsed(
                       request,
                       validated.fold(document)(_(variables)),
                       variables,
                       rootType,
                       validations = if (validated.isEmpty) None else VariableValidation
                     )
                     .mapError(Rejected(_))
      plan      <- getPlan(variables, execution)
    } yield ExecutableOperation(request, document, execution, plan)

  private def validateDocument(document: Document)(implicit trace: Trace): IO[CalibanError, Unit] =
    Configurator.ref.getWith(config => if (config.skipValidation) Exit.unit else Validator.validate(document, rootType))

  private def withDeferValidations[R1, E, A](effect: ZIO[R1, E, A])(implicit trace: Trace): ZIO[R1, E, A] =
    Configurator.ref.locallyWith(config => config.copy(validations = config.validations ::: DeferValidations))(effect)

  private def pushedDown(document: Document, operationName: Option[String]): Option[Executable] =
    if (Deferral.hasDefer(document, operationName)) Some(Deferral.pushDown(document, _)) else None

  private def buildPlan(document: Document, execution: ExecutionRequest, activeOverrides: Set[OverrideLabel])(implicit
    trace: Trace
  ): IO[CalibanError, OperationPlan] =
    ZIO
      .blocking(ZIO.fromEither(planner.plan(document, execution, activeOverrides)))
      .mapError(failure => CalibanError.ValidationError(failure.message, ""))

  private def selectAndPlan(request: GraphQLRequest, document: Document, execution: ExecutionRequest)(implicit
    trace: Trace
  ): ZIO[R, Failure, OperationPlan] =
    selectOverrides(request, planner.progressiveOverrides(execution.field.fields)).flatMap(
      buildPlan(document, execution, _).mapError(Rejected(_))
    )

  private def selectOverrides(
    request: GraphQLRequest,
    labels: Set[OverrideLabel]
  )(implicit trace: Trace): ZIO[R, Failure, Set[OverrideLabel]] =
    if (labels.isEmpty) Exit.succeed(Set.empty)
    else {
      val overrides = labels.toList.sortBy(_.value)
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
      plan.payload.allRoots.foldLeft(0L)((count, fetch) => count + fieldWeight(fetch.downstream)) +
      plan.payload.allEntities.foldLeft(0L)((count, fetch) => count + fieldWeight(fetch.fields)) +
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
      (isInclusionDirective(directive) || directive.name == Directives.Defer) && directive.arguments.values.exists {
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

  private val DeferValidations: List[Validator.QueryValidation] = IncrementalDelivery.deferValidations

  def make[R](
    rootType: RootType,
    planner: OperationPlanner,
    security: OperationSecurity,
    maxOperationCacheWeight: Long,
    costLimit: Option[(OperationCost, Long)],
    hooks: PhaseHooks[R]
  )(implicit trace: Trace): UIO[OperationPreparation[R]] =
    OperationCache
      .make[CacheKey, CalibanError, CachedOperation[R], R](maxOperationCacheWeight, hooks)
      .map(new OperationPreparation(rootType, planner, security, _, costLimit, hooks))

  sealed abstract class Failure(val outcome: Outcome, val mark: UIO[Unit]) { def error: CalibanError }
  final case class Rejected(error: CalibanError) extends Failure(Outcome.RequestError, markRequestError(error))
  final case class Internal(error: CalibanError) extends Failure(Outcome.InternalError, InternalMark)

  private val InternalMark = markServerError(ServerFailure.Internal)

  private val ResolutionFailure              = "Operation resolution failed."
  private val AuthorizationFailure           = "Operation authorization failed."
  private val OverrideLabelResolutionFailure = "Progressive override label resolution failed."
  private val UnsupportedPolicyFailure       = "Operation selects fields guarded by unsupported @policy directives."

  private def runHook[R, Ev](steps: List[Ev => ZIO[R, Throwable, Ev]], event: Ev, failureMessage: String)(
    rejection: PartialFunction[Throwable, CalibanError]
  )(implicit trace: Trace): ZIO[R, Failure, Ev] =
    if (steps.isEmpty) Exit.succeed(event)
    else
      ZIO
        .suspendSucceed(ZIO.foldLeft(steps)(event)((current, step) => step(current)))
        .mapErrorCause { cause =>
          def internal: Failure =
            Internal(CalibanError.ExecutionError(failureMessage, innerThrowable = Some(cause.squash)))
          cause.interruptOption.fold[Cause[Failure]](Cause.fail(cause.failures match {
            case failure :: Nil if cause.defects.isEmpty =>
              rejection.andThen(Rejected(_)).applyOrElse(failure, (_: Throwable) => internal)
            case _                                       => internal
          }))(Cause.interrupt(_))
        }

  private type CachedOperation[-R] = GraphQLRequest => ZIO[R, Failure, ExecutableOperation]

  // The document an operation executes, given its coerced variables.
  private type Executable = Map[String, InputValue] => Document

  private final case class CacheKey(
    request: GraphQLRequest,
    // Reuse function instances across requests. Fresh lambdas cause misses; rebuilding only the list does not.
    config: Configurator.ExecutionConfiguration,
    // None for the entry that selects override labels, Some for the plan of each selected label set.
    activeOverrides: Option[Set[OverrideLabel]]
  )

  // Cache entries never read the query cache, so the key does not depend on it.
  private val UnusedQueryCache: UIO[Cache] = Cache.empty(Trace.empty)

}
