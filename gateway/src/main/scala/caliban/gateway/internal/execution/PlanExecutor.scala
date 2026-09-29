package caliban.gateway.internal.execution

import caliban.ResponseValue.ObjectValue
import caliban.Value.{ NullValue, StringValue }
import caliban.execution.{ isMetaField, ExecutionRequest, Executor, Field }
import caliban.gateway.{ responseNames, PhaseHooks, TypenameField }
import caliban.gateway.internal.SubscriptionTermination
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.execution.EntityExecutor.EntityLocation
import caliban.gateway.internal.execution.PlanExecutor._
import caliban.gateway.internal.execution.ResponseCompletion.Completion
import caliban.gateway.internal.execution.ResponseMerge._
import caliban.gateway.internal.planning.OperationPlan
import caliban.gateway.internal.planning.OperationPlan._
import caliban.introspection.Introspector
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.{ Document, OperationType }
import caliban.rendering.DocumentRenderer
import caliban.schema.RootSchema
import caliban._
import zio.stream.ZStream
import zio.{ Scope, Trace, UIO, URIO, ZIO }

import scala.collection.compat._

/**
 * Runs a fetch graph; request lifetimes and preparation stay in the interpreter.
 */
private[gateway] final class PlanExecutor[-R](
  graph: ComposedGraph,
  subgraphExecutors: Map[String, SubgraphExecutor[R]],
  hooks: PhaseHooks[R]
) {
  def execute(plan: OperationPlan, execution: ExecutionRequest, resolvedRequest: GraphQLRequest)(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] =
    plan.passthroughSubgraph match {
      case Some(source) =>
        val executor = executorFor(source)
        executor
          .execute(resolvedRequest, plan.operationType)
          .catchAll(_ =>
            ZIO.succeed(GraphQLResponse(RemoteError.nullObject(plan.fields), RemoteError.forFields(plan.fields)))
          )
          .flatMap(response => observeCompletion(completePassthrough(plan, executor.errorPolicy, response)))
      case None         =>
        val batches             =
          if (plan.operationType == OperationType.Mutation) executeMutations(plan, plan.preparedRoots)
          else
            ZIO
              .foreachPar(plan.preparedRoots)(executeRoot(plan, _))
              .flatMap(completeRoots(plan, plan.completion, _))
              .map(_ :: Nil)
        val introspectionFields = plan.introspectionFields
        if (introspectionFields.isEmpty)
          batches.flatMap(done => observeCompletion(assemble(plan, done, NoLocalResponse)))
        else
          batches.zipPar(executeIntrospection(execution, introspectionFields)).flatMap { case (done, local) =>
            observeCompletion(assemble(plan, done, local))
          }
    }

  def subscription(plan: OperationPlan, resolvedRequest: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, Subscription[R]] =
    plan.preparedRoots match {
      case Nil       => ZIO.fail(SubgraphExecutor.InvalidRequest)
      case root :: _ =>
        val used = plan.roots.map(_.source.name).toSet ++ plan.entities.map(_.target.source.name)
        ZIO
          .foreach(subgraphExecutors) { case (name, executor) =>
            (if (used(name)) executor.forSubscription else ZIO.succeed(executor)).map(name -> _)
          }
          .map(new PlanExecutor(graph, _, hooks).bind(plan, root, resolvedRequest))
    }

  private def bind(plan: OperationPlan, root: PreparedRoot, resolvedRequest: GraphQLRequest)(implicit
    trace: Trace
  ): Subscription[R] = {
    val executor = executorFor(root.fetch.source)
    val policy   = executor.errorPolicy

    def open(outgoing: GraphQLRequest)(restoreErrors: List[CalibanError] => List[CalibanError]) =
      executor
        .subscribe(outgoing)
        .map(_.mapError {
          case error: SubscriptionTermination.Error => error
          case error: CalibanError.ExecutionError   =>
            restoreErrors(List(error)).collectFirst { case e: CalibanError.ExecutionError => e }.getOrElse(error)
          case error                                => error
        })

    if (plan.passthroughSubgraph.nonEmpty)
      Subscription[R](
        open(resolvedRequest)(policy.forFetch(plan.fields, _)),
        response => ZIO.succeed(completePassthrough(plan, policy, response.copy(extensions = None)))
      )
    else
      Subscription[R](
        open(root.request)(root.restoreErrors(policy, _)),
        response =>
          completeRoots(plan, plan.completion, root.restore(policy, response) :: Nil).map(batch =>
            assemble(plan, batch :: Nil, NoLocalResponse)
          )
      )
  }

  private lazy val introspection: RootSchema[Any] = Introspector.introspect[Any](graph.rootType)
  private val entityExecutor                      = new EntityExecutor[R](graph, executorFor)

  private def executorFor(source: ComposedGraph.Source): SubgraphExecutor[R] =
    subgraphExecutors.getOrElse(source.name, new SubgraphExecutor.Unavailable(source.name))

  private def observeCompletion(response: => GraphQLResponse[CalibanError])(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] =
    hooks.completion.run(PhaseHooks.Event.Completion)(ZIO.succeed(response))(PhaseHooks.Result.classifyResponse(_))

  /**
   * Finish each root's dependent entity fetches and response completion before starting the next mutation root:
   * later mutations could change values still being read for the current root.
   * A non-null failure that bubbles to the response root stops the remaining mutation roots.
   */
  private def executeMutations(
    plan: OperationPlan,
    pending: List[PreparedRoot]
  )(implicit trace: Trace): URIO[R, List[Batch]] =
    pending match {
      case Nil              => ZIO.succeed(Nil)
      case prepared :: tail =>
        executeRoot(plan, prepared).flatMap(root => completeRoots(plan, prepared.completion, root :: Nil)).flatMap {
          batch =>
            if (batch.completed.bubblesNull) ZIO.succeed(batch :: Nil)
            else executeMutations(plan, tail).map(batch :: _)
        }
    }

  private def completeRoots(plan: OperationPlan, completion: ResponseCompletion, roots: List[RootResult])(implicit
    trace: Trace
  ): URIO[R, Batch] =
    executeEntityFetches(plan, roots).map { fetched =>
      val patched = roots.map(fetched.patched)
      new Batch(completion, patched, patched.flatMap(_.errors) ::: fetched.entityErrors)
    }

  private def executeEntityFetches(plan: OperationPlan, roots: List[RootResult])(implicit
    trace: Trace
  ): URIO[R, Fetched] =
    ZIO.foldLeft(plan.entityWaves)(Fetched(roots.iterator.map(root => root.id -> root.data).toMap, Nil, Nil)) {
      (fetched, wave) =>
        entityExecutor
          .execute(wave.filter(fetch => fetched.data.contains(fetch.root)), fetched.data, fetched.blocked)
          .map { result =>
            val patches = (result.patches ::: result.blocked.map(_.nullPatch)).groupMap(_._1)(_._2)
            Fetched(
              fetched.data.map { case (id, value) => id -> patches.get(id).fold(value)(applyPatches(value, _)) },
              fetched.entityErrors ::: result.errors,
              result.blocked ::: fetched.blocked
            )
          }
    }

  private def executeRoot(plan: OperationPlan, prepared: PreparedRoot)(implicit trace: Trace): URIO[R, RootResult] = {
    val fetch    = prepared.fetch
    val executor = executorFor(fetch.source)
    executor
      .execute(prepared.request, plan.operationType)
      .map(prepared.restore(executor.errorPolicy, _))
      .catchAll(_ =>
        ZIO.succeed(
          RootResult(prepared, RemoteError.nullObject(prepared.client), RemoteError.forFields(prepared.client))
        )
      )
  }

  private def executeIntrospection(execution: ExecutionRequest, fields: List[Field])(implicit
    trace: Trace
  ): UIO[GraphQLResponse[CalibanError]] =
    Executor.executeRequest(execution.copy(field = execution.field.copy(fields = fields)), introspection.query.plan)

  private def assemble(
    plan: OperationPlan,
    batches: List[Batch],
    local: GraphQLResponse[CalibanError]
  ): GraphQLResponse[CalibanError] = {
    val errors = local.errors ::: batches.flatMap(batch => batch.errors ::: batch.completed.errors)
    batches match {
      case _ if batches.exists(_.completed.bubblesNull) => GraphQLResponse(NullValue, errors)
      case batch :: Nil if !plan.hasLocalFields         => GraphQLResponse(batch.completed.toResponseValue, errors)
      case _                                            =>
        val values = batches.map(_.completed.toResponseValue)
        GraphQLResponse(
          ObjectValue(plan.fields.flatMap { field =>
            val name  = field.aliasedName
            val value =
              if (field.name == TypenameField) Some(StringValue(plan.rootName))
              else fieldValue(local.data, name).orElse(rootValue(values, name))
            value.map(name -> _)
          }),
          errors
        )
    }
  }

  private def completePassthrough(
    plan: OperationPlan,
    errorPolicy: SubgraphExecutor.ErrorPolicy,
    response: GraphQLResponse[CalibanError]
  ): GraphQLResponse[CalibanError] = {
    val errors    = errorPolicy.forFetch(plan.fields, response.errors)
    val completed = plan.completion.complete(response.data, errors)
    response.copy(data = completed.toResponseValue, errors = errors ::: completed.errors)
  }
}

private[gateway] object PlanExecutor {
  private[execution] final case class RootResult(
    prepared: PreparedRoot,
    data: ResponseValue,
    errors: List[CalibanError]
  ) {
    def id: FetchId = prepared.fetch.id
  }

  private final class Batch(completion: ResponseCompletion, roots: List[RootResult], val errors: List[CalibanError]) {
    lazy val completed: Completion = roots match {
      case RootResult(_, data: ObjectValue, _) :: Nil => completion.complete(data, errors)
      case _                                          =>
        val values = roots.map(_.data)
        completion.complete(
          ObjectValue(completion.names.flatMap(name => rootValue(values, name).map(name -> _))),
          errors
        )
    }
  }

  private def rootValue(values: List[ResponseValue], name: String): Option[ResponseValue] =
    values.flatMap(fieldValue(_, name)).reduceLeftOption(mergeRootValue)

  private def fieldValue(data: ResponseValue, name: String): Option[ResponseValue] =
    data match {
      case obj: ObjectValue => Option(obj.getOrNull(name))
      case _                => None
    }

  private val NoLocalResponse = GraphQLResponse(ObjectValue.empty, Nil)

  final case class Subscription[-R](
    open: ZIO[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]],
    process: GraphQLResponse[CalibanError] => URIO[R, GraphQLResponse[CalibanError]]
  )

  final case class PreparedRoot(
    fetch: RootFetch,
    client: List[Field],
    completion: ResponseCompletion,
    request: GraphQLRequest,
    projection: ResponseProjection
  ) {
    private[execution] def restore(
      errorPolicy: SubgraphExecutor.ErrorPolicy,
      response: GraphQLResponse[CalibanError]
    ): RootResult =
      RootResult(
        this,
        response.data match {
          case NullValue => RemoteError.nullObject(client)
          case data      => projection(data)
        },
        restoreErrors(errorPolicy, response.errors)
      )

    def restoreErrors(errorPolicy: SubgraphExecutor.ErrorPolicy, errors: List[CalibanError]): List[CalibanError] =
      errorPolicy.forFetch(
        client,
        errors.map {
          case error: CalibanError.ExecutionError =>
            error.copy(path = projection.path(error.path), locationInfo = None)
          case error                              => error
        }
      )
  }

  private[internal] def prepareRoot(plan: OperationPlan, fetch: RootFetch): PreparedRoot = {
    val mapping    = fetch.source.mapping
    val executable = fetch.source.prepareFields(plan.rootName, fetch.downstream)
    val downstream = executable.map(mapping.fieldToSource)
    val operation  =
      OperationDefinition(plan.operationType, plan.operationName, Nil, Nil, downstream.map(_.toSelection))
    val names      = responseNames(fetch.downstream)
    PreparedRoot(
      fetch,
      plan.fields.filter(field => !isMetaField(field) && names(field.aliasedName)),
      plan.completion.only(names),
      GraphQLRequest(query = Some(renderOperation(operation)), operationName = plan.operationName),
      ResponseProjection.compile(fetch.downstream, executable, mapping.typeNames)
    )
  }

  private[execution] def renderOperation(operation: OperationDefinition): String =
    DocumentRenderer.renderCompact(Document(operation :: Nil, SourceMapper.empty))

  private final case class Fetched(
    data: Map[FetchId, ResponseValue],
    entityErrors: List[CalibanError],
    blocked: List[EntityLocation]
  ) {
    def patched(root: RootResult): RootResult = {
      val value = data.getOrElse(root.id, root.data)
      if (value eq root.data) root else root.copy(data = value)
    }
  }
}
