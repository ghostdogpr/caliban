package caliban.gateway.internal.execution

import caliban.ResponseValue.ObjectValue
import caliban.Value.{ NullValue, StringValue }
import caliban.execution.{ ExecutionRequest, Executor, Field }
import caliban.gateway.{ PhaseHooks, TypenameField }
import caliban.gateway.internal.SubscriptionTermination
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.execution.EntityExecutor.{ unionBlocked, EntityResult, FetchPaths }
import caliban.gateway.internal.execution.PlanExecutor._
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

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

/**
 * Runs a prepared fetch graph; request admission and preparation stay in the interpreter.
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
      case Some(subgraphName) =>
        val executor = subgraphExecutors(subgraphName)
        executor
          .execute(resolvedRequest, plan.operation)
          .flatMap(response =>
            hooks.observeCompletion(
              ZIO.succeed(
                completePassthrough(
                  plan.completion,
                  plan.fields,
                  response,
                  executor.errorPolicy.passthrough(plan.fields, response.errors)
                )
              )
            )
          )
          .catchAll(_ => hooks.observeCompletion(ZIO.succeed(passthroughFailure(plan))))
      case None               =>
        val introspectionFields = plan.introspectionFields
        if (introspectionFields.isEmpty)
          executeRemote(plan, execution, resolvedRequest)
            .flatMap(remote => hooks.observeCompletion(ZIO.succeed(assemble(plan, remote, NoLocalResponse))))
        else
          executeRemote(plan, execution, resolvedRequest)
            .zipPar(executeIntrospection(execution, introspectionFields))
            .flatMap { case (remote, local) => hooks.observeCompletion(ZIO.succeed(assemble(plan, remote, local))) }
    }

  def forSubscription(
    plan: OperationPlan
  )(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, PlanExecutor[R]] = {
    val used = plan.roots.map(_.source).toSet ++ plan.entities.map(_.source)
    ZIO
      .foreach(subgraphExecutors) { case (name, executor) =>
        (if (used(name)) executor.forSubscription else ZIO.succeed(executor)).map(name -> _)
      }
      .map(new PlanExecutor(graph, _, hooks))
  }

  def subscribe(plan: OperationPlan, execution: ExecutionRequest, resolvedRequest: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]] = {
    val fetch    = plan.roots.head
    val executor = subgraphExecutors(fetch.source)

    def open(
      outgoing: GraphQLRequest,
      restore: GraphQLResponse[CalibanError] => GraphQLResponse[CalibanError]
    ): ZIO[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]] =
      executor
        .subscribe(outgoing)
        .map(_.map(restore).mapError {
          case error: CalibanError.ExecutionError if SubscriptionTermination.isGatewayError(error) => error
          case error: CalibanError.ExecutionError                                                  =>
            restore(GraphQLResponse(NullValue, List(error))).errors.collectFirst {
              case e: CalibanError.ExecutionError =>
                e
            }.getOrElse(error)
          case error                                                                               => error
        })

    if (plan.passthroughSubgraph.nonEmpty)
      open(
        resolvedRequest,
        response => response.copy(errors = executor.errorPolicy.passthrough(plan.fields, response.errors))
      )
    else {
      val root     = prepareRoot(fetch, OperationType.Subscription, execution.operationName, plan.executionCache)
      val outgoing = GraphQLRequest(query = Some(root.query), operationName = execution.operationName)
      open(outgoing, response => restoreRoot(fetch, root, executor.errorPolicy, response).response)
    }
  }

  def executeEvent(
    plan: OperationPlan,
    request: GraphQLRequest,
    response: GraphQLResponse[CalibanError]
  )(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    if (response.data == NullValue) ZIO.succeed(response.copy(extensions = None))
    else
      executeEntityFetches(
        plan.entities,
        RootResult(plan.roots.head, response.copy(extensions = None)) :: Nil,
        request,
        plan.executionCache
      ).map(assemble(plan, _, NoLocalResponse))

  private lazy val introspection: RootSchema[Any] = Introspector.introspect[Any](graph.rootType)
  private val entityExecutor                      = new EntityExecutor[R](graph, subgraphExecutors)

  private def prepareRoot(
    fetch: RootFetch,
    operationType: OperationType,
    operationName: Option[String],
    cache: PlanExecutionCache
  ): PreparedRoot =
    cache.root(fetch.id) {
      val mapping    = graph.schemaMapping(fetch.source)
      val executable = fetch.selections.map(graph.prepareField(fetch.source, _))
      val downstream = executable.map(mapping.fieldToSource)
      val operation  = OperationDefinition(operationType, operationName, Nil, Nil, downstream.map(_.toSelection))
      PreparedRoot(
        DocumentRenderer.renderCompact(Document(operation :: Nil, SourceMapper.empty)),
        mapping.responseProjection(fetch.selections, executable)
      )
    }

  private def executeRemote(
    plan: OperationPlan,
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest
  )(implicit trace: Trace): URIO[R, RemoteExecution] =
    if (plan.operation == OperationType.Mutation) executeMutations(plan, plan.roots, execution, resolvedRequest)
    else
      executeRoots(plan.roots, execution, resolvedRequest, plan.executionCache)
        .flatMap(executeEntityFetches(plan.entities, _, resolvedRequest, plan.executionCache))

  /**
   * Finish each root's dependent entity fetches and response completion before starting the next mutation root:
   * later mutations could change values still being read for the current root.
   * A non-null failure that bubbles to the response root stops the remaining mutation roots.
   */
  private def executeMutations(
    plan: OperationPlan,
    pending: List[RootFetch],
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest
  )(implicit trace: Trace): URIO[R, RemoteExecution.Completed] =
    pending match {
      case Nil           => ZIO.succeed(RemoteExecution.Completed(Nil, Nil, Nil, aborted = false))
      case fetch :: tail =>
        executeRoot(fetch, execution, resolvedRequest, plan.executionCache).flatMap { root =>
          val rootData = mutationRootData(fetch, root.response.data)
          val entities = plan.entities.filter(_.root == fetch.id)
          executeEntityFetches(
            entities,
            root.copy(response = root.response.copy(data = rootData)) :: Nil,
            resolvedRequest,
            plan.executionCache
          ).flatMap { remote =>
            val updated       = remote.roots.head
            val errors        = updated.response.errors ::: remote.entityResults.flatMap(_.errors)
            val completed     = plan.completion.complete(fetch.client, updated.response.data, errors)
            val completedRoot = updated.copy(response = updated.response.copy(data = completed.toResponseValue))
            if (completed.bubblesNull)
              ZIO.succeed(
                RemoteExecution.Completed(completedRoot :: Nil, remote.entityResults, completed.errors, aborted = true)
              )
            else
              executeMutations(plan, tail, execution, resolvedRequest).map(next =>
                RemoteExecution.Completed(
                  completedRoot :: next.roots,
                  remote.entityResults ::: next.entityResults,
                  completed.errors ::: next.completionErrors,
                  next.aborted
                )
              )
          }
        }
    }

  private def mutationRootData(fetch: RootFetch, data: ResponseValue): ResponseValue =
    data match {
      case NullValue => RemoteError.nullObject(fetch.client)
      case value     => value
    }

  private def executeEntityFetches(
    fetches: List[EntityFetch],
    roots: List[RootResult],
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, RemoteExecution.Fetched] = {
    val rootValues = roots.iterator.map(result => result.fetch.id -> result.response.data).toMap
    executeEntityWaves(
      fetches,
      rootValues,
      roots.iterator.map(_.fetch.id).toSet,
      Map.empty,
      resolvedRequest,
      cache
    ).map { execution =>
      val updated = roots.map(result =>
        result.copy(response =
          result.response.copy(data = execution.roots.getOrElse(result.fetch.id, result.response.data))
        )
      )
      RemoteExecution.Fetched(updated, execution.results)
    }
  }

  private def executeEntityWaves(
    pending: List[EntityFetch],
    roots: Map[FetchId, ResponseValue],
    completed: Set[FetchId],
    blocked: FetchPaths,
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, EntityExecution] =
    if (pending.isEmpty) ZIO.succeed(EntityExecution(roots, Nil))
    else {
      val ready = readyFetches(pending, completed, cache)
      if (ready.isEmpty)
        ZIO.succeed(
          EntityExecution(
            roots,
            EntityResult(
              Nil,
              List(CalibanError.ExecutionError("Entity routing dependency cycle detected.")),
              Map.empty,
              Map.empty
            ) :: Nil
          )
        )
      else
        entityExecutor.execute(ready, roots, blocked, resolvedRequest, cache).flatMap { results =>
          val nextRoots     = fillUnfetched(applyEntityPatches(roots, results), pending, results)
          val nextCompleted = completed ++ ready.iterator.map(_.id)
          val nextBlocked   = unionBlocked(blocked, results.flatMap(_.blocked))
          val remaining     = pending.filterNot(fetch => nextCompleted.contains(fetch.id))
          executeEntityWaves(remaining, nextRoots, nextCompleted, nextBlocked, resolvedRequest, cache)
            .map(next => next.copy(results = results ::: next.results))
        }
    }

  private def readyFetches(
    pending: List[EntityFetch],
    completed: Set[FetchId],
    cache: PlanExecutionCache
  ): List[EntityFetch] = {
    val (available, waiting) = pending.partition(fetch => fetch.dependencies.forall(completed.contains))
    // Wait for compatible fetches to batch together, unless they depend on an available fetch.
    val scheduled            = available.filterNot { fetch =>
      val key = cache.groupKey(fetch)
      waiting.exists(cache.groupKey(_) == key) && waiting.forall(!_.dependencies.contains(fetch.id))
    }
    if (scheduled.isEmpty) available else scheduled
  }

  private def applyEntityPatches(
    roots: Map[FetchId, ResponseValue],
    results: List[EntityResult]
  ): Map[FetchId, ResponseValue] = {
    val patchesByRoot = mutable.LinkedHashMap.empty[FetchId, mutable.ListBuffer[Patch]]
    results.foreach(
      _.patches.foreach(patch =>
        patchesByRoot.getOrElseUpdate(patch.fetch.root, mutable.ListBuffer.empty) += (patch.path -> patch.value)
      )
    )
    patchRoots(roots, patchesByRoot)(applyPatches)
  }

  private def fillUnfetched(
    roots: Map[FetchId, ResponseValue],
    pending: List[EntityFetch],
    results: List[EntityResult]
  ): Map[FetchId, ResponseValue] = {
    val unfetched = results.flatMap(result => result.blocked.toList ::: result.unmatched.toList)
    if (unfetched.isEmpty) roots
    else {
      val patchesByRoot = mutable.LinkedHashMap.empty[FetchId, mutable.ListBuffer[Patch]]
      val fetchesById   = pending.iterator.map(fetch => fetch.id -> fetch).toMap
      unfetched.foreach { case (fetchId, paths) =>
        fetchesById.get(fetchId).foreach { fetch =>
          val patch = RemoteError.nullObject(fetch.fields)
          paths.foreach(path => patchesByRoot.getOrElseUpdate(fetch.root, mutable.ListBuffer.empty) += (path -> patch))
        }
      }
      patchRoots(roots, patchesByRoot)(fillMissing)
    }
  }

  private def patchRoots(
    roots: Map[FetchId, ResponseValue],
    patchesByRoot: Iterable[(FetchId, mutable.ListBuffer[Patch])]
  )(patch: (ResponseValue, List[Patch]) => ResponseValue): Map[FetchId, ResponseValue] =
    patchesByRoot.foldLeft(roots) { case (values, (rootId, patches)) =>
      values.get(rootId) match {
        case Some(root) => values.updated(rootId, patch(root, patches.toList))
        case None       => values
      }
    }

  private def executeRoots(
    fetches: List[RootFetch],
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, List[RootResult]] =
    fetches match {
      case fetch :: Nil => executeRoot(fetch, execution, resolvedRequest, cache).map(_ :: Nil)
      case _            => ZIO.foreachPar(fetches)(executeRoot(_, execution, resolvedRequest, cache))
    }

  private def executeRoot(
    fetch: RootFetch,
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, RootResult] = {
    val prepared = prepareRoot(fetch, execution.operationType, execution.operationName, cache)
    val request  = GraphQLRequest(
      query = Some(prepared.query),
      operationName = execution.operationName,
      extensions = resolvedRequest.extensions
    )

    val executor = subgraphExecutors(fetch.source)
    executor
      .execute(request, execution.operationType)
      .map(response => restoreRoot(fetch, prepared, executor.errorPolicy, response))
      .catchAll(_ => ZIO.succeed(RootResult(fetch, rootFailure(fetch))))
  }

  private def restoreRoot(
    fetch: RootFetch,
    prepared: PreparedRoot,
    errorPolicy: SubgraphExecutor.ErrorPolicy,
    response: GraphQLResponse[CalibanError]
  ): RootResult = {
    val errors = response.errors.map {
      case error: CalibanError.ExecutionError => error.copy(path = prepared.projection.path(error.path))
      case error                              => error
    }
    RootResult(
      fetch,
      response.copy(
        data = prepared.projection(response.data),
        errors = errorPolicy.forFetch(fetch.client, errors)
      )
    )
  }

  private def executeIntrospection(execution: ExecutionRequest, fields: List[Field])(implicit
    trace: Trace
  ): UIO[GraphQLResponse[CalibanError]] =
    Executor.executeRequest(execution.copy(field = execution.field.copy(fields = fields)), introspection.query.plan)

  private def assemble(
    plan: OperationPlan,
    remote: RemoteExecution,
    local: GraphQLResponse[CalibanError]
  ): GraphQLResponse[CalibanError] = {
    val roots       = remote.roots
    val localValues = responseFields(local).toMap
    val rootValues  =
      roots.flatMap(result => responseFields(result.response)).foldLeft(Map.empty[String, ResponseValue]) {
        case (values, (name, value)) => values.updated(name, values.get(name).fold(value)(mergeRootValue(_, value)))
      }
    val data        = ObjectValue(plan.fields.map { field =>
      val value =
        if (field.name == TypenameField) StringValue(plan.rootName)
        else
          localValues
            .get(field.aliasedName)
            .orElse(rootValues.get(field.aliasedName))
            .getOrElse(NullValue)
      field.aliasedName -> value
    })
    val errors      = local.errors ::: roots.flatMap(_.response.errors) ::: remote.entityResults.flatMap(_.errors)
    remote match {
      case RemoteExecution.Fetched(_, _)                           =>
        val completed = plan.completion.complete(plan.fields, data, errors)
        GraphQLResponse(completed.toResponseValue, errors ::: completed.errors)
      case RemoteExecution.Completed(_, _, completionErrors, true) =>
        GraphQLResponse(NullValue, errors ::: completionErrors)
      case RemoteExecution.Completed(_, _, completionErrors, _)    =>
        GraphQLResponse(data, errors ::: completionErrors)
    }
  }

  private def responseFields(response: GraphQLResponse[CalibanError]): List[(String, ResponseValue)] =
    response.data match {
      case ObjectValue(fields) => fields
      case _                   => Nil
    }

  private def completePassthrough(
    completion: ResponseCompletion,
    fields: List[Field],
    response: GraphQLResponse[CalibanError],
    errors: List[CalibanError]
  ): GraphQLResponse[CalibanError] = {
    val completed = completion.complete(fields, response.data, errors)
    response.copy(data = completed.toResponseValue, errors = errors ::: completed.errors)
  }

  private def rootFailure(fetch: RootFetch): GraphQLResponse[CalibanError] =
    GraphQLResponse(RemoteError.nullObject(fetch.client), RemoteError.forFields(fetch.client))

  private def passthroughFailure(plan: OperationPlan): GraphQLResponse[CalibanError] = {
    val data   = RemoteError.nullObject(plan.fields)
    val errors = RemoteError.forFields(plan.fields)
    completePassthrough(plan.completion, plan.fields, GraphQLResponse(data, errors), errors)
  }
}

private[gateway] object PlanExecutor {
  private final case class RootResult(fetch: RootFetch, response: GraphQLResponse[CalibanError])

  private val NoLocalResponse = GraphQLResponse(ObjectValue.empty, Nil)

  final case class PreparedRoot(query: String, projection: ResponseProjection)

  private final case class EntityExecution(roots: Map[FetchId, ResponseValue], results: List[EntityResult])

  private sealed trait RemoteExecution {
    def roots: List[RootResult]
    def entityResults: List[EntityResult]
  }

  private object RemoteExecution {
    final case class Fetched(roots: List[RootResult], entityResults: List[EntityResult]) extends RemoteExecution

    final case class Completed(
      roots: List[RootResult],
      entityResults: List[EntityResult],
      completionErrors: List[CalibanError],
      aborted: Boolean
    ) extends RemoteExecution
  }
}

private[gateway] final class PlanExecutionCache {
  def root(id: FetchId)(prepare: => PlanExecutor.PreparedRoot): PlanExecutor.PreparedRoot =
    memoize(roots, id)(prepare)

  private[execution] def lookup(id: FetchId)(prepare: => EntityLookup.PreparedLookup): EntityLookup.PreparedLookup =
    memoize(lookups, id)(prepare)

  private[execution] def groupKey(fetch: EntityFetch): EntityGroupKey =
    memoize(groupKeys, fetch.id)(entityGroupKey(fetch))

  private val roots     = new ConcurrentHashMap[FetchId, PlanExecutor.PreparedRoot]
  private val groupKeys = new ConcurrentHashMap[FetchId, EntityGroupKey]
  private val lookups   = new ConcurrentHashMap[FetchId, EntityLookup.PreparedLookup]

  /**
   * Racing callers may compute the same entry more than once, so computations must be side-effect-free
   * and produce equivalent results for the same fetch ID within this plan's cache.
   */
  private def memoize[A <: AnyRef](cache: ConcurrentHashMap[FetchId, A], id: FetchId)(compute: => A): A = {
    val cached = cache.get(id)
    if (cached ne null) cached
    else {
      val created = compute
      cache.put(id, created)
      created
    }
  }
}
