package caliban.gateway.internal.execution

import caliban.ResponseValue.ObjectValue
import caliban.Value.{ NullValue, StringValue }
import caliban.execution.{ ExecutionRequest, Executor, Field }
import caliban.gateway.{ PhaseHooks, TypenameField }
import caliban.gateway.internal.SubscriptionTermination
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.execution.EntityExecutor.{ EntityBatch, EntityLocation }
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
        val remote: URIO[R, GraphQLResponse[CalibanError] => GraphQLResponse[CalibanError]] =
          if (plan.operationType == OperationType.Mutation)
            executeMutations(plan, plan.roots).map(mutations => completeMutations(plan, mutations, _))
          else
            ZIO
              .foreachPar(plan.roots)(executeRoot(plan, _))
              .flatMap(executeEntityFetches(plan, _))
              .map(fetched => completeFetched(plan, fetched, _))
        val introspectionFields                                                             = plan.introspectionFields
        if (introspectionFields.isEmpty) remote.flatMap(complete => observeCompletion(complete(NoLocalResponse)))
        else
          remote.zipPar(executeIntrospection(execution, introspectionFields)).flatMap { case (complete, local) =>
            observeCompletion(complete(local))
          }
    }

  def forSubscription(
    plan: OperationPlan
  )(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, PlanExecutor[R]] = {
    val used = plan.roots.map(_.source.name).toSet ++ plan.entities.map(_.source.name)
    ZIO
      .foreach(subgraphExecutors) { case (name, executor) =>
        (if (used(name)) executor.forSubscription else ZIO.succeed(executor)).map(name -> _)
      }
      .map(new PlanExecutor(graph, _, hooks))
  }

  def subscribe(plan: OperationPlan, resolvedRequest: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]] = {
    val fetch                     = plan.roots.head
    val executor                  = executorFor(fetch.source)
    val (outgoing, restoreErrors) =
      if (plan.passthroughSubgraph.nonEmpty)
        resolvedRequest -> ((errors: List[CalibanError]) => executor.errorPolicy.forFetch(plan.fields, errors))
      else {
        val root = prepareRoot(plan, fetch)
        root.request -> ((errors: List[CalibanError]) => root.restoreErrors(executor.errorPolicy, errors))
      }
    executor
      .subscribe(outgoing)
      .map(_.mapError {
        case error: CalibanError.ExecutionError if SubscriptionTermination.isGatewayError(error) => error
        case error: CalibanError.ExecutionError                                                  =>
          restoreErrors(List(error)).collectFirst { case e: CalibanError.ExecutionError => e }.getOrElse(error)
        case error                                                                               => error
      })
  }

  def executeEvent(plan: OperationPlan, response: GraphQLResponse[CalibanError])(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] = {
    val root        = plan.roots.head
    val errorPolicy = executorFor(root.source).errorPolicy
    if (plan.passthroughSubgraph.nonEmpty)
      ZIO.succeed(completePassthrough(plan, errorPolicy, response.copy(extensions = None)))
    else
      executeEntityFetches(plan, prepareRoot(plan, root).restore(errorPolicy, response) :: Nil)
        .map(completeFetched(plan, _, NoLocalResponse))
  }

  private lazy val introspection: RootSchema[Any] = Introspector.introspect[Any](graph.rootType)
  private val entityExecutor                      = new EntityExecutor[R](graph, executorFor)

  private def executorFor(source: ComposedGraph.Source): SubgraphExecutor[R] =
    subgraphExecutors.getOrElse(source.name, new SubgraphExecutor.Unavailable(source.name))

  private def prepareRoot(plan: OperationPlan, fetch: RootFetch): PreparedRoot =
    plan.executionCache.root(fetch.id) {
      val mapping    = fetch.source.mapping
      val executable = fetch.downstream.map(fetch.source.prepareField(_))
      val downstream = executable.map(mapping.fieldToSource)
      val operation  =
        OperationDefinition(plan.operationType, plan.operationName, Nil, Nil, downstream.map(_.toSelection))
      PreparedRoot(
        fetch,
        GraphQLRequest(query = Some(renderOperation(operation)), operationName = plan.operationName),
        ResponseProjection.compile(fetch.downstream, executable, mapping.typeNames)
      )
    }

  private def observeCompletion(response: => GraphQLResponse[CalibanError])(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] =
    hooks.observeCompletion(ZIO.succeed(response))

  /**
   * Finish each root's dependent entity fetches and response completion before starting the next mutation root:
   * later mutations could change values still being read for the current root.
   * A non-null failure that bubbles to the response root stops the remaining mutation roots.
   */
  private def executeMutations(
    plan: OperationPlan,
    pending: List[RootFetch]
  )(implicit trace: Trace): URIO[R, Mutations] =
    pending match {
      case Nil           => ZIO.succeed(Mutations(Nil, aborted = false))
      case fetch :: tail =>
        executeRoot(plan, fetch).flatMap { root =>
          executeEntityFetches(plan, root :: Nil).flatMap { remote =>
            val errors        = remote.roots.head.errors ::: remote.entityErrors
            val completed     = plan.completion.completeRoot(fetch, remote.roots.head.data, errors)
            val completedRoot = RootResult(fetch, completed.toResponseValue, errors ::: completed.errors)
            if (completed.bubblesNull) ZIO.succeed(Mutations(completedRoot :: Nil, aborted = true))
            else executeMutations(plan, tail).map(next => next.copy(roots = completedRoot :: next.roots))
          }
        }
    }

  private def executeEntityFetches(plan: OperationPlan, roots: List[RootResult])(implicit
    trace: Trace
  ): URIO[R, Fetched] =
    ZIO
      .foldLeft(plan.entityWaves)((Fetched(roots, Nil), List.empty[EntityLocation])) {
        case ((fetched, blocked), wave) =>
          val data = fetched.roots.iterator.map(root => root.fetch.id -> root.data).toMap
          entityExecutor
            .execute(wave.filter(fetch => data.contains(fetch.root)), data, blocked, plan.executionCache)
            .map { result =>
              val patches = result.patches ::: result.blocked.map(_.nullPatch)
              Fetched(
                patchRoots(fetched.roots, patches),
                fetched.entityErrors ::: result.errors
              ) -> (result.blocked ::: blocked)
            }
      }
      .map(_._1)

  private def patchRoots(roots: List[RootResult], patches: List[(FetchId, Patch)]): List[RootResult] = {
    val byRoot = patches.groupMap(_._1)(_._2)
    roots.map(root =>
      byRoot.get(root.fetch.id).fold(root)(patches => root.copy(data = applyPatches(root.data, patches)))
    )
  }

  private def executeRoot(plan: OperationPlan, fetch: RootFetch)(implicit trace: Trace): URIO[R, RootResult] = {
    val prepared = prepareRoot(plan, fetch)
    val executor = executorFor(fetch.source)
    executor
      .execute(prepared.request, plan.operationType)
      .map(prepared.restore(executor.errorPolicy, _))
      .catchAll(_ =>
        ZIO.succeed(RootResult(fetch, RemoteError.nullObject(fetch.client), RemoteError.forFields(fetch.client)))
      )
  }

  private def executeIntrospection(execution: ExecutionRequest, fields: List[Field])(implicit
    trace: Trace
  ): UIO[GraphQLResponse[CalibanError]] =
    Executor.executeRequest(execution.copy(field = execution.field.copy(fields = fields)), introspection.query.plan)

  private def completeFetched(
    plan: OperationPlan,
    fetched: Fetched,
    local: GraphQLResponse[CalibanError]
  ): GraphQLResponse[CalibanError] =
    completeResponse(
      plan,
      GraphQLResponse(
        assemble(plan, fetched.roots, local),
        local.errors ::: fetched.roots.flatMap(_.errors) ::: fetched.entityErrors
      )
    )

  private def completeMutations(
    plan: OperationPlan,
    mutations: Mutations,
    local: GraphQLResponse[CalibanError]
  ): GraphQLResponse[CalibanError] =
    GraphQLResponse(
      if (mutations.aborted) NullValue else assemble(plan, mutations.roots, local),
      local.errors ::: mutations.roots.flatMap(_.errors)
    )

  private def assemble(
    plan: OperationPlan,
    roots: List[RootResult],
    local: GraphQLResponse[CalibanError]
  ): ObjectValue =
    ObjectValue(plan.fields.flatMap { field =>
      val name  = field.aliasedName
      val value =
        if (field.name == TypenameField) Some(StringValue(plan.rootName))
        else
          fieldValue(local.data, name).orElse(
            roots.flatMap(root => fieldValue(root.data, name)).reduceLeftOption(mergeRootValue)
          )
      value.map(name -> _)
    })

  private def fieldValue(data: ResponseValue, name: String): Option[ResponseValue] =
    data match {
      case obj: ObjectValue => Option(obj.getOrNull(name))
      case _                => None
    }

  private def completePassthrough(
    plan: OperationPlan,
    errorPolicy: SubgraphExecutor.ErrorPolicy,
    response: GraphQLResponse[CalibanError]
  ): GraphQLResponse[CalibanError] =
    completeResponse(plan, response.copy(errors = errorPolicy.forFetch(plan.fields, response.errors)))

  private def completeResponse(
    plan: OperationPlan,
    response: GraphQLResponse[CalibanError]
  ): GraphQLResponse[CalibanError] = {
    val completed = plan.completion.complete(response.data, response.errors)
    response.copy(data = completed.toResponseValue, errors = response.errors ::: completed.errors)
  }
}

private[gateway] object PlanExecutor {
  private[execution] final case class RootResult(fetch: RootFetch, data: ResponseValue, errors: List[CalibanError])

  private val NoLocalResponse = GraphQLResponse(ObjectValue.empty, Nil)

  final case class PreparedRoot(fetch: RootFetch, request: GraphQLRequest, projection: ResponseProjection) {
    private[execution] def restore(
      errorPolicy: SubgraphExecutor.ErrorPolicy,
      response: GraphQLResponse[CalibanError]
    ): RootResult =
      RootResult(
        fetch,
        response.data match {
          case NullValue => RemoteError.nullObject(fetch.client)
          case data      => projection(data)
        },
        restoreErrors(errorPolicy, response.errors)
      )

    def restoreErrors(errorPolicy: SubgraphExecutor.ErrorPolicy, errors: List[CalibanError]): List[CalibanError] =
      errorPolicy.forFetch(
        fetch.client,
        errors.map {
          case error: CalibanError.ExecutionError =>
            error.copy(path = projection.path(error.path), locationInfo = None)
          case error                              => error
        }
      )
  }

  private[execution] def renderOperation(operation: OperationDefinition): String =
    DocumentRenderer.renderCompact(Document(operation :: Nil, SourceMapper.empty))

  private final case class Fetched(roots: List[RootResult], entityErrors: List[CalibanError])

  private final case class Mutations(roots: List[RootResult], aborted: Boolean)
}

private[gateway] final class PlanExecutionCache {
  def root(id: FetchId)(prepare: => PlanExecutor.PreparedRoot): PlanExecutor.PreparedRoot =
    roots.computeIfAbsent(id, _ => prepare)

  private[execution] def federationLookup(batch: EntityBatch)(
    build: (EntityFetch, Map[ContextualArgument, InputValue]) => EntityLookup.FederationVariant
  ): EntityLookup.FederationVariant =
    lookup(federationLookups, batch)(build)

  private[execution] def graphqlLookup(batch: EntityBatch)(
    build: (EntityFetch, Map[ContextualArgument, InputValue]) => EntityLookup.GraphQLVariant
  ): EntityLookup.GraphQLVariant =
    lookup(graphqlLookups, batch)(build)

  private val roots             = new ConcurrentHashMap[FetchId, PlanExecutor.PreparedRoot]
  private val federationLookups = new ConcurrentHashMap[FetchId, EntityLookup.FederationVariant]
  private val graphqlLookups    = new ConcurrentHashMap[FetchId, EntityLookup.GraphQLVariant]

  // Context argument values are injected into the lookup, so only context-free batches are shared.
  private def lookup[A <: AnyRef](cache: ConcurrentHashMap[FetchId, A], batch: EntityBatch)(
    build: (EntityFetch, Map[ContextualArgument, InputValue]) => A
  ): A =
    if (batch.contexts.isEmpty) cache.computeIfAbsent(batch.fetch.id, _ => build(batch.fetch, batch.contexts))
    else build(batch.fetch, batch.contexts)
}
