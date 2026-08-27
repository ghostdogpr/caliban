package caliban.gateway.internal.execution

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, PathValue, ResponseValue }
import caliban.execution.{ ExecutionRequest, Executor, Field }
import caliban.gateway.GatewayWrapper
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.execution.EntityExecutor.EntityResult
import caliban.gateway.internal.execution.PlanExecutor._
import caliban.gateway.internal.execution.ResponseMerge._
import caliban.gateway.internal.planning.OperationPlan
import caliban.gateway.internal.planning.OperationPlan._
import caliban.introspection.Introspector
import caliban.parsing.adt.{ Document, OperationType }
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.SourceMapper
import caliban.rendering.DocumentRenderer
import caliban.ResponseValue.ObjectValue
import caliban.schema.{ RootSchema, RootType }
import caliban.Value.{ NullValue, StringValue }
import zio.{ Trace, URIO, ZIO }

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

/**
 * Runs a prepared fetch graph; request admission and preparation stay in the interpreter.
 */
private[gateway] final class PlanExecutor[-R](
  graph: ComposedGraph,
  subgraphExecutors: Map[String, SubgraphExecutor[R]],
  wrapper: GatewayWrapper[R]
) {
  private val rootType: RootType             = graph.rootType
  private val introspection: RootSchema[Any] = Introspector.introspect[Any](rootType)
  private val responseMappings               = graph.mappings.map { case (subgraphName, mapping) =>
    subgraphName -> new ResponseMapping(mapping)
  }
  private val entityExecutor                 = new EntityExecutor[R](graph, subgraphExecutors, responseMappings)

  private def preparedRoot(
    fetch: RootFetch,
    operationType: OperationType,
    operationName: Option[String],
    cache: PlanExecutionCache
  ): PreparedRoot =
    PlanExecutionCache.memoize(cache.roots, fetch.id) {
      val mapping          = graph.mapping(fetch.source)
      val executable       = fetch.downstream.map(graph.executableField(fetch.source, _))
      val downstream       = mapping.fold(executable)(value => executable.map(value.rootFieldToSource))
      val responseToClient = responseMappings.get(fetch.source).map(_.rootResponseMapper(executable))
      val operation        = OperationDefinition(
        operationType,
        operationName,
        Nil,
        Nil,
        downstream.map(_.toSelection)
      )
      PreparedRoot(
        executable,
        responseToClient,
        DocumentRenderer.renderCompact(Document(operation :: Nil, SourceMapper.empty)),
        ResponseMapping.responseNameRestorer(fetch.downstream, executable)
      )
    }

  def execute(
    prepared: PreparedPlan,
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest
  )(implicit trace: Trace): ZIO[R, Nothing, GraphQLResponse[CalibanError]] = {
    val plan = prepared.plan
    plan.passthroughSubgraph match {
      case Some(subgraphName) =>
        subgraphExecutors.get(subgraphName) match {
          case Some(executor) =>
            executor
              .execute(resolvedRequest, plan.operation)
              .flatMap(response =>
                wrapper.observeCompletion(
                  ZIO.succeed(
                    completeSourceResponse(
                      prepared.completion,
                      plan.fields,
                      response,
                      executor.errorPolicy.passthrough(plan.fields, response.errors)
                    )
                  )
                )
              )
              .catchAll(_ => wrapper.observeCompletion(ZIO.succeed(singleSourceFailure(prepared))))
          case None           =>
            wrapper.observeCompletion(ZIO.succeed(singleSourceFailure(prepared)))
        }
      case None               =>
        val introspectionFields = plan.introspectionFields
        if (introspectionFields.isEmpty)
          executeRemote(prepared, execution, resolvedRequest)
            .flatMap(remote =>
              wrapper.observeCompletion(
                ZIO.succeed(assemble(prepared, remote, GraphQLResponse(ObjectValue.empty, Nil)))
              )
            )
        else
          executeRemote(prepared, execution, resolvedRequest)
            .zipPar(executeIntrospection(execution, introspectionFields))
            .flatMap { case (remote, local) =>
              wrapper.observeCompletion(ZIO.succeed(assemble(prepared, remote, local)))
            }
    }

  }

  private def executeRemote(
    prepared: PreparedPlan,
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest
  )(implicit trace: Trace): ZIO[R, Nothing, RemoteExecution] = {
    val plan = prepared.plan
    plan.operation match {
      case OperationType.Query        =>
        executeRoots(plan.roots, execution, resolvedRequest, prepared.cache).flatMap { roots =>
          val rootValues = roots.iterator.map(result => result.fetch.id -> result.response.data).toMap
          executeEntities(
            plan.entities,
            rootValues,
            plan.roots.iterator.map(_.id).toSet,
            Map.empty,
            resolvedRequest,
            prepared.cache
          ).map { entityExecution =>
            val updated = roots.map(result =>
              result.copy(
                response = result.response.copy(
                  data = entityExecution.roots.getOrElse(result.fetch.id, result.response.data)
                )
              )
            )
            RemoteExecution(updated, entityExecution.results)
          }
        }
      case OperationType.Mutation     =>
        executeMutations(prepared, plan.roots, execution, resolvedRequest)
      case OperationType.Subscription => ZIO.succeed(RemoteExecution(Nil, Nil))
    }

  }

  /**
   * Finish each root's dependent entity fetches and response completion before starting the next mutation root:
   * later mutations could change values still being read for the current root.
   * A non-null failure that bubbles to the response root stops the remaining mutation roots.
   */
  private def executeMutations(
    prepared: PreparedPlan,
    pending: List[RootFetch],
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest
  )(implicit trace: Trace): ZIO[R, Nothing, RemoteExecution] = {
    val plan = prepared.plan
    pending match {
      case Nil           => ZIO.succeed(RemoteExecution(Nil, Nil))
      case fetch :: tail =>
        executeRoot(fetch, execution, resolvedRequest, prepared.cache).flatMap { root =>
          val rootData = mutationRootData(fetch, root.response.data)
          val current  = plan.entities.filter(_.root == fetch.id)
          executeEntities(
            current,
            Map(fetch.id -> rootData),
            Set(fetch.id),
            Map.empty,
            resolvedRequest,
            prepared.cache
          ).flatMap { entityExecution =>
            val updated       = root.copy(
              response = root.response.copy(
                data = entityExecution.roots.getOrElse(fetch.id, rootData)
              )
            )
            val errors        = updated.response.errors ::: entityExecution.results.flatMap(_.errors)
            val completed     = prepared.completion.complete(fetch.client, updated.response.data, errors)
            val completedRoot = updated.copy(
              response = updated.response.copy(data = completed.toResponseValue)
            )
            if (completed.bubblesNull)
              ZIO.succeed(
                RemoteExecution(
                  completedRoot :: Nil,
                  entityExecution.results,
                  completed.errors,
                  aborted = true
                )
              )
            else
              executeMutations(prepared, tail, execution, resolvedRequest).map(next =>
                next.copy(
                  roots = completedRoot :: next.roots,
                  entities = entityExecution.results ::: next.entities,
                  completionErrors = completed.errors ::: next.completionErrors
                )
              )
          }
        }
    }

  }

  private def mutationRootData(fetch: RootFetch, data: ResponseValue): ResponseValue =
    data match {
      case NullValue => ObjectValue(fetch.client.map(field => field.aliasedName -> NullValue))
      case value     => value
    }

  private def executeEntities(
    pending: List[EntityFetch],
    roots: Map[FetchId, ResponseValue],
    completed: Set[FetchId],
    blocked: Map[FetchId, Set[List[PathValue]]],
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, EntityExecution] =
    if (pending.isEmpty) ZIO.succeed(EntityExecution(roots, Nil))
    else {
      val ready = pending.filter(fetch => fetch.dependencies.forall(completed.contains))
      if (ready.isEmpty)
        ZIO.succeed(
          EntityExecution(
            roots,
            EntityResult(
              Nil,
              List(CalibanError.ExecutionError("Entity routing dependency cycle detected.")),
              Set.empty,
              Map.empty
            ) :: Nil
          )
        )
      else
        entityExecutor.execute(ready, roots, blocked, resolvedRequest, cache).flatMap { results =>
          val patchesByRoot =
            mutable.LinkedHashMap.empty[FetchId, mutable.ListBuffer[(List[PathValue], ResponseValue)]]
          results.foreach(
            _.patches.foreach(patch =>
              patchesByRoot.getOrElseUpdate(patch.fetch.root, mutable.ListBuffer.empty) += (patch.path -> patch.value)
            )
          )
          val patchedRoots  = patchesByRoot.foldLeft(roots) { case (values, (rootId, patches)) =>
            values.get(rootId) match {
              case Some(root) => values.updated(rootId, applyPatches(root, patches.toList))
              case None       => values
            }
          }
          val nextRoots     =
            if (!results.exists(_.blocked.nonEmpty)) patchedRoots
            else {
              val blockedByRoot =
                mutable.LinkedHashMap.empty[FetchId, mutable.ListBuffer[(List[PathValue], ResponseValue)]]
              val fetchesById   = pending.iterator.map(fetch => fetch.id -> fetch).toMap
              results.foreach(
                _.blocked.foreach { case (fetchId, paths) =>
                  fetchesById.get(fetchId).foreach { fetch =>
                    val patch = ObjectValue(fetch.fields.map(field => field.aliasedName -> NullValue))
                    paths.foreach(path =>
                      blockedByRoot.getOrElseUpdate(fetch.root, mutable.ListBuffer.empty) += (path -> patch)
                    )
                  }
                }
              )
              blockedByRoot.foldLeft(patchedRoots) { case (values, (rootId, patches)) =>
                values.get(rootId) match {
                  case Some(root) =>
                    values.updated(
                      rootId,
                      patches.foldLeft(root) { case (value, (path, patch)) => mergeMissingAt(value, path, patch) }
                    )
                  case None       => values
                }
              }
            }
          val nextCompleted = completed ++ results.iterator.flatMap(_.completed)
          val nextBlocked   = results.flatMap(_.blocked).foldLeft(blocked) { case (values, (fetchId, paths)) =>
            values.updated(fetchId, values.getOrElse(fetchId, Set.empty) ++ paths)
          }
          val remaining     = pending.filterNot(fetch => nextCompleted.contains(fetch.id))
          executeEntities(remaining, nextRoots, nextCompleted, nextBlocked, resolvedRequest, cache)
            .map(next => next.copy(results = results ::: next.results))
        }
    }

  private def executeRoots(
    fetches: List[RootFetch],
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): ZIO[R, Nothing, List[RootResult]] =
    fetches match {
      case fetch :: Nil => executeRoot(fetch, execution, resolvedRequest, cache).map(_ :: Nil)
      case _            => ZIO.foreachPar(fetches)(executeRoot(_, execution, resolvedRequest, cache))
    }

  private def executeRoot(
    fetch: RootFetch,
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): ZIO[R, Nothing, RootResult] = {
    val prepared = preparedRoot(fetch, execution.operationType, execution.operationName, cache)
    val request  = GraphQLRequest(
      query = Some(prepared.query),
      operationName = execution.operationName,
      extensions = resolvedRequest.extensions
    )

    subgraphExecutors.get(fetch.source) match {
      case Some(executor) =>
        executor
          .execute(request, execution.operationType)
          .map { response =>
            val translated = prepared.responseToClient.fold(response)(_(response))
            val errors     = translated.errors.map {
              case error: CalibanError.ExecutionError =>
                error
                  .copy(path = ResponseMapping.restoreResponsePath(fetch.downstream, prepared.executable, error.path))
              case error                              => error
            }
            val restored   = prepared.restorer match {
              case Some(mappings) => ResponseMapping.restoreResponseNames(mappings, translated.data)
              case None           => translated.data
            }
            RootResult(
              fetch,
              translated.copy(
                data = restored,
                errors = executor.errorPolicy.routed(fetch.client, errors)
              )
            )
          }
          .catchAll(_ => ZIO.succeed(RootResult(fetch, rootFailure(fetch))))
      case None           => ZIO.succeed(RootResult(fetch, rootFailure(fetch)))
    }
  }

  private def executeIntrospection(
    execution: ExecutionRequest,
    fields: List[Field]
  )(implicit trace: Trace): ZIO[Any, Nothing, GraphQLResponse[CalibanError]] =
    Executor.executeRequest(
      execution.copy(field = execution.field.copy(fields = fields)),
      introspection.query.plan
    )

  private def assemble(
    prepared: PreparedPlan,
    remote: RemoteExecution,
    local: GraphQLResponse[CalibanError]
  ): GraphQLResponse[CalibanError] = {
    val plan        = prepared.plan
    val roots       = remote.roots
    val entities    = remote.entities
    val localValues = responseFields(local).toMap
    val rootValues  =
      roots.flatMap(result => responseFields(result.response)).foldLeft(Map.empty[String, ResponseValue]) {
        case (values, (name, value)) => values.updated(name, values.get(name).fold(value)(mergeRootValue(_, value)))
      }
    val data        = ObjectValue(plan.fields.map { field =>
      val value =
        if (field.name == "__typename") StringValue(plan.rootName)
        else
          localValues
            .get(field.aliasedName)
            .orElse(rootValues.get(field.aliasedName))
            .getOrElse(NullValue)
      field.aliasedName -> value
    })
    val errors      =
      local.errors ::: roots.flatMap(_.response.errors) ::: entities.flatMap(_.errors) ::: remote.completionErrors
    if (remote.aborted) GraphQLResponse(NullValue, errors)
    else if (plan.operation == OperationType.Mutation) GraphQLResponse(data, errors)
    else {
      val completed = prepared.completion.complete(plan.fields, data, errors)
      GraphQLResponse(completed.toResponseValue, errors ::: completed.errors)
    }
  }

  private def responseFields(response: GraphQLResponse[CalibanError]): List[(String, ResponseValue)] =
    response.data match {
      case ObjectValue(fields) => fields
      case _                   => Nil
    }

  private def completeSourceResponse(
    completion: ResponseCompletion,
    fields: List[Field],
    response: GraphQLResponse[CalibanError],
    errors: List[CalibanError]
  ): GraphQLResponse[CalibanError] = {
    val completed = completion.complete(fields, response.data, errors)
    response.copy(data = completed.toResponseValue, errors = errors ::: completed.errors)
  }

  private def rootFailure(fetch: RootFetch): GraphQLResponse[CalibanError] =
    GraphQLResponse(
      ObjectValue(fetch.client.map(field => field.aliasedName -> NullValue)),
      fetch.client.map(field => RemoteError.at(List(PathValue.Key(field.aliasedName))))
    )

  private def singleSourceFailure(prepared: PreparedPlan): GraphQLResponse[CalibanError] = {
    val plan   = prepared.plan
    val data   = ObjectValue(plan.fields.map(field => field.aliasedName -> NullValue))
    val errors = plan.fields.map(field => RemoteError.at(List(PathValue.Key(field.aliasedName))))
    completeSourceResponse(prepared.completion, plan.fields, GraphQLResponse(data, errors), errors)
  }
}

private[gateway] object PlanExecutor {
  private final case class RootResult(fetch: RootFetch, response: GraphQLResponse[CalibanError])

  private[internal] final case class PreparedRoot(
    executable: List[Field],
    responseToClient: Option[GraphQLResponse[CalibanError] => GraphQLResponse[CalibanError]],
    query: String,
    restorer: Option[Map[String, ResponseMapping.ResponseNameMapping]]
  )

  private final case class EntityExecution(roots: Map[FetchId, ResponseValue], results: List[EntityResult])

  private final case class RemoteExecution(
    roots: List[RootResult],
    entities: List[EntityResult],
    completionErrors: List[CalibanError] = Nil,
    aborted: Boolean = false
  )

}

private[internal] object PlanExecutionCache {

  /**
   * Racing callers may compute the same entry more than once, so computations must be side-effect-free
   * and produce equivalent results for the same fetch ID within this plan's cache.
   */
  def memoize[A <: AnyRef](cache: ConcurrentHashMap[FetchId, A], id: FetchId)(compute: => A): A = {
    val cached = cache.get(id)
    if (cached ne null) cached
    else {
      val created = compute
      cache.put(id, created)
      created
    }
  }
}

private[internal] final class PlanExecutionCache {
  val roots: ConcurrentHashMap[FetchId, PlanExecutor.PreparedRoot]              = new ConcurrentHashMap
  val groupKeys: ConcurrentHashMap[FetchId, OperationPlan.EntityGroupKey]       = new ConcurrentHashMap
  val lookups: ConcurrentHashMap[FetchId, EntityExecutor.PreparedLookup]        = new ConcurrentHashMap
  val identities: ConcurrentHashMap[FetchId, EntityExecutor.IdentitySelections] = new ConcurrentHashMap
}

/**
 * Execution-only memoization, reused with the cached plan and replaced when variables are bound.
 */
private[gateway] final class PreparedPlan(val plan: OperationPlan) {
  lazy val cache: PlanExecutionCache                                 = new PlanExecutionCache
  lazy val completion: ResponseCompletion                            = new ResponseCompletion(plan.typenameSelections)
  def operation: OperationType                                       = plan.operation
  def render: String                                                 = plan.render
  def hasVariableReferences: Boolean                                 = plan.hasVariableReferences
  def bind(variables: Map[String, caliban.InputValue]): PreparedPlan = new PreparedPlan(plan.bind(variables))
}
