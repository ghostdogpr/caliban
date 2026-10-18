package caliban.gateway.internal.execution

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ NullValue, StringValue }
import caliban.execution.{ isMetaField, ExecutionRequest, Executor, Field }
import caliban.gateway.{ responseNames, PhaseHooks, TypenameField }
import caliban.gateway.internal.SubscriptionTermination
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.execution.EntityExecutor.{ collectEntities, EntityLocation }
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
import scala.collection.mutable.ListBuffer

/**
 * Runs a fetch graph; request lifetimes and preparation stay in the interpreter.
 */
private[gateway] final class PlanExecutor[-R](
  graph: ComposedGraph,
  subgraphExecutors: Map[String, SubgraphExecutor[R]],
  hooks: PhaseHooks[R]
) {

  /**
   * Executes the initial payload. Deferred fragments run when the returned stream runs, each one within the given
   * guard.
   */
  def execute(plan: OperationPlan, execution: ExecutionRequest, resolvedRequest: GraphQLRequest)(implicit
    trace: Trace
  ): URIO[R, Executed[R]] =
    plan.passthroughSubgraph match {
      case Some(source) =>
        val executor = executorFor(source)
        executor
          .execute(resolvedRequest, plan.operationType)
          .catchAll(_ =>
            ZIO.succeed(GraphQLResponse(RemoteError.nullObject(plan.fields), RemoteError.forFields(plan.fields)))
          )
          .flatMap(response => observeCompletion(completePassthrough(plan, executor.errorPolicy, response)))
          .map(Executed[R](_, None))
      case None         =>
        val batches             =
          if (plan.operationType == OperationType.Mutation) executeMutations(plan, plan.preparedRoots)
          else
            ZIO
              .foreachPar(plan.preparedRoots)(executeRoot(plan, _))
              .flatMap(completeRoots(plan, plan.completion, _))
              .map(_ :: Nil)
        val introspectionFields = plan.introspectionFields

        def finish(done: List[Batch], local: GraphQLResponse[CalibanError]): URIO[R, Executed[R]] =
          observeCompletion(assemble(plan, done, local)).map { response =>
            val deferred =
              if (plan.payload.deferred.isEmpty) None
              else
                Some { (guard: Guard) =>
                  val data    = done.iterator.flatMap(_.roots).map(root => root.id -> root.data).toMap
                  val fetched = Fetched(data, Nil, done.flatMap(_.blocked))
                  runFragments(plan, plan.preparedFragments, fetched, View(Nil, response.data) :: Nil, guard)
                }
            Executed(response, deferred)
          }

        if (introspectionFields.isEmpty) batches.flatMap(finish(_, NoLocalResponse))
        else
          batches.zipPar(executeIntrospection(execution, introspectionFields)).flatMap { case (done, local) =>
            finish(done, local)
          }
    }

  private def runFragments(
    plan: OperationPlan,
    fragments: List[PreparedFragment],
    fetched: Fetched,
    views: List[View],
    guard: Guard
  )(implicit trace: Trace): ZStream[R, Nothing, List[Incremental[CalibanError]]] =
    ZStream.mergeAllUnbounded()(fragments.map(runFragment(plan, _, fetched, views, guard)): _*)

  /**
   * Runs a deferred fragment once the payload containing its parent objects is complete, then the fragments nested in
   * it. Objects nulled in an earlier payload get no deferred payload.
   */
  private def runFragment(
    plan: OperationPlan,
    prepared: PreparedFragment,
    fetched: Fetched,
    views: List[View],
    guard: Guard
  )(implicit trace: Trace): ZStream[R, Nothing, List[Incremental[CalibanError]]] = {
    val instances = views.flatMap(_.at(prepared.below))
    if (instances.isEmpty) ZStream.empty
    else
      ZStream.unwrap(
        guard(resolveFragment(plan, prepared, fetched, instances, guard))(timedOut(prepared, instances, _))
      )
  }

  private def resolveFragment(
    plan: OperationPlan,
    prepared: PreparedFragment,
    fetched: Fetched,
    instances: List[View],
    guard: Guard
  )(implicit trace: Trace): URIO[R, ZStream[R, Nothing, List[Incremental[CalibanError]]]] =
    for {
      roots <- ZIO.foreachPar(prepared.roots)(executeRoot(plan, _))
      kept   = instances.map(_.path).toSet
      data   = fetched.data.map { case (id, value) =>
                 val found = ListBuffer.empty[(List[PathValue], ResponseValue)]
                 collectEntities(value, prepared.fragment.point.path.toList, Nil, found)
                 // Objects an earlier payload nulled get no deferred fetch.
                 id -> applyPatches(
                   value,
                   found.toList.collect { case (path, _) if !kept(path) => path -> Overwrite(NullValue) }
                 )
               }
      done  <- executeEntityFetches(
                 prepared.fragment.payload.entityWaves,
                 fetched.copy(data = data ++ roots.map(root => root.id -> root.data), entityErrors = Nil)
               )
    } yield {
      val fragment          = prepared.fragment
      val values            = done.data.values.toList
      // At the root, only the fragment's own root fields are merged.
      val names             =
        fragment.point.path.headOption.fold(fragment.payload.fields.map(_.aliasedName).distinct)(_ :: Nil)
      val root              = ObjectValue(names.flatMap(name => rootValue(values, name).map(name -> _)))
      val errors            = roots.flatMap(_.errors) ::: done.entityErrors
      // An error under no instance goes to the first one.
      val owned             =
        errors.groupBy(error => instances.indexWhere(instance => errorPath(error).startsWith(instance.path)).max(0))
      val (payloads, views) = instances.zipWithIndex.map { case (instance, index) =>
        val path       = instance.path
        val own        = owned.getOrElse(index, Nil)
        val raw        = rawValue(root, path).getOrElse(NullValue)
        val completion = prepared.completion.complete(raw, path, own)
        completion.toResponseValue match {
          // No field of the fragment applies to this object: nested fragments still read the parent payload.
          case ObjectValue(Nil) => (None, instance)
          case value            =>
            val merged = if (value eq NullValue) NullValue else mergeRootValue(instance.value, value)
            (Some(prepared.payload(path, value, own ::: completion.errors)), instance.copy(value = merged))
        }
      }.unzip
      val ready             = payloads.flatten
      val payload           = if (ready.isEmpty) ZStream.empty else ZStream.succeed(ready)
      payload ++ runFragments(plan, prepared.deferred, done, views, guard)
    }

  private def timedOut(
    prepared: PreparedFragment,
    instances: List[View],
    error: CalibanError.ExecutionError
  ): ZStream[Any, Nothing, List[Incremental[CalibanError]]] =
    ZStream.succeed(instances.map { instance =>
      prepared.payload(instance.path, NullValue, error.copy(path = instance.path) :: Nil)
    })

  def subscription(plan: OperationPlan, resolvedRequest: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, Subscription[R]] =
    plan.preparedRoots match {
      case Nil       => ZIO.fail(SubgraphExecutor.InvalidRequest)
      case root :: _ =>
        val used = plan.payload.roots.map(_.source.name).toSet ++ plan.payload.entities.map(_.target.source.name)
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
    executeEntityFetches(
      plan.payload.entityWaves,
      Fetched(roots.iterator.map(root => root.id -> root.data).toMap, Nil, Nil)
    ).map { fetched =>
      val patched = roots.map(fetched.patched)
      new Batch(completion, patched, patched.flatMap(_.errors) ::: fetched.entityErrors, fetched.blocked)
    }

  private def executeEntityFetches(waves: List[List[EntityFetch]], initial: Fetched)(implicit
    trace: Trace
  ): URIO[R, Fetched] =
    ZIO.foldLeft(waves)(initial) { (fetched, wave) =>
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

  private final class Batch(
    completion: ResponseCompletion,
    val roots: List[RootResult],
    val errors: List[CalibanError],
    val blocked: List[EntityLocation]
  ) {
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

  /**
   * Bounds the work of a deferred fragment, returning `onTimeout` applied to the timeout error when the request deadline
   * expires first.
   */
  trait Guard {
    def apply[R, A](effect: URIO[R, A])(onTimeout: CalibanError.ExecutionError => A): URIO[R, A]
  }

  /**
   * The initial response, and the payloads of its deferred fragments when the operation defers any.
   */
  final case class Executed[-R](
    response: GraphQLResponse[CalibanError],
    deferred: Option[Guard => ZStream[R, Nothing, List[Incremental[CalibanError]]]]
  )

  final case class PreparedFragment(
    fragment: DeferredFragment,
    completion: ResponseCompletion.FragmentCompletion,
    roots: List[PreparedRoot],
    deferred: List[PreparedFragment],
    below: List[String]
  ) {
    def payload(path: List[PathValue], data: ResponseValue, errors: List[CalibanError]): Incremental[CalibanError] =
      Incremental.Defer(data, ListValue(path), errors, fragment.point.label)
  }

  private[internal] def prepareFragments(plan: OperationPlan): List[PreparedFragment] = {
    def prepare(fragment: DeferredFragment, parent: Vector[String]): PreparedFragment =
      PreparedFragment(
        fragment,
        ResponseCompletion.forFragment(plan, fragment),
        fragment.payload.roots.map(prepareRoot(plan, _)),
        fragment.payload.deferred.map(prepare(_, fragment.point.path)),
        fragment.point.path.drop(parent.size).toList
      )
    plan.payload.deferred.map(prepare(_, Vector.empty))
  }

  /**
   * Completed objects of a payload: `path` locates `value` in the response.
   */
  private final case class View(path: List[PathValue], value: ResponseValue) {

    /**
     * The non-null objects at `below`, a path without indices relative to this view, with their concrete paths.
     */
    def at(below: List[String]): List[View] = {
      val found = ListBuffer.empty[(List[PathValue], ResponseValue)]
      collectEntities(value, below, path.reverse, found)
      found.toList.collect { case (path, obj: ObjectValue) => View(path, obj) }
    }
  }

  /**
   * The fetched object at a concrete response path below `root`.
   */
  private def rawValue(root: ResponseValue, path: List[PathValue]): Option[ResponseValue] =
    path.foldLeft(Option(root)) {
      case (Some(obj: ObjectValue), PathValue.Key(key))      => Option(obj.getOrNull(key))
      case (Some(ListValue(values)), PathValue.Index(index)) => values.lift(index)
      case _                                                 => None
    }

  private def errorPath(error: CalibanError): List[PathValue] =
    error match {
      case error: CalibanError.ExecutionError => error.path
      case _                                  => Nil
    }

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
