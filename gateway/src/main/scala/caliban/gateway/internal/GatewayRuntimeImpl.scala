package caliban.gateway.internal

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue }
import caliban.execution.{ ExecutionRequest, Executor, Field }
import caliban.gateway.{ GatewayRuntime, GatewayWrapper }
import caliban.gateway.GatewayWrapper.{ Event, Outcome, Result }
import caliban.gateway.internal.EntityExecutor.EntityResult
import caliban.gateway.internal.GatewayRuntimeImpl._
import caliban.gateway.internal.OperationPlanner._
import caliban.introspection.Introspector
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.{ Document, OperationType }
import caliban.rendering.DocumentRenderer
import caliban.schema.{ RootSchema, RootType }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, GraphQLResponseContext, PathValue, ResponseValue }
import caliban.GraphQLResponseContext.ServerFailure
import zio.{ Exit, IO, Trace, URIO, ZIO }

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable

private[gateway] final class GatewayRuntimeImpl[-R](
  graph: ComposedGraph,
  sources: Map[String, GraphQLSource[R]],
  operations: OperationPreparation[R],
  control: RuntimeControl,
  wrapper: GatewayWrapper[R]
) extends GatewayRuntime[R] {

  private val rootType: RootType             = graph.rootType
  private val introspection: RootSchema[Any] = Introspector.introspect[Any](rootType)
  private val entityExecutor                 = new EntityExecutor[R](graph, sources)

  private def preparedRoot(
    route: RootRoute,
    operationType: OperationType,
    operationName: Option[String],
    caches: PlanCaches
  ): PreparedRoot = {
    val cached = caches.roots.get(route.id)
    if (cached != null && cached.operationType == operationType && cached.operationName == operationName) cached
    else {
      val mapping    = graph.mapping(route.source)
      val executable = route.downstream.map(graph.executableField(route.source, _))
      val downstream = mapping.fold(executable)(value => executable.map(value.rootFieldToSource))
      val operation  = OperationDefinition(
        operationType,
        operationName,
        Nil,
        Nil,
        downstream.map(_.toSelection)
      )
      val prepared   = PreparedRoot(
        operationType,
        operationName,
        executable,
        DocumentRenderer.renderCompact(Document(operation :: Nil, SourceMapper.empty)),
        graph.responseNameRestorer(route.downstream, executable)
      )
      caches.roots.put(route.id, prepared)
      prepared
    }
  }

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    control.runRequest(operations.check(query))(ZIO.fail(requestShutdownError)).flatMap {
      case Some(_) => ZIO.unit
      case None    => ZIO.fail(requestTimeoutError)
    }

  def status(implicit trace: Trace): zio.UIO[GatewayRuntime.Status] =
    operations.cacheStatus.flatMap(control.status)

  def explain(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, String] =
    control
      .runRequest(operations.prepare(request).map(prepared => renderPlan(prepared.plan)))(
        ZIO.fail(requestShutdownError)
      )
      .flatMap {
        case Some(value) => ZIO.succeed(value)
        case None        => ZIO.fail(requestTimeoutError)
      }

  def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    if (wrapper.enabled) executeObservedRequest(request)
    else
      control
        .runRequest(
          operations
            .prepare(request)
            .foldZIO(
              error => GraphQLResponseContext.markRequestError(error) *> Executor.fail(error),
              prepared =>
                GraphQLResponseContext.markExecuted *>
                  executePlan(prepared.plan, prepared.executionRequest, prepared.request)
            )
        )(
          GraphQLResponseContext
            .markServerError(ServerFailure.Unavailable)
            .as(Some(requestShutdownResponse))
        )
        .flatMap {
          case Some(response) => ZIO.succeed(response)
          case None           =>
            GraphQLResponseContext.markServerError(ServerFailure.TimedOut).as(requestTimeoutResponse)
        }

  private def executeObservedRequest(request: GraphQLRequest)(implicit
    trace: Trace
  ): URIO[R, GraphQLResponse[CalibanError]] = {
    val execution =
      wrapper
        .wrap(Event.Routing)(operations.prepare(request))(
          Result.fromExit(_)(
            _ => Result(Outcome.Success),
            _ => Result(Outcome.RequestError)
          )
        )
        .foldZIO(
          error =>
            observeCompletion(GraphQLResponseContext.markRequestError(error) *> Executor.fail(error))
              .map(RequestResult(_, Outcome.RequestError, None)),
          prepared =>
            (GraphQLResponseContext.markExecuted *>
              executePlan(prepared.plan, prepared.executionRequest, prepared.request)).map { response =>
              RequestResult(
                response,
                if (response.errors.isEmpty) Outcome.Success else Outcome.GraphQLError,
                Some(prepared.plan.operation)
              )
            }
        )

    control
      .runObservedRequest(wrapper, Event.Request(request.operationName))(execution)(
        wrapper.wrap(Event.Completion)(
          GraphQLResponseContext
            .markServerError(ServerFailure.TimedOut)
            .as(RequestResult(requestTimeoutResponse, Outcome.Timeout, None))
        )(
          Result.fromExit(_)(
            value => Result(value.outcome, errorCount = value.response.errors.size),
            _ => Result(Outcome.InternalError)
          )
        )
      )(
        wrapper.wrap(Event.Completion)(
          GraphQLResponseContext
            .markServerError(ServerFailure.Unavailable)
            .as(RequestResult(requestShutdownResponse, Outcome.Http5xx, None))
        )(
          Result.fromExit(_)(
            value => Result(value.outcome, errorCount = value.response.errors.size),
            _ => Result(Outcome.InternalError)
          )
        )
      )(classifyRequestResult)
      .map(_.response)
  }

  private def classifyRequestResult(exit: Exit[Nothing, RequestResult]): Result =
    Result.fromExit(exit)(
      result => Result(result.outcome, result.operationType, result.response.errors.size),
      _ => Result(Outcome.InternalError)
    )

  private def executePlan(
    plan: OperationPlan,
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest
  )(implicit trace: Trace): ZIO[R, Nothing, GraphQLResponse[CalibanError]] =
    plan.passthrough match {
      case Some(source) =>
        sources.get(source) match {
          case Some(source) =>
            source
              .execute(resolvedRequest, plan.operation)
              .flatMap(response =>
                observeCompletion(
                  ZIO.succeed(
                    completeSourceResponse(
                      plan.fields,
                      response,
                      source.errorPolicy.passthrough(plan.fields, response.errors)
                    )
                  )
                )
              )
              .catchAll(_ => observeCompletion(ZIO.succeed(singleSourceFailure(plan))))
          case None         =>
            observeCompletion(ZIO.succeed(singleSourceFailure(plan)))
        }
      case None         =>
        val introspectionFields = plan.introspectionFields
        if (introspectionFields.isEmpty)
          executeRemote(plan, execution, resolvedRequest)
            .flatMap(remote =>
              observeCompletion(
                ZIO.succeed(assemble(plan, remote, GraphQLResponse(ObjectValue.empty, Nil)))
              )
            )
        else
          executeRemote(plan, execution, resolvedRequest)
            .zipPar(executeIntrospection(execution, introspectionFields))
            .flatMap { case (remote, local) =>
              observeCompletion(ZIO.succeed(assemble(plan, remote, local)))
            }
    }

  private def observeCompletion[R0, E](effect: ZIO[R0, E, GraphQLResponse[CalibanError]])(implicit
    trace: Trace
  ): ZIO[R with R0, E, GraphQLResponse[CalibanError]] =
    if (!wrapper.enabled) effect
    else
      wrapper.wrap(Event.Completion)(effect)(
        Result.fromExit(_)(Result.fromResponse, _ => Result(Outcome.InternalError))
      )

  private def executeRemote(
    plan: OperationPlan,
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest
  )(implicit trace: Trace): ZIO[R, Nothing, RemoteExecution] =
    plan.operation match {
      case OperationType.Query        =>
        executeRoots(plan.roots, execution, resolvedRequest, plan.caches).flatMap { roots =>
          val rootValues = roots.iterator.map(result => result.route.id -> result.response.data).toMap
          executeEntities(
            plan.entities,
            rootValues,
            plan.roots.iterator.map(_.id).toSet,
            Map.empty,
            resolvedRequest,
            plan.caches
          ).map { entityExecution =>
            val updated = roots.map(result =>
              result.copy(
                response = result.response.copy(
                  data = entityExecution.roots.getOrElse(result.route.id, result.response.data)
                )
              )
            )
            RemoteExecution(updated, entityExecution.results)
          }
        }
      case OperationType.Mutation     =>
        executeMutations(plan, plan.roots, execution, resolvedRequest)
      case OperationType.Subscription => ZIO.succeed(RemoteExecution(Nil, Nil))
    }

  private def executeMutations(
    plan: OperationPlan,
    pending: List[RootRoute],
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest
  )(implicit trace: Trace): ZIO[R, Nothing, RemoteExecution] =
    pending match {
      case Nil           => ZIO.succeed(RemoteExecution(Nil, Nil))
      case route :: tail =>
        executeRoot(route, execution, resolvedRequest, plan.caches).flatMap { root =>
          val rootData = mutationRootData(route, root.response.data)
          val current  = plan.entities.filter(_.root == route.id)
          executeEntities(
            current,
            Map(route.id -> rootData),
            Set(route.id),
            Map.empty,
            resolvedRequest,
            plan.caches
          ).flatMap { entityExecution =>
            val updated       = root.copy(
              response = root.response.copy(
                data = entityExecution.roots.getOrElse(route.id, rootData)
              )
            )
            val errors        = updated.response.errors ::: entityExecution.results.flatMap(_.errors)
            val completed     = completeObject(
              route.client,
              updated.response.data,
              Nil,
              errors,
              plan.runtimeTypes
            )
            val completedRoot = updated.copy(
              response = updated.response.copy(data = completed.value.getOrElse(NullValue))
            )
            if (completed.value.isEmpty)
              ZIO.succeed(
                RemoteExecution(
                  completedRoot :: Nil,
                  entityExecution.results,
                  completed.errors,
                  aborted = true
                )
              )
            else
              executeMutations(plan, tail, execution, resolvedRequest).map(next =>
                next.copy(
                  roots = completedRoot :: next.roots,
                  entities = entityExecution.results ::: next.entities,
                  completionErrors = completed.errors ::: next.completionErrors
                )
              )
          }
        }
    }

  private def mutationRootData(route: RootRoute, data: ResponseValue): ResponseValue =
    data match {
      case NullValue => ObjectValue(route.client.map(field => field.aliasedName -> NullValue))
      case value     => value
    }

  private def executeEntities(
    pending: List[EntityRoute],
    roots: Map[RouteId, ResponseValue],
    completed: Set[RouteId],
    blocked: Map[RouteId, Set[List[PathValue]]],
    resolvedRequest: GraphQLRequest,
    caches: PlanCaches
  )(implicit trace: Trace): URIO[R, EntityExecution] =
    if (pending.isEmpty) ZIO.succeed(EntityExecution(roots, Nil))
    else {
      val ready = pending.filter(route => route.dependencies.forall(completed.contains))
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
        entityExecutor.execute(ready, roots, blocked, resolvedRequest, caches).flatMap { results =>
          val patchesByRoot =
            mutable.LinkedHashMap.empty[RouteId, mutable.ListBuffer[(List[PathValue], ResponseValue)]]
          results.foreach(
            _.patches.foreach(patch =>
              patchesByRoot.getOrElseUpdate(patch.route.root, mutable.ListBuffer.empty) += (patch.path -> patch.value)
            )
          )
          val nextRoots     = patchesByRoot.foldLeft(roots) { case (values, (rootId, patches)) =>
            values.get(rootId) match {
              case Some(root) => values.updated(rootId, applyPatches(root, patches.toList))
              case None       => values
            }
          }
          val nextCompleted = completed ++ results.iterator.flatMap(_.completed)
          val nextBlocked   = results.flatMap(_.blocked).foldLeft(blocked) { case (values, (route, paths)) =>
            values.updated(route, values.getOrElse(route, Set.empty) ++ paths)
          }
          val remaining     = pending.filterNot(route => nextCompleted.contains(route.id))
          executeEntities(remaining, nextRoots, nextCompleted, nextBlocked, resolvedRequest, caches)
            .map(next => next.copy(results = results ::: next.results))
        }
    }

  private def executeRoots(
    routes: List[RootRoute],
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest,
    caches: PlanCaches
  )(implicit trace: Trace): ZIO[R, Nothing, List[RootResult]] =
    routes match {
      case route :: Nil => executeRoot(route, execution, resolvedRequest, caches).map(_ :: Nil)
      case _            => ZIO.foreachPar(routes)(executeRoot(_, execution, resolvedRequest, caches))
    }

  private def executeRoot(
    route: RootRoute,
    execution: ExecutionRequest,
    resolvedRequest: GraphQLRequest,
    caches: PlanCaches
  )(implicit trace: Trace): ZIO[R, Nothing, RootResult] = {
    val mapping  = graph.mapping(route.source)
    val prepared = preparedRoot(route, execution.operationType, execution.operationName, caches)
    val request  = GraphQLRequest(
      query = Some(prepared.query),
      operationName = execution.operationName,
      extensions = resolvedRequest.extensions
    )

    sources.get(route.source) match {
      case Some(source) =>
        source
          .execute(request, execution.operationType)
          .map { response =>
            val translated = mapping.fold(response)(_.rootResponseToClient(prepared.executable, response))
            val errors     = translated.errors.map {
              case error: CalibanError.ExecutionError =>
                error.copy(path = graph.restoreResponsePath(route.downstream, prepared.executable, error.path))
              case error                              => error
            }
            val restored   = prepared.restorer match {
              case Some(mappings) => graph.restoreResponseNames(mappings, translated.data)
              case None           => translated.data
            }
            RootResult(
              route,
              translated.copy(
                data = restored,
                errors = source.errorPolicy.routed(route.client, errors)
              )
            )
          }
          .catchAll(_ => ZIO.succeed(RootResult(route, rootFailure(route))))
      case None         => ZIO.succeed(RootResult(route, rootFailure(route)))
    }
  }

  private def executeIntrospection(
    execution: ExecutionRequest,
    fields: List[Field]
  )(implicit trace: Trace): ZIO[Any, Nothing, GraphQLResponse[CalibanError]] =
    if (fields.isEmpty) ZIO.succeed(GraphQLResponse(ObjectValue(Nil), Nil))
    else
      Executor.executeRequest(
        execution.copy(field = execution.field.copy(fields = fields)),
        introspection.query.plan
      )

  private def assemble(
    plan: OperationPlan,
    remote: RemoteExecution,
    local: GraphQLResponse[CalibanError]
  ): GraphQLResponse[CalibanError] = {
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
      val completed = completeObject(plan.fields, data, Nil, errors, plan.runtimeTypes)
      GraphQLResponse(completed.value.getOrElse(NullValue), errors ::: completed.errors)
    }
  }

  private def completeObject(
    fields: List[Field],
    value: ResponseValue,
    path: List[PathValue],
    sourceErrors: List[CalibanError],
    runtimeTypes: List[RuntimeTypeSelection]
  ): CompletedValue =
    value match {
      case obj: ObjectValue =>
        val completed = new mutable.ListBuffer[(String, ResponseValue)]
        val errors    = new mutable.ListBuffer[CalibanError.ExecutionError]
        var missing   = false
        var remaining = fields

        val lookup = IndexedFields(obj)

        while (remaining ne Nil) {
          val field  = remaining.head
          val name   = field.aliasedName
          val value  = lookup.getOrNullValue(name)
          val result = completeValue(
            field.fieldType,
            field,
            value,
            PathValue.Key(name) :: path,
            sourceErrors,
            runtimeTypes
          )
          if (result.errors ne Nil) errors ++= result.errors
          result.value match {
            case Some(completedValue) => completed += (name -> completedValue)
            case None                 => missing = true
          }
          remaining = remaining.tail
        }
        if (missing) CompletedValue(None, errors.toList)
        else CompletedValue(Some(ObjectValue(completed.toList)), errors.toList)
      case _                => CompletedValue(Some(NullValue), invalidSourceValueErrors(path.reverse, sourceErrors))
    }

  private def completeValue(
    fieldType: __Type,
    field: Field,
    value: ResponseValue,
    path: List[PathValue],
    sourceErrors: List[CalibanError],
    runtimeTypes: List[RuntimeTypeSelection]
  ): CompletedValue =
    fieldType.kind match {
      case __TypeKind.NON_NULL                     =>
        val completed = fieldType.ofType
          .map(completeValue(_, field, value, path, sourceErrors, runtimeTypes))
          .getOrElse(CompletedValue(Some(NullValue), Nil))
        completed.value match {
          case Some(NullValue) =>
            CompletedValue(
              None,
              completed.errors ::: nullViolation(field, path.reverse, sourceErrors ::: completed.errors)
            )
          case _               => completed
        }
      case _ if value == NullValue                 => CompletedValue(Some(NullValue), Nil)
      case __TypeKind.LIST                         =>
        (value, fieldType.ofType) match {
          case (ListValue(values), Some(itemType)) =>
            val completed = new mutable.ListBuffer[ResponseValue]
            val errors    = new mutable.ListBuffer[CalibanError.ExecutionError]
            var missing   = false
            var index     = 0
            var remaining = values
            while (remaining ne Nil) {
              val result =
                completeValue(
                  itemType,
                  field,
                  remaining.head,
                  PathValue.Index(index) :: path,
                  sourceErrors,
                  runtimeTypes
                )
              if (result.errors ne Nil) errors ++= result.errors
              result.value match {
                case Some(completedValue) => completed += completedValue
                case None                 => missing = true
              }
              index += 1
              remaining = remaining.tail
            }
            if (missing) CompletedValue(Some(NullValue), errors.toList)
            else CompletedValue(Some(ListValue(completed.toList)), errors.toList)
          case _                                   =>
            CompletedValue(Some(NullValue), invalidSourceValueErrors(path.reverse, sourceErrors))
        }
      case __TypeKind.INTERFACE | __TypeKind.UNION =>
        val possible        = fieldType.possibleTypes.getOrElse(Nil).flatMap(_.name).toSet
        val runtime         = runtimeType(value, responsePath(path), runtimeTypes, field)
        val requiresRuntime = field.fields.exists(child =>
          child.name == "__typename" || child._condition.nonEmpty || child.targets.nonEmpty
        )
        runtime.filter(name => possible.isEmpty || possible.contains(name)) match {
          case Some(typeName)                              =>
            completeNestedObject(typeName, field, value, path, sourceErrors, runtimeTypes)
          case None if runtime.isEmpty && !requiresRuntime =>
            completeNestedObject(
              fieldType.innerType.name.getOrElse(""),
              field,
              value,
              path,
              sourceErrors,
              runtimeTypes
            )
          case None                                        =>
            CompletedValue(Some(NullValue), invalidSourceValueErrors(path.reverse, sourceErrors))
        }
      case __TypeKind.OBJECT                       =>
        completeNestedObject(
          fieldType.innerType.name.getOrElse(""),
          field,
          value,
          path,
          sourceErrors,
          runtimeTypes
        )
      case __TypeKind.ENUM                         =>
        value match {
          case StringValue(name) if fieldType.allEnumValues.exists(_.name == name) => CompletedValue(Some(value), Nil)
          case EnumValue(name) if fieldType.allEnumValues.exists(_.name == name)   => CompletedValue(Some(value), Nil)
          case _                                                                   =>
            val errors =
              if (hasErrorAt(sourceErrors, path.reverse)) Nil
              else {
                val enumName = fieldType.name.getOrElse("Unknown")
                List(
                  CalibanError.ExecutionError(
                    s"Invalid value for enum '$enumName'.",
                    path.reverse,
                    Some(field.locationInfo)
                  )
                )
              }
            CompletedValue(Some(NullValue), errors)
        }
      case __TypeKind.SCALAR                       =>
        val valid = fieldType.name match {
          case Some("String") | Some("ID") =>
            value match {
              case _: StringValue => true
              case _              => false
            }
          case Some("Int")                 =>
            value match {
              case _: IntValue => true
              case _           => false
            }
          case Some("Float")               =>
            value match {
              case _: IntValue | _: FloatValue => true
              case _                           => false
            }
          case Some("Boolean")             =>
            value match {
              case _: BooleanValue => true
              case _               => false
            }
          case _                           => true
        }
        if (valid) CompletedValue(Some(value), Nil)
        else CompletedValue(Some(NullValue), invalidSourceValueErrors(path.reverse, sourceErrors))
      case _                                       => CompletedValue(Some(value), Nil)
    }

  private def completeNestedObject(
    typeName: String,
    field: Field,
    value: ResponseValue,
    path: List[PathValue],
    sourceErrors: List[CalibanError],
    runtimeTypes: List[RuntimeTypeSelection]
  ): CompletedValue = {
    val completed = completeObject(field.collectFields(typeName), value, path, sourceErrors, runtimeTypes)
    if (completed.value.isEmpty) CompletedValue(Some(NullValue), completed.errors) else completed
  }

  private def nullViolation(
    field: Field,
    path: List[PathValue],
    errors: List[CalibanError]
  ): List[CalibanError.ExecutionError] =
    if (hasErrorAt(errors, path)) Nil
    else {
      val parent = field.parentType.flatMap(_.name).getOrElse("Unknown")
      List(
        CalibanError.ExecutionError(
          s"Cannot return null for non-nullable field $parent.${field.name}.",
          path,
          Some(field.locationInfo)
        )
      )
    }

  private def invalidSourceValueErrors(
    path: List[PathValue],
    errors: List[CalibanError]
  ): List[CalibanError.ExecutionError] =
    if ((path.isEmpty && errors.nonEmpty) || hasErrorAt(errors, path)) Nil else List(RemoteError.at(path))

  private def hasErrorAt(errors: List[CalibanError], path: List[PathValue]): Boolean =
    errors.exists {
      case error: CalibanError.ExecutionError => pathsOverlap(error.path, path)
      case _                                  => false
    }

  private def pathsOverlap(left: List[PathValue], right: List[PathValue]): Boolean =
    left.nonEmpty && (left.startsWith(right) || right.startsWith(left))

  private def applyPatches(
    value: ResponseValue,
    patches: List[(List[PathValue], ResponseValue)]
  ): ResponseValue =
    patches match {
      case Nil                  => value
      case (path, patch) :: Nil => mergeAt(value, path, patch)
      case _                    =>
        var current   = value
        var remaining = patches
        val nested    = new mutable.ListBuffer[(List[PathValue], ResponseValue)]
        while (remaining ne Nil) {
          val patch = remaining.head
          if (patch._1.isEmpty) {
            if (nested.nonEmpty) {
              current = applyNestedPatches(current, nested.toList)
              nested.clear()
            }
            current = mergeObject(current, patch._2)
          } else nested += patch
          remaining = remaining.tail
        }
        if (nested.isEmpty) current else applyNestedPatches(current, nested.toList)
    }

  private def applyNestedPatches(
    value: ResponseValue,
    patches: List[(List[PathValue], ResponseValue)]
  ): ResponseValue =
    patches match {
      case (path, patch) :: Nil => mergeAt(value, path, patch)
      case _                    => applyGroupedPatches(value, patches)
    }

  private def applyGroupedPatches(
    value: ResponseValue,
    patches: List[(List[PathValue], ResponseValue)]
  ): ResponseValue =
    value match {
      case ObjectValue(fields) =>
        var byKey: List[(String, mutable.ListBuffer[(List[PathValue], ResponseValue)])] = Nil
        patches.foreach { patch =>
          patch._1 match {
            case StringValue(key) :: tail =>
              var remaining = byKey
              while ((remaining ne Nil) && !remaining.head._1.equals(key)) remaining = remaining.tail
              remaining match {
                case (_, bucket) :: _ => bucket += (tail -> patch._2)
                case Nil              =>
                  val bucket = new mutable.ListBuffer[(List[PathValue], ResponseValue)]
                  bucket += (tail -> patch._2)
                  byKey = (key -> bucket) :: byKey
              }
            case _                        => ()
          }
        }
        if (byKey.isEmpty) value
        else
          ObjectValue(fields.map { field =>
            var remaining = byKey
            while ((remaining ne Nil) && !remaining.head._1.equals(field._1)) remaining = remaining.tail
            remaining match {
              case (_, nestedPatches) :: _ => (field._1, applyPatches(field._2, nestedPatches.toList))
              case Nil                     => field
            }
          })
      case ListValue(values)   =>
        val byIndex = mutable.LongMap.empty[mutable.ListBuffer[(List[PathValue], ResponseValue)]]
        patches.foreach { patch =>
          patch._1 match {
            case IntValue.IntNumber(index) :: tail if index >= 0 =>
              byIndex.getOrElseUpdate(index.toLong, mutable.ListBuffer.empty) += (tail -> patch._2)
            case _                                               => ()
          }
        }
        if (byIndex.isEmpty) value
        else {
          var index = 0
          ListValue(values.map { nested =>
            val nestedPatches = byIndex.getOrNull(index.toLong)
            val patched       = if (nestedPatches eq null) nested else applyPatches(nested, nestedPatches.toList)
            index += 1
            patched
          })
        }
      case other               => other
    }

  private def mergeAt(value: ResponseValue, path: List[PathValue], patch: ResponseValue): ResponseValue =
    path match {
      case Nil          => mergeObject(value, patch)
      case head :: tail =>
        head match {
          case StringValue(key)          =>
            value match {
              case ObjectValue(fields) => ObjectValue(updateFieldAt(fields, key, tail, patch))
              case other               => other
            }
          case IntValue.IntNumber(index) =>
            value match {
              case ListValue(values) if index >= 0 => ListValue(updateValueAt(values, index, tail, patch))
              case other                           => other
            }
        }
    }

  private def updateFieldAt(
    fields: List[(String, ResponseValue)],
    key: String,
    path: List[PathValue],
    patch: ResponseValue
  ): List[(String, ResponseValue)] = {
    val updated   = new mutable.ListBuffer[(String, ResponseValue)]
    var found     = false
    var remaining = fields
    while (remaining ne Nil) {
      val field = remaining.head
      if (field._1.equals(key)) {
        found = true
        updated += ((key, mergeAt(field._2, path, patch)))
      } else updated += field
      remaining = remaining.tail
    }
    if (found) updated.toList else fields
  }

  private def updateValueAt(
    values: List[ResponseValue],
    index: Int,
    path: List[PathValue],
    patch: ResponseValue
  ): List[ResponseValue] = {
    var reversedPrefix: List[ResponseValue] = Nil
    var remaining                           = values
    var position                            = 0
    while (position < index && (remaining ne Nil)) {
      reversedPrefix = remaining.head :: reversedPrefix
      remaining = remaining.tail
      position += 1
    }
    remaining match {
      case nested :: tail => reversedPrefix reverse_::: (mergeAt(nested, path, patch) :: tail)
      case Nil            => values
    }
  }

  private val rightWins: (ResponseValue, ResponseValue) => ResponseValue = (_, value) => value

  private def mergeObject(left: ResponseValue, right: ResponseValue): ResponseValue =
    mergeValues(left, right)(rightWins)

  private def mergeRootValue(left: ResponseValue, right: ResponseValue): ResponseValue =
    mergeValues(left, right) {
      case (NullValue, value)                                                                     => value
      case (value, NullValue)                                                                     => value
      case (ListValue(leftValues), ListValue(rightValues)) if leftValues.size == rightValues.size =>
        ListValue(leftValues.zip(rightValues).map { case (leftValue, rightValue) =>
          mergeRootValue(leftValue, rightValue)
        })
      case (_, value)                                                                             => value
    }

  private def mergeValues(
    left: ResponseValue,
    right: ResponseValue
  )(mergeLeaf: (ResponseValue, ResponseValue) => ResponseValue): ResponseValue =
    (left, right) match {
      case (leftObj: ObjectValue, rightObj: ObjectValue) =>
        val leftFields                                                          = leftObj.fields
        var leftSize                                                            = 0
        var remaining                                                           = leftFields
        while (remaining ne Nil) {
          leftSize += 1
          remaining = remaining.tail
        }
        var positions: java.util.HashMap[String, Integer]                       = null
        var duplicates: java.util.HashMap[String, java.util.ArrayList[Integer]] = null
        if (leftSize >= IndexedFields.WideObjectFields) {
          positions = new java.util.HashMap[String, Integer](leftSize * 2)
          var position = 0
          remaining = leftFields
          while (remaining ne Nil) {
            val name  = remaining.head._1
            val boxed = Integer.valueOf(position)
            positions.get(name) match {
              case null   =>
                if ((duplicates eq null) || !duplicates.containsKey(name)) positions.put(name, boxed)
                else duplicates.get(name).add(boxed)
              case single =>
                val duplicated = new java.util.ArrayList[Integer](2)
                duplicated.add(single)
                duplicated.add(boxed)
                if (duplicates eq null)
                  duplicates = new java.util.HashMap[String, java.util.ArrayList[Integer]]
                duplicates.put(name, duplicated)
                positions.remove(name)
            }
            position += 1
            remaining = remaining.tail
          }
        }
        val matches                                                             = new Array[ResponseValue](leftSize)
        var extras: mutable.ListBuffer[(String, ResponseValue)]                 = null
        var rightRemaining                                                      = rightObj.fields
        while (rightRemaining ne Nil) {
          val field   = rightRemaining.head
          var matched = false
          if (positions ne null) {
            val values = if (duplicates eq null) null else duplicates.get(field._1)
            if (values ne null) {
              var i = 0
              while (i < values.size) {
                matches(values.get(i).intValue) = field._2
                i += 1
              }
              matched = true
            } else {
              val single = positions.get(field._1)
              if (single ne null) {
                matches(single.intValue) = field._2
                matched = true
              }
            }
          } else {
            var position = 0
            remaining = leftFields
            while (remaining ne Nil) {
              if (remaining.head._1.equals(field._1)) {
                matches(position) = field._2
                matched = true
              }
              position += 1
              remaining = remaining.tail
            }
          }
          if (!matched) {
            if (extras eq null) extras = new mutable.ListBuffer
            extras += field
          }
          rightRemaining = rightRemaining.tail
        }
        val merged                                                              = new mutable.ListBuffer[(String, ResponseValue)]
        var position                                                            = 0
        remaining = leftFields
        while (remaining ne Nil) {
          val field   = remaining.head
          val matched = matches(position)
          merged += (if (matched eq null) field else (field._1, mergeValues(field._2, matched)(mergeLeaf)))
          position += 1
          remaining = remaining.tail
        }
        if (extras ne null) merged ++= extras
        ObjectValue(merged.toList)
      case _                                             => mergeLeaf(left, right)
    }

  private def responseFields(response: GraphQLResponse[CalibanError]): List[(String, ResponseValue)] =
    response.data match {
      case ObjectValue(fields) => fields
      case _                   => Nil
    }

  private def completeSourceResponse(
    fields: List[Field],
    response: GraphQLResponse[CalibanError],
    errors: List[CalibanError]
  ): GraphQLResponse[CalibanError] = {
    val completed = completeObject(fields, response.data, Nil, errors, Nil)
    response.copy(data = completed.value.getOrElse(NullValue), errors = errors ::: completed.errors)
  }

  private def runtimeType(
    value: ResponseValue,
    path: Vector[String],
    runtimeTypes: List[RuntimeTypeSelection],
    field: Field
  ): Option[String] =
    value match {
      case obj: ObjectValue =>
        val lookup          = IndexedFields(obj)
        val matching        = runtimeTypes.filter(_.path == path)
        val selectedAliases = field.fields.iterator.filter(_.name == "__typename").map(_.aliasedName)
        matching.iterator
          .flatMap(selection => lookup.get(selection.responseName))
          .collectFirst { case StringValue(name) => StringValue(name) }
          .orElse(selectedAliases.flatMap(lookup.get(_)).collectFirst { case value: StringValue => value })
          .orElse(lookup.get("__typename"))
          .orElse(
            runtimeTypes.iterator
              .filterNot(matching.contains)
              .flatMap(selection => lookup.get(selection.responseName))
              .collectFirst { case value: StringValue => value }
          )
          .collect { case StringValue(name) => name }
      case _                => None
    }

  private def responsePath(path: List[PathValue]): Vector[String] = {
    var names: List[String] = Nil
    var remaining           = path
    while (remaining ne Nil) {
      remaining.head match {
        case PathValue.Key(name) => names = name :: names
        case _                   => ()
      }
      remaining = remaining.tail
    }
    names.toVector
  }

  private def renderPlan(plan: OperationPlan): String = {
    val header = plan.operation.toString.toLowerCase
    val roots  = plan.roots.flatMap { route =>
      route.client.zip(route.downstream).map { case (client, downstream) =>
        val entity = plan.entities.find(_.mergePath.headOption.contains(client.aliasedName))
        val fields = flatten(downstream.fields).map { path =>
          entity
            .flatMap(join =>
              join.keys.find(_.responseName == path).orElse(join.typename.filter(_.responseName == path))
            )
            .map(selection => s"${selection.field} (key)")
            .getOrElse(path)
        }
        s"fetch ${route.source} at $$.${client.aliasedName} fields ${fields.mkString("[", ", ", "]")}"
      }
    }
    val joins  = plan.entities.map(route =>
      s"fetch ${route.source} after ${route.dependencySource} at $$.${route.mergePath.mkString(".")} " +
        s"via ${route.entityType}(${route.keys.map(_.field).mkString(",")}) fields ${flatten(route.fields).mkString("[", ", ", "]")}"
    )
    (header :: roots ::: joins).mkString("\n")
  }

  private def flatten(fields: List[Field]): List[String] =
    fields.flatMap { field =>
      if (field.fields.isEmpty) List(field.aliasedName)
      else flatten(field.fields).map(child => s"${field.aliasedName}.$child")
    }

  private def rootFailure(route: RootRoute): GraphQLResponse[CalibanError] =
    GraphQLResponse(
      ObjectValue(route.client.map(field => field.aliasedName -> NullValue)),
      route.client.map(field => RemoteError.at(List(PathValue.Key(field.aliasedName))))
    )

  private def singleSourceFailure(plan: OperationPlan): GraphQLResponse[CalibanError] = {
    val data   = ObjectValue(plan.fields.map(field => field.aliasedName -> NullValue))
    val errors = plan.fields.map(field => RemoteError.at(List(PathValue.Key(field.aliasedName))))
    completeSourceResponse(plan.fields, GraphQLResponse(data, errors), errors)
  }
}

private object GatewayRuntimeImpl {
  private val requestTimeoutError = CalibanError.ExecutionError("Gateway request timed out.")

  private val requestShutdownError = CalibanError.ExecutionError("Gateway is shutting down.")

  private val requestTimeoutResponse =
    GraphQLResponse(NullValue, List(CalibanError.ExecutionError("Gateway request timed out.")))

  private val requestShutdownResponse =
    GraphQLResponse(NullValue, requestShutdownError :: Nil)

  private final case class RequestResult(
    response: GraphQLResponse[CalibanError],
    outcome: Outcome,
    operationType: Option[OperationType]
  )

  final case class RootResult(route: RootRoute, response: GraphQLResponse[CalibanError])

  private[internal] final case class PreparedRoot(
    operationType: OperationType,
    operationName: Option[String],
    executable: List[Field],
    query: String,
    restorer: Option[Map[String, ComposedGraph.ResponseNameMapping]]
  )

  private final case class EntityExecution(roots: Map[RouteId, ResponseValue], results: List[EntityResult])

  private final case class RemoteExecution(
    roots: List[RootResult],
    entities: List[EntityResult],
    completionErrors: List[CalibanError] = Nil,
    aborted: Boolean = false
  )

  private final case class CompletedValue(value: Option[ResponseValue], errors: List[CalibanError.ExecutionError])
}

private[internal] object PlanCaches {

  def memoize[A <: AnyRef](cache: ConcurrentHashMap[RouteId, A], id: RouteId)(compute: => A): A = {
    val cached = cache.get(id)
    if (cached ne null) cached
    else {
      val created = compute
      cache.put(id, created)
      created
    }
  }
}

private[internal] final class PlanCaches {
  val roots: ConcurrentHashMap[RouteId, GatewayRuntimeImpl.PreparedRoot]        = new ConcurrentHashMap
  val groupKeys: ConcurrentHashMap[RouteId, EntityExecutor.EntityGroupKey]      = new ConcurrentHashMap
  val lookups: ConcurrentHashMap[RouteId, EntityExecutor.PreparedLookup]        = new ConcurrentHashMap
  val identities: ConcurrentHashMap[RouteId, EntityExecutor.IdentitySelections] = new ConcurrentHashMap
}
