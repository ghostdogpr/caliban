package caliban.gateway.internal

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ NullValue, StringValue }
import caliban.execution.{ Executor, Field, RequestPreparation }
import caliban.gateway.GatewayRuntime
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
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, PathValue, ResponseValue }
import zio.{ IO, Trace, URIO, ZIO }

private[gateway] final class GatewayRuntimeImpl[-R](
  graph: ComposedGraph,
  sources: Map[String, GraphQLSource[R]]
) extends GatewayRuntime[R] {

  private val rootType: RootType             = graph.rootType
  private val introspection: RootSchema[Any] = Introspector.introspect[Any](rootType)
  private val planner                        = new OperationPlanner(graph, sources.size)
  private val entityExecutor                 = new EntityExecutor[R](sources)

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    RequestPreparation.check(query, rootType)

  def explain(request: GraphQLRequest)(implicit trace: Trace): IO[CalibanError, String] =
    RequestPreparation.prepare(request, rootType).flatMap { prepared =>
      ZIO
        .fromEither(planner.plan(prepared.document, prepared.executionRequest))
        .mapError(failure => CalibanError.ValidationError(failure.message, ""))
        .map(renderPlan)
    }

  def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    RequestPreparation
      .prepare(request, rootType)
      .foldZIO(
        Executor.fail,
        prepared =>
          planner.plan(prepared.document, prepared.executionRequest) match {
            case Left(failure) => ZIO.succeed(planFailure(failure))
            case Right(plan)   => executePlan(plan, prepared.executionRequest, request)
          }
      )

  private def executePlan(
    plan: OperationPlan,
    execution: caliban.execution.ExecutionRequest,
    original: GraphQLRequest
  )(implicit trace: Trace): ZIO[R, Nothing, GraphQLResponse[CalibanError]] =
    plan.passthrough match {
      case Some(source) =>
        sources.get(source) match {
          case Some(source) =>
            source
              .execute(original)
              .map(response =>
                completeSourceResponse(
                  plan.fields,
                  response,
                  source.errorPolicy.passthrough(plan.fields, response.errors)
                )
              )
              .catchAll(_ => ZIO.succeed(singleSourceFailure(plan)))
          case None         => ZIO.succeed(singleSourceFailure(plan))
        }
      case None         =>
        executeRoots(plan, execution, original).flatMap { roots =>
          val rootValues = roots.iterator.map(result => result.route.id -> result.response.data).toMap
          entityExecutor.execute(plan.entities, rootValues, original).map(entities => roots -> entities)
        }
          .zipPar(executeIntrospection(execution, plan.localFields.filter(isIntrospectionField)))
          .map { case (roots, entities, local) => assemble(plan, roots, entities, local) }
    }

  private def executeRoots(
    plan: OperationPlan,
    execution: caliban.execution.ExecutionRequest,
    original: GraphQLRequest
  )(implicit trace: Trace): ZIO[R, Nothing, List[RootResult]] = {
    val execute = executeRoot(_: RootRoute, execution, original)
    plan.operation match {
      case OperationType.Query        => ZIO.foreachPar(plan.roots)(execute)
      case OperationType.Mutation     => ZIO.foreach(plan.roots)(execute)
      case OperationType.Subscription => ZIO.succeed(Nil)
    }
  }

  private def executeRoot(
    route: RootRoute,
    execution: caliban.execution.ExecutionRequest,
    original: GraphQLRequest
  )(implicit trace: Trace): ZIO[R, Nothing, RootResult] = {
    val operation = OperationDefinition(
      execution.operationType,
      execution.operationName,
      Nil,
      Nil,
      route.downstream.map(_.toSelection)
    )
    val request   = GraphQLRequest(
      query = Some(DocumentRenderer.renderCompact(Document(operation :: Nil, SourceMapper.empty))),
      operationName = execution.operationName,
      extensions = original.extensions
    )

    sources.get(route.source) match {
      case Some(source) =>
        source
          .execute(request)
          .map(response =>
            RootResult(
              route,
              response.copy(errors = source.errorPolicy.routed(route.client, response.errors))
            )
          )
          .catchAll(_ => ZIO.succeed(RootResult(route, rootFailure(route))))
      case None         => ZIO.succeed(RootResult(route, rootFailure(route)))
    }
  }

  private def executeIntrospection(
    execution: caliban.execution.ExecutionRequest,
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
    roots: List[RootResult],
    entities: List[EntityResult],
    local: GraphQLResponse[CalibanError]
  ): GraphQLResponse[CalibanError] = {
    val localValues = responseFields(local).toMap
    val rootValues  = roots.flatMap(result => responseFields(result.response)).toMap
    val data        = ObjectValue(plan.fields.map { field =>
      val value =
        if (field.name == "__typename") StringValue(plan.rootName)
        else
          localValues
            .get(field.aliasedName)
            .orElse(rootValues.get(field.aliasedName).map(value => mergeEntities(field, value, entities)))
            .getOrElse(NullValue)
      field.aliasedName -> project(field, value, Vector(field.aliasedName), plan.entities)
    })
    val errors      = local.errors ::: roots.flatMap(_.response.errors) ::: entities.flatMap(_.errors)
    val completed   = completeObject(plan.fields, data, Vector.empty, errors)
    GraphQLResponse(completed.value.getOrElse(NullValue), errors ::: completed.errors)
  }

  private def completeObject(
    fields: List[Field],
    value: ResponseValue,
    path: Vector[PathValue],
    sourceErrors: List[CalibanError]
  ): CompletedValue =
    value match {
      case ObjectValue(values) =>
        val byName    = values.toMap
        val completed = fields.map { field =>
          val fieldPath = path :+ PathValue.Key(field.aliasedName)
          field.aliasedName -> completeValue(
            field.fieldType,
            field,
            byName.getOrElse(field.aliasedName, NullValue),
            fieldPath,
            sourceErrors
          )
        }
        val errors    = completed.flatMap(_._2.errors)
        if (completed.exists(_._2.value.isEmpty)) CompletedValue(None, errors)
        else
          CompletedValue(
            Some(ObjectValue(completed.flatMap { case (name, result) => result.value.map(name -> _) })),
            errors
          )
      case _                   => CompletedValue(Some(NullValue), Nil)
    }

  private def completeValue(
    fieldType: __Type,
    field: Field,
    value: ResponseValue,
    path: Vector[PathValue],
    sourceErrors: List[CalibanError]
  ): CompletedValue =
    fieldType.kind match {
      case __TypeKind.NON_NULL                                         =>
        val completed = fieldType.ofType
          .map(completeValue(_, field, value, path, sourceErrors))
          .getOrElse(CompletedValue(Some(NullValue), Nil))
        completed.value match {
          case Some(NullValue) =>
            CompletedValue(
              None,
              completed.errors ::: nullViolation(field, path.toList, sourceErrors ::: completed.errors)
            )
          case _               => completed
        }
      case _ if value == NullValue                                     => CompletedValue(Some(NullValue), Nil)
      case __TypeKind.LIST                                             =>
        (value, fieldType.ofType) match {
          case (ListValue(values), Some(itemType)) =>
            val completed = values.zipWithIndex.map { case (item, index) =>
              completeValue(itemType, field, item, path :+ PathValue.Index(index), sourceErrors)
            }
            val errors    = completed.flatMap(_.errors)
            if (completed.exists(_.value.isEmpty)) CompletedValue(Some(NullValue), errors)
            else CompletedValue(Some(ListValue(completed.flatMap(_.value))), errors)
          case _                                   =>
            CompletedValue(Some(NullValue), invalidListErrors(path.toList, sourceErrors))
        }
      case __TypeKind.OBJECT | __TypeKind.INTERFACE | __TypeKind.UNION =>
        val typeName  = fieldType.innerType.name.getOrElse("")
        val completed = completeObject(field.collectFields(typeName), value, path, sourceErrors)
        if (completed.value.isEmpty) CompletedValue(Some(NullValue), completed.errors) else completed
      case _                                                           => CompletedValue(Some(value), Nil)
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

  private def invalidListErrors(
    path: List[PathValue],
    errors: List[CalibanError]
  ): List[CalibanError.ExecutionError] =
    if (hasErrorAt(errors, path)) Nil else List(RemoteError.at(path))

  private def hasErrorAt(errors: List[CalibanError], path: List[PathValue]): Boolean =
    errors.exists {
      case error: CalibanError.ExecutionError => pathsOverlap(error.path, path)
      case _                                  => false
    }

  private def pathsOverlap(left: List[PathValue], right: List[PathValue]): Boolean =
    left.nonEmpty && (left.startsWith(right) || right.startsWith(left))

  private def mergeEntities(field: Field, value: ResponseValue, entities: List[EntityResult]): ResponseValue =
    entities.foldLeft(value) { case (current, result) =>
      result.patches
        .filter(_.route.mergePath.headOption.contains(field.aliasedName))
        .foldLeft(current) { case (merged, patch) =>
          mergeAt(merged, patch.path.drop(1), patch.value)
        }
    }

  private def mergeAt(value: ResponseValue, path: List[PathValue], patch: ResponseValue): ResponseValue =
    path match {
      case Nil                            => mergeObject(value, patch)
      case PathValue.Key(key) :: tail     =>
        value match {
          case ObjectValue(fields) =>
            ObjectValue(fields.map {
              case (name, nested) if name == key => name -> mergeAt(nested, tail, patch)
              case other                         => other
            })
          case other               => other
        }
      case PathValue.Index(index) :: tail =>
        value match {
          case ListValue(values) if index >= 0 && index < values.size =>
            ListValue(values.zipWithIndex.map { case (nested, current) =>
              if (current == index) mergeAt(nested, tail, patch) else nested
            })
          case other                                                  => other
        }
      case _                              => value
    }

  private def mergeObject(left: ResponseValue, right: ResponseValue): ResponseValue =
    (left, right) match {
      case (ObjectValue(leftFields), ObjectValue(rightFields)) =>
        val rightMap = rightFields.toMap
        val names    = leftFields.iterator.map(_._1).toSet
        ObjectValue(
          leftFields.map { case (name, value) => name -> rightMap.getOrElse(name, value) } :::
            rightFields.filterNot(field => names.contains(field._1))
        )
      case (_, value)                                          => value
    }

  private def project(
    field: Field,
    value: ResponseValue,
    path: Vector[String],
    entities: List[EntityRoute]
  ): ResponseValue =
    value match {
      case ObjectValue(fields) if field.fields.nonEmpty =>
        val values          = fields.toMap
        val privateTypename = entities
          .find(_.mergePath == path)
          .flatMap(_.typename)
          .flatMap(selection => values.get(selection.responseName))
        val clientTypename  = field.fields
          .find(_.name == "__typename")
          .flatMap(child => values.get(child.aliasedName))
        val typeName        = privateTypename
          .orElse(clientTypename)
          .collect { case StringValue(name) => name }
          .orElse(field.fieldType.innerType.name)
          .getOrElse("")
        ObjectValue(
          field
            .collectFields(typeName)
            .map(child =>
              child.aliasedName -> project(
                child,
                values.getOrElse(child.aliasedName, NullValue),
                path :+ child.aliasedName,
                entities
              )
            )
        )
      case ListValue(values)                            => ListValue(values.map(project(field, _, path, entities)))
      case other                                        => other
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
    val completed = completeObject(fields, response.data, Vector.empty, errors)
    response.copy(data = completed.value.getOrElse(NullValue), errors = errors ::: completed.errors)
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

  private def isIntrospectionField(field: Field): Boolean =
    field.name == "__schema" || field.name == "__type"

  private def planFailure(failure: PlanningFailure): GraphQLResponse[CalibanError] =
    GraphQLResponse(NullValue, List(CalibanError.ExecutionError(failure.message)))

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
  final case class RootResult(route: RootRoute, response: GraphQLResponse[CalibanError])

  private final case class CompletedValue(value: Option[ResponseValue], errors: List[CalibanError.ExecutionError])
}
