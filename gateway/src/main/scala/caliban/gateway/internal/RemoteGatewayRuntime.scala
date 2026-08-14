package caliban.gateway.internal

import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue, VariableValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ NullValue, StringValue }
import caliban.execution.{ Executor, Field, RequestPreparation }
import caliban.gateway.GatewayRuntime
import caliban.introspection.Introspector
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Document, OperationType, Selection, VariableDefinition }
import caliban.rendering.DocumentRenderer
import caliban.schema.{ RootSchema, RootType }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, PathValue, ResponseValue }
import zio.{ IO, Trace, URIO, ZIO }

import OperationPlanner._
import RemoteGatewayRuntime._

private[gateway] final class RemoteGatewayRuntime[-R](
  graph: ComposedGraph,
  sources: Map[String, RemoteGraphQLSource]
) extends GatewayRuntime[R] {

  private val rootType: RootType             = graph.rootType
  private val introspection: RootSchema[Any] = Introspector.introspect[Any](rootType)
  private val planner                        = new OperationPlanner(graph, sources.size)

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
  )(implicit trace: Trace): ZIO[Any, Nothing, GraphQLResponse[CalibanError]] =
    plan.passthrough match {
      case Some(source) =>
        sources(source).execute(original).catchAll(_ => ZIO.succeed(singleSourceFailure))
      case None         =>
        executeRoots(plan, execution, original).flatMap { roots =>
          ZIO.foreach(plan.entities)(executeEntity(_, roots, original)).map(entities => roots -> entities)
        }
          .zipPar(executeIntrospection(execution, plan.localFields.filter(isIntrospectionField)))
          .map { case (roots, entities, local) => assemble(plan, roots, entities, local) }
    }

  private def executeRoots(
    plan: OperationPlan,
    execution: caliban.execution.ExecutionRequest,
    original: GraphQLRequest
  )(implicit trace: Trace): ZIO[Any, Nothing, List[RootResult]] = {
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
  )(implicit trace: Trace): ZIO[Any, Nothing, RootResult] = {
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

    sources(route.source)
      .execute(request)
      .map(response => RootResult(route, response))
      .catchAll(_ => ZIO.succeed(RootResult(route, rootFailure(route))))
  }

  private def executeEntity(
    route: EntityRoute,
    roots: List[RootResult],
    original: GraphQLRequest
  )(implicit trace: Trace): ZIO[Any, Nothing, EntityResult] = {
    val parent                                                         = roots.find(_.route.id == route.dependency).map(_.response.data).getOrElse(NullValue)
    val entity                                                         = ResponseValue.at(route.mergePath.map(name => PathValue.Key(name)))(parent)
    val representation: Option[Option[caliban.InputValue.ObjectValue]] = entity match {
      case NullValue           => None
      case ObjectValue(fields) =>
        val values   = fields.toMap
        val key      = values.get(route.key.responseName).collect {
          case value: InputValue if value != NullValue => value
        }
        val typename = values.get(route.typename.responseName).collect { case value: StringValue => value }
        Some(for {
          keyValue      <- key
          typenameValue <- typename
        } yield InputObjectValue(Map("__typename" -> typenameValue, route.key.field -> keyValue)))
      case _                   => Some(None)
    }

    representation match {
      case None                       => ZIO.succeed(EntityResult(route, skippedEntity))
      case Some(None)                 => ZIO.succeed(EntityResult(route, missingRepresentation(route)))
      case Some(Some(representation)) =>
        val variables   = Map("representations" -> InputListValue(List(representation)))
        val entityField = Selection.Field(
          None,
          "_entities",
          Map("representations" -> VariableValue("representations")),
          Nil,
          List(
            Selection.InlineFragment(
              Some(NamedType(route.entityType, nonNull = false)),
              Nil,
              route.fields.map(_.toSelection)
            )
          ),
          0
        )
        val operation   = OperationDefinition(
          OperationType.Query,
          Some("__GatewayEntity"),
          List(
            VariableDefinition(
              "representations",
              ListType(NamedType("_Any", nonNull = true), nonNull = true),
              None,
              Nil
            )
          ),
          Nil,
          List(entityField)
        )
        val request     = GraphQLRequest(
          query = Some(DocumentRenderer.renderCompact(Document(operation :: Nil, SourceMapper.empty))),
          operationName = Some("__GatewayEntity"),
          variables = Some(variables),
          extensions = original.extensions
        )

        sources(route.source)
          .execute(request)
          .map(response => EntityResult(route, response))
          .catchAll(_ => ZIO.succeed(EntityResult(route, entityFailure(route))))
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
      field.aliasedName -> project(field, value, List(field.aliasedName), plan.entities)
    })
    val errors      = local.errors :::
      roots.flatMap(result => errorsAtRoot(result.route, result.response.errors)) :::
      entities.flatMap(result => errorsAtEntity(result.route, result.response.errors))
    GraphQLResponse(data, errors)
  }

  private def mergeEntities(field: Field, value: ResponseValue, entities: List[EntityResult]): ResponseValue =
    entities
      .filter(_.route.mergePath.headOption.contains(field.aliasedName))
      .foldLeft(value) { case (current, result) =>
        entityValue(result.response.data) match {
          case Some(patch) => mergeAt(current, result.route.mergePath.drop(1), patch)
          case None        => current
        }
      }

  private def entityValue(data: ResponseValue): Option[ResponseValue] =
    data match {
      case ObjectValue(fields) =>
        fields.collectFirst { case ("_entities", ListValue(value :: _)) =>
          value
        }
      case _                   => None
    }

  private def mergeAt(value: ResponseValue, path: List[String], patch: ResponseValue): ResponseValue =
    path match {
      case Nil          => mergeObject(value, patch)
      case head :: tail =>
        value match {
          case ObjectValue(fields) =>
            ObjectValue(fields.map {
              case (`head`, nested) => head -> mergeAt(nested, tail, patch)
              case other            => other
            })
          case ListValue(values)   => ListValue(values.map(mergeAt(_, path, patch)))
          case other               => other
        }
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
    path: List[String],
    entities: List[EntityRoute]
  ): ResponseValue =
    value match {
      case ObjectValue(fields) if field.fields.nonEmpty =>
        val values          = fields.toMap
        val privateTypename = entities
          .find(_.mergePath == path)
          .flatMap(route => values.get(route.typename.responseName))
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

  private def errorsAtRoot(route: RootRoute, errors: List[CalibanError]): List[CalibanError] =
    errors.flatMap {
      case error: CalibanError.ExecutionError if error.path.isEmpty =>
        route.client.map(field => error.copy(path = List(PathValue.Key(field.aliasedName))))
      case error                                                    => error :: Nil
    }

  private def errorsAtEntity(route: EntityRoute, errors: List[CalibanError]): List[CalibanError] =
    errors.map {
      case error: CalibanError.ExecutionError =>
        val suffix = error.path match {
          case PathValue.Key("_entities") :: PathValue.Index(0) :: tail => tail
          case PathValue.Key("_entities") :: tail                       => tail
          case path                                                     => path
        }
        error.copy(path = route.mergePath.map(name => PathValue.Key(name)) ::: suffix)
      case error                              => error
    }

  private def renderPlan(plan: OperationPlan): String = {
    val header = plan.operation.toString.toLowerCase
    val roots  = plan.roots.flatMap { route =>
      route.client.zip(route.downstream).map { case (client, downstream) =>
        val entity = plan.entities.find(_.mergePath.headOption.contains(client.aliasedName))
        val fields = flatten(downstream.fields).map { path =>
          entity match {
            case Some(join) if path == join.key.responseName      => s"${join.key.field} (key)"
            case Some(join) if path == join.typename.responseName => s"${join.typename.field} (key)"
            case _                                                => path
          }
        }
        s"fetch ${route.source} at $$.${client.aliasedName} fields ${fields.mkString("[", ", ", "]")}"
      }
    }
    val joins  = plan.entities.map(route =>
      s"fetch ${route.source} after ${route.dependencySource} at $$.${route.mergePath.mkString(".")} " +
        s"via ${route.entityType}(${route.key.field}) fields ${flatten(route.fields).mkString("[", ", ", "]")}"
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
      route.client.map(field =>
        CalibanError.ExecutionError(
          "Remote GraphQL request failed.",
          path = List(PathValue.Key(field.aliasedName))
        )
      )
    )

  private def entityFailure(route: EntityRoute): GraphQLResponse[CalibanError] =
    GraphQLResponse(
      NullValue,
      List(
        CalibanError.ExecutionError(
          "Remote GraphQL request failed.",
          path = route.mergePath.map(name => PathValue.Key(name))
        )
      )
    )

  private def missingRepresentation(route: EntityRoute): GraphQLResponse[CalibanError] =
    GraphQLResponse(
      NullValue,
      List(
        CalibanError.ExecutionError(
          s"Entity key '${route.entityType}.${route.key.field}' was missing from the source result.",
          path = route.mergePath.map(name => PathValue.Key(name))
        )
      )
    )

  private val singleSourceFailure =
    GraphQLResponse(
      NullValue,
      List(CalibanError.ExecutionError("Remote GraphQL request failed."))
    )

  private val skippedEntity = GraphQLResponse(NullValue, Nil)
}

private object RemoteGatewayRuntime {
  final case class RootResult(route: RootRoute, response: GraphQLResponse[CalibanError])

  final case class EntityResult(route: EntityRoute, response: GraphQLResponse[CalibanError])
}
