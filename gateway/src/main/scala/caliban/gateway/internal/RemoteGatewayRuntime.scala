package caliban.gateway.internal

import caliban.Value.{ NullValue, StringValue }
import caliban.execution.{ Executor, RequestPreparation }
import caliban.introspection.Introspector
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection }
import caliban.rendering.DocumentRenderer
import caliban.ResponseValue.ObjectValue
import caliban.schema.{ RootSchema, RootType }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, PathValue }
import caliban.gateway.GatewayRuntime
import zio.{ IO, Trace, URIO, ZIO }

import scala.collection.mutable

import RemoteGatewayRuntime.{ Route, RouteResult }

private[gateway] final class RemoteGatewayRuntime[-R](
  graph: ComposedGraph,
  sources: Map[String, RemoteGraphQLSource]
) extends GatewayRuntime[R] {

  private val rootType: RootType             = graph.rootType
  private val introspection: RootSchema[Any] = Introspector.introspect[Any](rootType)

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    RequestPreparation.check(query, rootType)

  def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    RequestPreparation
      .prepare(request, rootType)
      .foldZIO(
        Executor.fail,
        prepared => executeRemote(prepared.document, prepared.executionRequest, request)
      )

  private def executeRemote(
    document: Document,
    execution: caliban.execution.ExecutionRequest,
    original: GraphQLRequest
  )(implicit trace: Trace): ZIO[Any, Nothing, GraphQLResponse[CalibanError]] = {
    val fields              = execution.field.collectFields(rootName(execution.operationType))
    val introspectionFields = fields.filter(isIntrospectionField)
    val localFields         = fields.filter(isLocalField)
    val remoteFields        = fields.filterNot(isLocalField)
    val owners              = remoteFields.flatMap(field => graph.source(execution.operationType, field.name)).distinct

    if (sources.size == 1 && owners.size == 1 && localFields.isEmpty)
      sources(owners.head)
        .execute(original)
        .catchAll(_ => ZIO.succeed(singleSourceFailure))
    else if (execution.operationType == OperationType.Mutation && owners.size > 1)
      ZIO.succeed(crossSourceMutation)
    else if (hasCustomExecutableDirective(document, execution.operationName))
      ZIO.succeed(unsupportedExecutableDirective)
    else {
      val calls = execution.operationType match {
        case OperationType.Query        =>
          ZIO.foreachPar(queryRoutes(remoteFields))(executeRoute(_, execution, original))
        case OperationType.Mutation     =>
          ZIO.foreach(mutationRoutes(remoteFields))(executeRoute(_, execution, original))
        case OperationType.Subscription => ZIO.succeed(Nil)
      }

      calls
        .zipPar(executeIntrospection(execution, introspectionFields))
        .map { case (results, local) => assemble(execution.operationType, fields, results, local) }
    }
  }

  private def executeIntrospection(
    execution: caliban.execution.ExecutionRequest,
    fields: List[caliban.execution.Field]
  )(implicit trace: Trace): ZIO[Any, Nothing, GraphQLResponse[CalibanError]] =
    if (fields.isEmpty) ZIO.succeed(GraphQLResponse(ObjectValue(Nil), Nil))
    else
      Executor.executeRequest(
        execution.copy(field = execution.field.copy(fields = fields)),
        introspection.query.plan
      )

  private def isIntrospectionField(field: caliban.execution.Field): Boolean =
    field.name == "__schema" || field.name == "__type"

  private def isLocalField(field: caliban.execution.Field): Boolean =
    isIntrospectionField(field) || field.name == "__typename"

  private def hasCustomExecutableDirective(
    document: Document,
    operationName: Option[String]
  ): Boolean = {
    val fragments = document.fragmentDefinitions.iterator.map(fragment => fragment.name -> fragment).toMap

    def loop(selections: List[Selection], visitedFragments: Set[String]): Boolean =
      selections.exists {
        case Selection.Field(_, _, _, directives, selectionSet, _) =>
          directives.exists(isCustomDirective) || loop(selectionSet, visitedFragments)
        case Selection.InlineFragment(_, directives, selectionSet) =>
          directives.exists(isCustomDirective) || loop(selectionSet, visitedFragments)
        case Selection.FragmentSpread(name, directives)            =>
          directives.exists(isCustomDirective) ||
          (!visitedFragments.contains(name) && fragments
            .get(name)
            .exists(fragment =>
              fragment.directives.exists(isCustomDirective) || loop(fragment.selectionSet, visitedFragments + name)
            ))
      }

    val operation = operationName match {
      case Some(name) => document.operationDefinitions.find(_.name.contains(name))
      case None       =>
        document.operationDefinitions match {
          case operation :: Nil => Some(operation)
          case _                => None
        }
    }

    operation.exists(operation =>
      operation.directives.exists(isCustomDirective) ||
        operation.variableDefinitions.exists(_.directives.exists(isCustomDirective)) ||
        loop(operation.selectionSet, Set.empty)
    )
  }

  private def isCustomDirective(directive: Directive): Boolean =
    directive.name != "skip" && directive.name != "include"

  private def queryRoutes(fields: List[caliban.execution.Field]): List[Route] = {
    val grouped = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[caliban.execution.Field]]
    fields.foreach { field =>
      graph.source(OperationType.Query, field.name).foreach { source =>
        grouped.getOrElseUpdate(source, mutable.ListBuffer.empty) += field
      }
    }
    grouped.iterator.map { case (source, selected) => Route(source, selected.toList) }.toList
  }

  private def mutationRoutes(fields: List[caliban.execution.Field]): List[Route] =
    fields.flatMap(field => graph.source(OperationType.Mutation, field.name).map(Route(_, field :: Nil)))

  private def executeRoute(
    route: Route,
    execution: caliban.execution.ExecutionRequest,
    original: GraphQLRequest
  )(implicit trace: Trace): ZIO[Any, Nothing, RouteResult] = {
    val operation = OperationDefinition(
      execution.operationType,
      execution.operationName,
      Nil,
      Nil,
      route.fields.map(_.toSelection)
    )
    val request   = GraphQLRequest(
      query = Some(DocumentRenderer.renderCompact(Document(operation :: Nil, SourceMapper.empty))),
      operationName = execution.operationName,
      extensions = original.extensions
    )

    sources(route.source)
      .execute(request)
      .map(response => RouteResult(route, response))
      .catchAll(_ => ZIO.succeed(RouteResult(route, routeFailure(route))))
  }

  private def assemble(
    operation: OperationType,
    fields: List[caliban.execution.Field],
    results: List[RouteResult],
    local: GraphQLResponse[CalibanError]
  ): GraphQLResponse[CalibanError] = {
    val values = (responseFields(local) ::: results.flatMap(result => responseFields(result.response))).toMap
    val data   = ObjectValue(fields.map { field =>
      val value =
        if (field.name == "__typename") StringValue(rootName(operation))
        else values.getOrElse(field.aliasedName, NullValue)
      field.aliasedName -> value
    })
    val errors = local.errors ::: results.flatMap(result => errorsAtRoute(result.route, result.response.errors))
    GraphQLResponse(data, errors)
  }

  private def responseFields(response: GraphQLResponse[CalibanError]): List[(String, caliban.ResponseValue)] =
    response.data match {
      case ObjectValue(fields) => fields
      case _                   => Nil
    }

  private def errorsAtRoute(route: Route, errors: List[CalibanError]): List[CalibanError] =
    errors.flatMap {
      case error: CalibanError.ExecutionError if error.path.isEmpty =>
        route.fields.map(field => error.copy(path = List(PathValue.Key(field.aliasedName))))
      case error                                                    => error :: Nil
    }

  private def routeFailure(route: Route): GraphQLResponse[CalibanError] =
    GraphQLResponse(
      ObjectValue(route.fields.map(field => field.aliasedName -> NullValue)),
      route.fields.map(field =>
        CalibanError.ExecutionError(
          "Remote GraphQL request failed.",
          path = List(PathValue.Key(field.aliasedName))
        )
      )
    )

  private def rootName(operation: OperationType): String =
    operation match {
      case OperationType.Query        => rootType.queryType.name.getOrElse("Query")
      case OperationType.Mutation     => rootType.mutationType.flatMap(_.name).getOrElse("Mutation")
      case OperationType.Subscription => "Subscription"
    }

  private val singleSourceFailure =
    GraphQLResponse(
      NullValue,
      List(CalibanError.ExecutionError("Remote GraphQL request failed."))
    )

  private val crossSourceMutation =
    GraphQLResponse(
      NullValue,
      List(CalibanError.ExecutionError("Mutations spanning multiple subgraphs are not supported by this gateway."))
    )

  private val unsupportedExecutableDirective =
    GraphQLResponse(
      NullValue,
      List(CalibanError.ExecutionError("Custom executable directives are not supported by this gateway."))
    )
}

private object RemoteGatewayRuntime {
  final case class Route(source: String, fields: List[caliban.execution.Field])
  final case class RouteResult(route: Route, response: GraphQLResponse[CalibanError])
}
