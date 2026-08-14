package caliban.gateway.internal

import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue, VariableValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ NullValue, StringValue }
import caliban.execution.Field
import caliban.gateway.internal.EntityExecutor._
import caliban.gateway.internal.OperationPlanner.{ privateAlias, EntityRoute, RequiredSelection, RouteId }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection, VariableDefinition }
import caliban.rendering.DocumentRenderer
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, PathValue, ResponseValue }
import zio.{ Trace, UIO, ZIO }

import scala.collection.immutable.ListMap
import scala.collection.mutable

private[gateway] final class EntityExecutor(sources: Map[String, RemoteGraphQLSource]) {

  def execute(
    routes: List[EntityRoute],
    roots: Map[RouteId, ResponseValue],
    original: GraphQLRequest
  )(implicit trace: Trace): UIO[List[EntityResult]] = {
    val grouped = mutable.LinkedHashMap.empty[EntityGroupKey, EntityGroup]
    routes.foreach { route =>
      val key = EntityGroupKey(route.source, route.entityType, route.key.field, entitySelectionKey(route.fields))
      grouped.get(key) match {
        case Some(group) => group.additionalRoutes += route
        case None        => grouped.put(key, EntityGroup(route, mutable.ListBuffer.empty))
      }
    }
    ZIO.foreach(grouped.values.toList)(group => executeGroup(group, roots, original))
  }

  private def executeGroup(
    group: EntityGroup,
    roots: Map[RouteId, ResponseValue],
    original: GraphQLRequest
  )(implicit trace: Trace): UIO[EntityResult] = {
    val route       = group.firstRoute
    val routes      = group.routes
    val batch       = prepareBatch(routes, roots)
    val correlation = entityCorrelation(route, routes)

    if (batch.entries.isEmpty) ZIO.succeed(EntityResult(Nil, batch.errors))
    else {
      val variables   = Map("representations" -> InputListValue(batch.entries.map(_.representation)))
      val entityField = Selection.Field(
        None,
        "_entities",
        Map("representations" -> VariableValue("representations")),
        Nil,
        List(
          Selection.InlineFragment(
            Some(NamedType(route.entityType, nonNull = false)),
            Nil,
            route.fields.map(_.toSelection) ::: correlation.selections
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
      val failure     = EntityResult(
        Nil,
        batch.errors ::: routes.map(route => RemoteError.at(routePath(route)))
      )

      sources.get(route.source) match {
        case Some(source) =>
          source
            .execute(request)
            .map(response => correlateResponse(route, batch, correlation, response))
            .catchAll(_ => ZIO.succeed(failure))
        case None         => ZIO.succeed(failure)
      }
    }
  }

  private def entityCorrelation(route: EntityRoute, routes: List[EntityRoute]): EntityCorrelation = {
    val usedNames = routes.iterator.flatMap(_.fields.iterator.map(_.aliasedName)).toSet
    val key       = RequiredSelection(
      route.key.field,
      privateAlias("_caliban_gateway_entity_key", usedNames)
    )
    val typename  = RequiredSelection(
      "__typename",
      privateAlias("_caliban_gateway_entity_typename", usedNames + key.responseName)
    )
    EntityCorrelation(key, typename)
  }

  private def prepareBatch(routes: List[EntityRoute], roots: Map[RouteId, ResponseValue]): EntityBatch = {
    val entries = mutable.LinkedHashMap.empty[EntityIdentity, (InputObjectValue, mutable.ListBuffer[EntityLocation])]
    val errors  = mutable.ListBuffer.empty[CalibanError]

    routes.foreach { route =>
      val parent = roots.getOrElse(route.dependency, NullValue)
      entityCandidates(parent, route.mergePath, Vector.empty).foreach {
        case (_, NullValue)           => ()
        case (path, ObjectValue(raw)) =>
          responseIdentity(route.key, route.typename, raw.toMap) match {
            case Some(identity) =>
              val representation = InputObjectValue(
                Map("__typename" -> StringValue(identity.typename), route.key.field -> identity.key)
              )
              entries.get(identity) match {
                case Some((_, locations)) => locations += EntityLocation(route, path)
                case None                 =>
                  entries.put(identity, representation -> mutable.ListBuffer(EntityLocation(route, path)))
              }
            case None           =>
              errors += missingRepresentation(route, path)
          }
        case (path, _)                =>
          errors += missingRepresentation(route, path)
      }
    }

    EntityBatch(
      entries.iterator.map { case (identity, (representation, locations)) =>
        EntityBatchEntry(identity, representation, locations.toList)
      }.toList,
      errors.toList
    )
  }

  private def entityCandidates(
    value: ResponseValue,
    fields: Vector[String],
    path: Vector[PathValue]
  ): List[(List[PathValue], ResponseValue)] =
    fields.headOption match {
      case None       =>
        value match {
          case ListValue(values) =>
            values.zipWithIndex.flatMap { case (item, index) =>
              entityCandidates(item, Vector.empty, path :+ PathValue.Index(index))
            }
          case NullValue         => Nil
          case other             => List(path.toList -> other)
        }
      case Some(head) =>
        val tail = fields.tail
        value match {
          case ObjectValue(values) =>
            values.collectFirst { case (name, nested) if name == head => nested } match {
              case Some(nested) => entityCandidates(nested, tail, path :+ PathValue.Key(head))
              case None         => Nil
            }
          case ListValue(values)   =>
            values.zipWithIndex.flatMap { case (item, index) =>
              entityCandidates(item, fields, path :+ PathValue.Index(index))
            }
          case NullValue           => Nil
          case other               => List(path.toList -> other)
        }
    }

  private def correlateResponse(
    route: EntityRoute,
    batch: EntityBatch,
    correlation: EntityCorrelation,
    response: GraphQLResponse[CalibanError]
  ): EntityResult = {
    val expected       = batch.entries.iterator.map(entry => entry.identity -> entry).toMap
    val resolved       = mutable.Set.empty[EntityIdentity]
    val patches        = mutable.LinkedHashMap.empty[EntityIdentity, ResponseValue]
    val protocolErrors = mutable.ListBuffer.empty[CalibanError]
    val values         = entityValues(response.data)

    values.zipWithIndex.foreach {
      case (NullValue, index)               =>
        batch.entries.lift(index) match {
          case Some(entry) if resolved.add(entry.identity) => ()
          case Some(_)                                     => protocolErrors += duplicateEntityResult(route)
          case None                                        => protocolErrors += unexpectedEntityResult(route)
        }
      case (value @ ObjectValue(fields), _) =>
        responseIdentity(correlation.key, correlation.typename, fields.toMap) match {
          case Some(identity) if expected.contains(identity) && resolved.add(identity) =>
            patches.put(identity, value)
          case Some(identity) if expected.contains(identity)                           =>
            protocolErrors += duplicateEntityResult(route)
          case _                                                                       =>
            protocolErrors += unexpectedEntityResult(route)
        }
      case (_, _)                           =>
        protocolErrors += unexpectedEntityResult(route)
    }

    val missing = batch.entries.filterNot(entry => resolved.contains(entry.identity))
    val merged  = batch.entries.flatMap(entry =>
      patches
        .get(entry.identity)
        .toList
        .flatMap(patch => entry.locations.map(location => EntityPatch(location.route, location.path, patch)))
    )
    val errors  = batch.errors :::
      relocateErrors(route, batch, correlation, values, response.errors) :::
      protocolErrors.toList :::
      missing.flatMap(entry => entry.locations.map(location => missingEntityResult(location.route, location.path)))

    EntityResult(merged, errors)
  }

  private def entityValues(data: ResponseValue): List[ResponseValue] =
    data match {
      case ObjectValue(fields) =>
        fields.collectFirst { case ("_entities", ListValue(values)) => values }.getOrElse(Nil)
      case _                   => Nil
    }

  private def responseIdentity(
    key: RequiredSelection,
    typename: RequiredSelection,
    fields: Map[String, ResponseValue]
  ): Option[EntityIdentity] = {
    val keyValue      = fields.get(key.responseName).collect {
      case value: InputValue if value != NullValue => value
    }
    val typenameValue = fields.get(typename.responseName).collect { case StringValue(value) => value }
    for {
      value    <- keyValue
      typeName <- typenameValue
    } yield EntityIdentity(typeName, value)
  }

  private def relocateErrors(
    route: EntityRoute,
    batch: EntityBatch,
    correlation: EntityCorrelation,
    values: List[ResponseValue],
    errors: List[CalibanError]
  ): List[CalibanError] =
    errors.flatMap {
      case error: CalibanError.ExecutionError =>
        error.path match {
          case PathValue.Key("_entities") :: PathValue.Index(index) :: tail =>
            val locations = entityLocations(batch, correlation, values, index)
            if (locations.isEmpty) mergePaths(route, batch).map(RemoteError.at)
            else
              locations.map { location =>
                if (tail.isEmpty || RemoteError.hasClientPath(location.route.fields, tail))
                  error.copy(path = location.path ::: tail, locationInfo = None)
                else RemoteError.at(location.path)
              }
          case _                                                            =>
            mergePaths(route, batch).map(RemoteError.at)
        }
      case error                              => List(error)
    }

  private def entityLocations(
    batch: EntityBatch,
    correlation: EntityCorrelation,
    values: List[ResponseValue],
    index: Int
  ): List[EntityLocation] = {
    val byIdentity = values
      .lift(index)
      .collect { case ObjectValue(fields) =>
        responseIdentity(correlation.key, correlation.typename, fields.toMap)
      }
      .flatten
      .flatMap(identity => batch.entries.find(_.identity == identity))
      .map(_.locations)

    byIdentity
      .orElse(batch.entries.lift(index).map(_.locations))
      .getOrElse(Nil)
  }

  private def mergePaths(route: EntityRoute, batch: EntityBatch): List[List[PathValue]] = {
    val paths = mutable.LinkedHashSet.empty[List[PathValue]]
    batch.entries.foreach(_.locations.foreach(location => paths += routePath(location.route)))
    paths += routePath(route)
    paths.toList
  }

  private def missingRepresentation(route: EntityRoute, path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      "Entity key '" + route.entityType + "." + route.key.field + "' was missing from the source result.",
      path = path
    )

  private def duplicateEntityResult(route: EntityRoute): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      "Remote entity response contained a duplicate result for '" + route.entityType + "." + route.key.field + "'.",
      path = routePath(route)
    )

  private def unexpectedEntityResult(route: EntityRoute): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      "Remote entity response contained an unexpected result for '" + route.entityType + "." + route.key.field + "'.",
      path = routePath(route)
    )

  private def missingEntityResult(route: EntityRoute, path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      "Remote entity response omitted a result for '" + route.entityType + "." + route.key.field + "'.",
      path = path
    )

  private def routePath(route: EntityRoute): List[PathValue] =
    route.mergePath.iterator.map(PathValue.Key(_)).toList

  private def entitySelectionKey(fields: List[Field]): String = {
    val selections = fields.map(field => canonicalSelection(field.toSelection)).sortBy(renderSelection)
    DocumentRenderer.renderCompact(
      Document(
        OperationDefinition(OperationType.Query, None, Nil, Nil, selections) :: Nil,
        SourceMapper.empty
      )
    )
  }

  private def canonicalSelection(selection: Selection): Selection =
    selection match {
      case field: Selection.Field             =>
        field.copy(
          arguments = ListMap(field.arguments.toList.sortBy(_._1): _*),
          directives = field.directives.map(canonicalDirective),
          selectionSet = field.selectionSet.map(canonicalSelection).sortBy(renderSelection),
          index = 0
        )
      case fragment: Selection.InlineFragment =>
        fragment.copy(
          dirs = fragment.dirs.map(canonicalDirective),
          selectionSet = fragment.selectionSet.map(canonicalSelection).sortBy(renderSelection)
        )
      case fragment: Selection.FragmentSpread =>
        fragment.copy(directives = fragment.directives.map(canonicalDirective))
    }

  private def canonicalDirective(directive: Directive): Directive =
    directive.copy(arguments = ListMap(directive.arguments.toList.sortBy(_._1): _*), index = 0)

  private def renderSelection(selection: Selection): String =
    DocumentRenderer.renderCompact(
      Document(
        OperationDefinition(OperationType.Query, None, Nil, Nil, selection :: Nil) :: Nil,
        SourceMapper.empty
      )
    )

}

private[gateway] object EntityExecutor {
  final case class EntityPatch(route: EntityRoute, path: List[PathValue], value: ResponseValue)

  final case class EntityResult(patches: List[EntityPatch], errors: List[CalibanError])

  private final case class EntityGroupKey(source: String, entityType: String, key: String, selection: String)

  private final case class EntityGroup(firstRoute: EntityRoute, additionalRoutes: mutable.ListBuffer[EntityRoute]) {
    def routes: List[EntityRoute] = firstRoute :: additionalRoutes.toList
  }

  private final case class EntityCorrelation(key: RequiredSelection, typename: RequiredSelection) {
    def selections: List[Selection] =
      List(
        Selection.Field(Some(key.responseName), key.field, Map.empty, Nil, Nil, 0),
        Selection.Field(Some(typename.responseName), typename.field, Map.empty, Nil, Nil, 0)
      )
  }

  private final case class EntityIdentity(typename: String, key: InputValue)

  private final case class EntityLocation(route: EntityRoute, path: List[PathValue])

  private final case class EntityBatchEntry(
    identity: EntityIdentity,
    representation: InputObjectValue,
    locations: List[EntityLocation]
  )

  private final case class EntityBatch(entries: List[EntityBatchEntry], errors: List[CalibanError])
}
