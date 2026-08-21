package caliban.gateway.internal

import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue, VariableValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ EnumValue, NullValue, StringValue }
import caliban.execution.Field
import caliban.gateway.internal.EntityExecutor._
import caliban.gateway.internal.OperationPlanner._
import caliban.introspection.adt.__TypeKind
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection, VariableDefinition }
import caliban.rendering.DocumentRenderer
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, PathValue, ResponseValue }
import zio.{ Trace, URIO, ZIO }

import scala.collection.immutable.ListMap
import scala.collection.mutable

private[gateway] final class EntityExecutor[-R](
  graph: ComposedGraph,
  sources: Map[String, GraphQLSource[R]]
) {

  def execute(
    routes: List[EntityRoute],
    roots: Map[RouteId, ResponseValue],
    blocked: Map[RouteId, Set[List[PathValue]]],
    resolvedRequest: GraphQLRequest
  )(implicit trace: Trace): URIO[R, List[EntityResult]] = {
    val grouped = mutable.LinkedHashMap.empty[EntityGroupKey, EntityGroup]
    routes.foreach { route =>
      val key = entityGroupKey(route)
      grouped.get(key) match {
        case Some(group) => group.additionalRoutes += route
        case None        => grouped.put(key, EntityGroup(route, mutable.ListBuffer.empty))
      }
    }
    ZIO.foreachPar(grouped.values.toList)(group => executeGroup(group, roots, blocked, resolvedRequest))
  }

  private def executeGroup(
    group: EntityGroup,
    roots: Map[RouteId, ResponseValue],
    blocked: Map[RouteId, Set[List[PathValue]]],
    resolvedRequest: GraphQLRequest
  )(implicit trace: Trace): URIO[R, EntityResult] = {
    val route  = group.firstRoute
    val routes = group.routes
    val batch  = prepareBatch(routes, roots, blocked)

    if (batch.entries.isEmpty) ZIO.succeed(EntityResult(Nil, batch.errors, batch.routes, batch.blocked))
    else {
      val failure = EntityResult(
        Nil,
        batch.errors ::: routes.map(route => RemoteError.at(routePath(route))),
        batch.routes,
        blockAll(batch)
      )

      graph
        .mapping(route.source)
        .flatMap(mapping => buildLookup(route, routes, batch, resolvedRequest, mapping).map(mapping -> _)) match {
        case Some((mapping, lookup)) =>
          sources.get(route.source) match {
            case Some(source) =>
              source
                .execute(lookup.request, OperationType.Query)
                .map(response =>
                  correlateResponse(
                    route,
                    batch,
                    lookup,
                    lookup.toClient(route, response, mapping, graph),
                    source.errorPolicy
                  )
                )
                .catchAll(_ => ZIO.succeed(failure))
            case None         => ZIO.succeed(failure)
          }
        case None                    => ZIO.succeed(failure)
      }
    }
  }

  private def federationCorrelation(
    route: EntityRoute,
    routes: List[EntityRoute]
  ): EntityCorrelation.Federation = {
    val usedNames = routes.iterator.flatMap(_.fields.iterator.map(_.aliasedName)).toSet
    val keys      = route.keys.foldLeft((List.empty[CorrelationKey], usedNames)) { case ((values, names), key) =>
      val alias = privateAlias("_caliban_gateway_entity_key", names)
      (CorrelationKey(key.field, key.copy(responseName = alias)) :: values, names + alias)
    }
    val ordered   = keys._1.reverse
    val names     = usedNames ++ ordered.iterator.map(_.selection.responseName)
    EntityCorrelation.Federation(
      IdentitySelections(
        ordered,
        Some(
          RequiredSelection(
            "__typename",
            privateAlias("_caliban_gateway_entity_typename", names)
          )
        )
      )
    )
  }

  private def graphqlCorrelation(
    route: EntityRoute,
    routes: List[EntityRoute],
    result: ComposedGraph.LookupResult.ListResult
  ): EntityCorrelation =
    result match {
      case ComposedGraph.LookupResult.Ordered       => EntityCorrelation.Ordered
      case ComposedGraph.LookupResult.ByKey(fields) =>
        val usedNames  = routes.iterator.flatMap(_.fields.iterator.map(_.aliasedName)).toSet
        val configured = route.keys.flatMap(key =>
          fields.collectFirst {
            case (responseField, keyField) if keyField == key.field => responseField -> keyField
          }
        )
        EntityCorrelation.ByKey(
          IdentitySelections(correlationKeys(configured, usedNames, "_caliban_gateway_lookup_key"), None)
        )
    }

  private def correlationKeys(
    fields: List[(String, String)],
    usedNames: Set[String],
    aliasBase: String
  ): List[CorrelationKey] =
    fields
      .foldLeft((List.empty[CorrelationKey], usedNames)) { case ((keys, names), (responseField, keyField)) =>
        val alias = privateAlias(aliasBase, names)
        (CorrelationKey(keyField, RequiredSelection(responseField, alias)) :: keys, names + alias)
      }
      ._1
      .reverse

  private def buildLookup(
    route: EntityRoute,
    routes: List[EntityRoute],
    batch: EntityBatch,
    resolvedRequest: GraphQLRequest,
    mapping: SchemaCoordinateMapping
  ): Option[LookupExecution] = {
    val executableFields = route.fields.map(graph.executableEntityField(route.source, route.entityType, _))
    val sourceSelections = executableFields.map(mapping.rootFieldToSource).flatMap(fieldSelection)

    route.lookup.operation match {
      case ComposedGraph.LookupOperation.FederationEntities(correlationKey)                                           =>
        val correlation =
          if (correlationKey.nonEmpty && batch.entries.map(_.identity).distinct.size == batch.entries.size)
            federationCorrelation(route, routes)
          else EntityCorrelation.Ordered
        val variables   = Map(
          "representations" -> InputListValue(
            batch.entries.map(entry =>
              mapping.representationToSource(route.entityType, federationRepresentation(route, entry))
            )
          )
        )
        val selections  = sourceSelections ::: correlation.required
          .map(mapping.requiredSelectionToSource(route.entityType, _))
          .map(requiredSelection)
        val entityField = Selection.Field(
          None,
          "_entities",
          Map("representations" -> VariableValue("representations")),
          Nil,
          List(
            Selection.InlineFragment(
              Some(NamedType(mapping.sourceType(route.entityType), nonNull = false)),
              Nil,
              selections
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
        Some(
          LookupExecution(
            request(operation, Some(variables), resolvedRequest),
            correlation,
            LookupResponse.ListRoot("_entities"),
            executableFields
          )
        )
      case ComposedGraph.LookupOperation.GraphQLQuery(field, mappings, result: ComposedGraph.LookupResult.ListResult) =>
        val correlation = graphqlCorrelation(route, routes, result)
        evaluateArguments(mappings, batch, None).map { arguments =>
          val alias           = "_caliban_gateway_lookup"
          val sourceArguments = mapping.sourceLookupArguments(field, arguments)
          val selection       = Selection.Field(
            Some(alias),
            mapping.sourceLookupField(field),
            sourceArguments,
            Nil,
            sourceSelections ::: correlation.required
              .map(mapping.requiredSelectionToSource(route.entityType, _))
              .map(requiredSelection),
            0
          )
          val operation       = OperationDefinition(
            OperationType.Query,
            Some("__GatewayLookup"),
            Nil,
            Nil,
            List(selection)
          )
          LookupExecution(
            request(operation, None, resolvedRequest),
            correlation,
            LookupResponse.ListRoot(alias),
            executableFields
          )
        }
      case ComposedGraph.LookupOperation.GraphQLQuery(field, mappings, ComposedGraph.LookupResult.Single)             =>
        val correlation = EntityCorrelation.Ordered
        val selections  = traverse(batch.entries.zipWithIndex) { case (entry, index) =>
          evaluateArguments(mappings, batch, Some(entry)).map { arguments =>
            val alias = s"_caliban_gateway_lookup_$index"
            Selection.Field(
              Some(alias),
              mapping.sourceLookupField(field),
              mapping.sourceLookupArguments(field, arguments),
              Nil,
              sourceSelections,
              0
            ) -> (alias -> index)
          }
        }
        selections.map { generated =>
          val (values, indices) = generated.unzip
          val operation         = OperationDefinition(
            OperationType.Query,
            Some("__GatewayLookup"),
            Nil,
            Nil,
            values
          )
          LookupExecution(
            request(operation, None, resolvedRequest),
            correlation,
            LookupResponse.Aliases(indices.toMap),
            executableFields
          )
        }
    }
  }

  private def request(
    operation: OperationDefinition,
    variables: Option[Map[String, InputValue]],
    resolvedRequest: GraphQLRequest
  ): GraphQLRequest =
    GraphQLRequest(
      query = Some(DocumentRenderer.renderCompact(Document(operation :: Nil, SourceMapper.empty))),
      operationName = operation.name,
      variables = variables,
      extensions = resolvedRequest.extensions
    )

  private def requiredSelection(value: RequiredSelection): Selection =
    Selection.Field(
      if (value.responseName == value.field) None else Some(value.responseName),
      value.field,
      Map.empty,
      Nil,
      value.children.map(requiredSelection),
      0
    )

  private def fieldSelection(field: Field): List[Selection] =
    field.targets match {
      case Some(targets) =>
        targets.toList.sorted.map(target =>
          Selection.InlineFragment(
            Some(NamedType(target, nonNull = false)),
            Nil,
            field.copy(targets = None).toSelection :: Nil
          )
        )
      case None          => field.toSelection :: Nil
    }

  private def federationRepresentation(route: EntityRoute, entry: EntityBatchEntry): InputObjectValue =
    InputObjectValue(
      entry.identity.keys.toMap ++ entry.requirements +
        ("__typename" -> StringValue(route.lookup.representationType.getOrElse(entry.identity.typename)))
    )

  private def traverse[A, B](values: Iterable[A])(f: A => Option[B]): Option[List[B]] =
    values
      .foldLeft(Option(List.empty[B])) { case (result, value) =>
        for {
          collected <- result
          next      <- f(value)
        } yield next :: collected
      }
      .map(_.reverse)

  private def evaluateArguments(
    arguments: Map[String, ComposedGraph.LookupArgument],
    batch: EntityBatch,
    current: Option[EntityBatchEntry]
  ): Option[Map[String, InputValue]] =
    traverse(arguments.toList) { case (name, argument) =>
      evaluateArgument(argument, batch, current).map(name -> _)
    }.map(_.toMap)

  private def evaluateArgument(
    argument: ComposedGraph.LookupArgument,
    batch: EntityBatch,
    current: Option[EntityBatchEntry]
  ): Option[InputValue] =
    argument match {
      case ComposedGraph.LookupArgument.Key(field, expectedType) =>
        current
          .flatMap(_.identity.keys.toMap.get(field))
          .map {
            case StringValue(value) if expectedType.kind == __TypeKind.ENUM =>
              EnumValue(value)
            case value                                                      => value
          }
      case ComposedGraph.LookupArgument.ObjectMapping(fields)    =>
        traverse(fields) { case (name, value) =>
          evaluateArgument(value, batch, current).map(name -> _)
        }.map(values => InputObjectValue(values.toMap))
      case ComposedGraph.LookupArgument.Batch(value)             =>
        traverse(batch.entries)(entry => evaluateArgument(value, batch, Some(entry)))
          .map(InputListValue.apply)
    }

  private def prepareBatch(
    routes: List[EntityRoute],
    roots: Map[RouteId, ResponseValue],
    blocked: Map[RouteId, Set[List[PathValue]]]
  ): EntityBatch = {
    val entries =
      mutable.LinkedHashMap.empty[(EntityIdentity, List[(String, InputValue)]), mutable.ListBuffer[EntityLocation]]
    val errors  = mutable.ListBuffer.empty[CalibanError]
    val skipped = mutable.Map.empty[RouteId, mutable.Set[List[PathValue]]]

    routes.foreach { route =>
      val parent = roots.getOrElse(route.root, NullValue)
      entityCandidates(parent, route.mergePath, Vector.empty).foreach {
        case (_, NullValue)           => ()
        case (path, ObjectValue(raw)) =>
          if (
            route.dependencies.exists(dependency =>
              blocked.getOrElse(dependency, Set.empty).exists(blockedPath => path.startsWith(blockedPath))
            )
          )
            skipped.getOrElseUpdate(route.id, mutable.Set.empty) += path
          else if (
            graph.isObjectType(route.entityType) && route.typename
              .flatMap(selection =>
                raw.collectFirst {
                  case (name, StringValue(runtimeType)) if name == selection.responseName => runtimeType
                }
              )
              .exists(_ != route.entityType)
          )
            skipped.getOrElseUpdate(route.id, mutable.Set.empty) += path
          else
            sourceRepresentation(route, raw.toMap) match {
              case Some(representation) =>
                entries.get(representation) match {
                  case Some(locations) => locations += EntityLocation(route, path)
                  case None            =>
                    entries.put(representation, mutable.ListBuffer(EntityLocation(route, path)))
                }
              case None                 =>
                errors += missingRepresentation(route, path)
                skipped.getOrElseUpdate(route.id, mutable.Set.empty) += path
            }
        case (path, _)                =>
          errors += missingRepresentation(route, path)
          skipped.getOrElseUpdate(route.id, mutable.Set.empty) += path
      }
    }

    EntityBatch(
      entries.iterator.map { case ((identity, requirements), locations) =>
        EntityBatchEntry(identity, requirements.toMap, locations.toList)
      }.toList,
      errors.toList,
      skipped.iterator.map { case (route, paths) => route -> paths.toSet }.toMap,
      routes.map(_.id).toSet
    )
  }

  private def sourceRepresentation(
    route: EntityRoute,
    fields: Map[String, ResponseValue]
  ): Option[(EntityIdentity, List[(String, InputValue)])] =
    for {
      identity     <- readIdentity(
                        route.entityType,
                        IdentitySelections(route.keys.map(key => CorrelationKey(key.field, key)), route.typename),
                        fields
                      )
      requirements <- traverse(route.requirements.filter(appliesTo(_, identity.typename)))(selection =>
                        fields
                          .get(selection.responseName)
                          .flatMap(selectedInput(selection, _, allowNull = true))
                          .map(selection.field -> _)
                      )
    } yield identity -> requirements

  private def readIdentity(
    entityType: String,
    selections: IdentitySelections,
    fields: Map[String, ResponseValue]
  ): Option[EntityIdentity] = {
    val keys     = traverse(selections.keys)(key =>
      fields.get(key.selection.responseName).flatMap(selectedInput(key.selection, _)).map(key.keyField -> _)
    )
    val typename = selections.typename match {
      case Some(selection) => fields.get(selection.responseName).collect { case StringValue(value) => value }
      case None            => Some(entityType)
    }
    for {
      values   <- keys
      typeName <- typename
    } yield EntityIdentity(typeName, values)
  }

  private def selectedInput(
    selection: RequiredSelection,
    value: ResponseValue,
    allowNull: Boolean = false
  ): Option[InputValue] =
    if (value == NullValue) if (allowNull) Some(NullValue) else None
    else if (selection.children.isEmpty) responseInput(value)
    else
      value match {
        case ObjectValue(fields) =>
          selectedObject(selection.children, selection.runtimeTypeAlias, fields.toMap, allowNull)
        case ListValue(values)   =>
          traverse(values)(selectedInput(selection, _, allowNull)).map(InputListValue.apply)
        case _                   => None
      }

  private def responseInput(value: ResponseValue): Option[InputValue] =
    value match {
      case input: InputValue   => Some(input)
      case ObjectValue(fields) =>
        traverse(fields) { case (name, nested) => responseInput(nested).map(name -> _) }
          .map(values => InputObjectValue(values.toMap))
      case ListValue(values)   => traverse(values)(responseInput).map(InputListValue.apply)
      case _                   => None
    }

  private def selectedObject(
    selections: List[RequiredSelection],
    runtimeTypeAlias: Option[String],
    fields: Map[String, ResponseValue],
    allowNull: Boolean
  ): Option[InputObjectValue] = {
    val applicable = runtimeTypeAlias match {
      case None        => Some(selections)
      case Some(alias) =>
        fields.get(alias).collect { case StringValue(runtimeType) => selections.filter(appliesTo(_, runtimeType)) }
    }
    applicable.flatMap(values =>
      traverse(values)(selection =>
        fields.get(selection.responseName).flatMap(selectedInput(selection, _, allowNull)).map(selection.field -> _)
      ).map(values => InputObjectValue(values.toMap))
    )
  }

  private def appliesTo(selection: RequiredSelection, runtimeType: String): Boolean =
    selection.conditions.forall(_.contains(runtimeType))

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
    lookup: LookupExecution,
    response: GraphQLResponse[CalibanError],
    errorPolicy: GraphQLSource.ErrorPolicy
  ): EntityResult = {
    val expected       = batch.entries.iterator.zipWithIndex.map { case (entry, index) => entry.identity -> index }.toMap
    val resolved       = mutable.Set.empty[Int]
    val patches        = mutable.LinkedHashMap.empty[Int, ResponseValue]
    val protocolErrors = mutable.ListBuffer.empty[CalibanError]
    val blocked        = mutable.Map.empty[RouteId, mutable.Set[List[PathValue]]]
    batch.blocked.foreach { case (route, paths) => blocked.put(route, mutable.Set(paths.toSeq: _*)) }
    val values         = lookup.response.values(response.data)

    values.foreach {
      case (index, NullValue)                   =>
        lookup.correlation match {
          case EntityCorrelation.Ordered | _: EntityCorrelation.Federation =>
            batch.entries.lift(index) match {
              case Some(entry) if resolved.add(index) => blockEntry(entry, blocked)
              case Some(_)                            => protocolErrors += duplicateEntityResult(route)
              case None                               => protocolErrors += unexpectedEntityResult(route)
            }
          case _: EntityCorrelation.ByKey                                  =>
            protocolErrors += unexpectedEntityResult(route)
        }
      case (index, value @ ObjectValue(fields)) =>
        val resolvedIndex = lookup.correlation match {
          case EntityCorrelation.Ordered      => batch.entries.lift(index).map(_ => index)
          case keyed: EntityCorrelation.Keyed =>
            readIdentity(route.entityType, keyed.identity, fields.toMap).flatMap(expected.get)
        }
        resolvedIndex match {
          case Some(entryIndex) if resolved.add(entryIndex) =>
            patches.put(entryIndex, value)
          case Some(_)                                      =>
            protocolErrors += duplicateEntityResult(route)
          case None                                         =>
            protocolErrors += unexpectedEntityResult(route)
        }
      case (_, _)                               =>
        protocolErrors += unexpectedEntityResult(route)
    }

    val missing       = batch.entries.zipWithIndex.filterNot { case (_, index) => resolved.contains(index) }
    missing.foreach { case (entry, _) => blockEntry(entry, blocked) }
    val merged        = batch.entries.zipWithIndex.flatMap { case (entry, index) =>
      patches
        .get(index)
        .toList
        .flatMap(patch => entry.locations.map(location => EntityPatch(location.route, location.path, patch)))
    }
    val missingErrors = lookup.correlation match {
      case _: EntityCorrelation.ByKey                                  => Nil
      case EntityCorrelation.Ordered | _: EntityCorrelation.Federation =>
        missing.flatMap { case (entry, _) =>
          entry.locations.map(location => missingEntityResult(location.route, location.path))
        }
    }
    val errors        = batch.errors :::
      relocateErrors(route, batch, lookup, values.toMap, response.errors, errorPolicy) :::
      protocolErrors.toList :::
      missingErrors

    EntityResult(
      merged,
      errors,
      batch.routes,
      blocked.iterator.map { case (route, paths) => route -> paths.toSet }.toMap
    )
  }

  private def blockEntry(
    entry: EntityBatchEntry,
    blocked: mutable.Map[RouteId, mutable.Set[List[PathValue]]]
  ): Unit =
    entry.locations.foreach(location => blocked.getOrElseUpdate(location.route.id, mutable.Set.empty) += location.path)

  private def blockAll(batch: EntityBatch): Map[RouteId, Set[List[PathValue]]] = {
    val blocked = mutable.Map.empty[RouteId, mutable.Set[List[PathValue]]]
    batch.blocked.foreach { case (route, paths) => blocked.put(route, mutable.Set(paths.toSeq: _*)) }
    batch.entries.foreach(blockEntry(_, blocked))
    blocked.iterator.map { case (route, paths) => route -> paths.toSet }.toMap
  }

  private def relocateErrors(
    route: EntityRoute,
    batch: EntityBatch,
    lookup: LookupExecution,
    values: Map[Int, ResponseValue],
    errors: List[CalibanError],
    errorPolicy: GraphQLSource.ErrorPolicy
  ): List[CalibanError] =
    errors.flatMap {
      case error: CalibanError.ExecutionError =>
        lookup.errorIndex(error.path) match {
          case Some((index, tail)) =>
            val locations  = entityLocations(route.entityType, batch, lookup.correlation, values.get(index), index)
            val clientTail = graph.restoreResponsePath(route.fields, lookup.executableFields, tail)
            if (locations.isEmpty) mergePaths(route, batch).map(errorPolicy.unusableEntity(error, _))
            else
              locations.map { location =>
                if (clientTail.isEmpty || RemoteError.hasClientPath(location.route.fields, clientTail))
                  error.copy(path = location.path ::: clientTail, locationInfo = None)
                else errorPolicy.unusableEntity(error, location.path)
              }
          case None                =>
            mergePaths(route, batch).map(errorPolicy.unusableEntity(error, _))
        }
      case error                              => List(error)
    }

  private def entityLocations(
    entityType: String,
    batch: EntityBatch,
    correlation: EntityCorrelation,
    value: Option[ResponseValue],
    index: Int
  ): List[EntityLocation] =
    correlation match {
      case EntityCorrelation.Ordered      => batch.entries.lift(index).map(_.locations).getOrElse(Nil)
      case keyed: EntityCorrelation.Keyed =>
        val byIdentity = value.collect { case ObjectValue(fields) =>
          readIdentity(entityType, keyed.identity, fields.toMap)
        }.flatten
          .flatMap(identity => batch.entries.find(_.identity == identity))
          .map(_.locations)
        val positional = keyed match {
          case _: EntityCorrelation.Federation => batch.entries.lift(index).map(_.locations)
          case _: EntityCorrelation.ByKey      => None
        }
        byIdentity.orElse(positional).getOrElse(Nil)
    }

  private def mergePaths(route: EntityRoute, batch: EntityBatch): List[List[PathValue]] = {
    val paths = mutable.LinkedHashSet.empty[List[PathValue]]
    batch.entries.foreach(_.locations.foreach(location => paths += routePath(location.route)))
    paths += routePath(route)
    paths.toList
  }

  private def missingRepresentation(route: EntityRoute, path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      s"Entity key '${entityKey(route)}' was missing from the source result.",
      path = path
    )

  private def duplicateEntityResult(route: EntityRoute): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      s"Entity lookup response contained a duplicate result for '${entityKey(route)}'.",
      path = routePath(route)
    )

  private def unexpectedEntityResult(route: EntityRoute): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      s"Entity lookup response contained an unexpected result for '${entityKey(route)}'.",
      path = routePath(route)
    )

  private def missingEntityResult(route: EntityRoute, path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      s"Entity lookup response omitted a result for '${entityKey(route)}'.",
      path = path
    )

  private def entityKey(route: EntityRoute): String =
    s"${route.entityType}(${route.keys.map(_.field).mkString(", ")})"

  private def routePath(route: EntityRoute): List[PathValue] =
    route.mergePath.iterator.map(PathValue.Key(_)).toList

}

private[gateway] object EntityExecutor {
  final case class EntityPatch(route: EntityRoute, path: List[PathValue], value: ResponseValue)

  final case class EntityResult(
    patches: List[EntityPatch],
    errors: List[CalibanError],
    completed: Set[RouteId],
    blocked: Map[RouteId, Set[List[PathValue]]]
  )

  private final case class EntityGroupKey(
    source: String,
    entityType: String,
    lookup: ComposedGraph.EntityLookup,
    keys: List[RequiredSelection],
    requirements: List[RequiredSelection],
    selection: String
  )

  private[gateway] def logicalCallCount(routes: List[EntityRoute]): Int = {
    val routeIds = routes.iterator.map(_.id).toSet

    def count(
      pending: List[EntityRoute],
      completed: Set[RouteId],
      calls: Int
    ): Int =
      if (pending.isEmpty) calls
      else {
        val ready = pending.filter(route => route.dependencies.forall(id => completed.contains(id) || !routeIds(id)))
        if (ready.isEmpty) calls
        else
          count(
            pending.filterNot(ready.toSet),
            completed ++ ready.iterator.map(_.id),
            calls + ready.iterator.map(entityGroupKey).toSet.size
          )
      }

    count(routes, Set.empty, 0)
  }

  private def entityGroupKey(route: EntityRoute): EntityGroupKey =
    EntityGroupKey(
      route.source,
      route.entityType,
      route.lookup,
      route.keys,
      route.requirements,
      entitySelectionKey(route.fields)
    )

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

  private final case class EntityGroup(firstRoute: EntityRoute, additionalRoutes: mutable.ListBuffer[EntityRoute]) {
    def routes: List[EntityRoute] = firstRoute :: additionalRoutes.toList
  }

  private sealed trait EntityCorrelation {
    def required: List[RequiredSelection]
  }

  private object EntityCorrelation {
    case object Ordered extends EntityCorrelation {
      val required: List[RequiredSelection] = Nil
    }

    sealed trait Keyed extends EntityCorrelation {
      def identity: IdentitySelections

      def required: List[RequiredSelection] =
        identity.keys.map(_.selection) ::: identity.typename.toList
    }

    final case class Federation(identity: IdentitySelections) extends Keyed
    final case class ByKey(identity: IdentitySelections)      extends Keyed
  }

  private final case class CorrelationKey(keyField: String, selection: RequiredSelection)

  private final case class IdentitySelections(
    keys: List[CorrelationKey],
    typename: Option[RequiredSelection]
  )

  private final case class LookupExecution(
    request: GraphQLRequest,
    correlation: EntityCorrelation,
    response: LookupResponse,
    executableFields: List[Field]
  ) {
    def errorIndex(path: List[PathValue]): Option[(Int, List[PathValue])] =
      response.errorIndex(path)

    def toClient(
      route: EntityRoute,
      result: GraphQLResponse[CalibanError],
      mapping: SchemaCoordinateMapping,
      graph: ComposedGraph
    ): GraphQLResponse[CalibanError] =
      result.copy(data = response.map(result.data) { value =>
        val translated = mapping.entityResponseToClient(
          route.entityType,
          executableFields,
          correlation.required,
          value
        )
        graph.restoreResponseNames(route.fields, executableFields, translated)
      })
  }

  private sealed trait LookupResponse {
    def values(data: ResponseValue): List[(Int, ResponseValue)]
    def errorIndex(path: List[PathValue]): Option[(Int, List[PathValue])]
    def map(data: ResponseValue)(f: ResponseValue => ResponseValue): ResponseValue
  }

  private object LookupResponse {
    final case class ListRoot(root: String) extends LookupResponse {
      def values(data: ResponseValue): List[(Int, ResponseValue)] =
        data match {
          case ObjectValue(fields) =>
            fields.collectFirst { case (`root`, ListValue(values)) => values.zipWithIndex.map(_.swap) }.getOrElse(Nil)
          case _                   => Nil
        }

      def errorIndex(path: List[PathValue]): Option[(Int, List[PathValue])] =
        path match {
          case PathValue.Key(`root`) :: PathValue.Index(index) :: tail if index >= 0 => Some(index -> tail)
          case _                                                                     => None
        }

      def map(data: ResponseValue)(f: ResponseValue => ResponseValue): ResponseValue =
        data match {
          case ObjectValue(fields) =>
            ObjectValue(fields.map {
              case (`root`, ListValue(values)) => root -> ListValue(values.map(f))
              case other                       => other
            })
          case other               => other
        }
    }

    final case class Aliases(indices: Map[String, Int]) extends LookupResponse {
      def values(data: ResponseValue): List[(Int, ResponseValue)] =
        data match {
          case ObjectValue(fields) =>
            val values = fields.toMap
            indices.toList.sortBy(_._2).flatMap { case (alias, index) => values.get(alias).map(index -> _) }
          case _                   => Nil
        }

      def errorIndex(path: List[PathValue]): Option[(Int, List[PathValue])] =
        path match {
          case PathValue.Key(alias) :: tail => indices.get(alias).map(_ -> tail)
          case _                            => None
        }

      def map(data: ResponseValue)(f: ResponseValue => ResponseValue): ResponseValue =
        data match {
          case ObjectValue(fields) =>
            ObjectValue(fields.map {
              case (alias, value) if indices.contains(alias) => alias -> f(value)
              case other                                     => other
            })
          case other               => other
        }
    }
  }

  private final case class EntityIdentity(typename: String, keys: List[(String, InputValue)])

  private final case class EntityLocation(route: EntityRoute, path: List[PathValue])

  private final case class EntityBatchEntry(
    identity: EntityIdentity,
    requirements: Map[String, InputValue],
    locations: List[EntityLocation]
  )

  private final case class EntityBatch(
    entries: List[EntityBatchEntry],
    errors: List[CalibanError],
    blocked: Map[RouteId, Set[List[PathValue]]],
    routes: Set[RouteId]
  )
}
