package caliban.gateway.internal

import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue, VariableValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ EnumValue, NullValue, StringValue }
import caliban.Hash
import caliban.Scala3Annotations.threadUnsafe
import caliban.execution.Field
import caliban.gateway.traverseOption
import caliban.gateway.internal.EntityExecutor._
import caliban.gateway.internal.OperationPlanner._
import caliban.introspection.adt.__TypeKind
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Document, OperationType, Selection, VariableDefinition }
import caliban.rendering.DocumentRenderer
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, PathValue, ResponseValue }
import zio.{ Trace, URIO, ZIO }

import scala.collection.mutable

private[gateway] final class EntityExecutor[-R](
  graph: ComposedGraph,
  sources: Map[String, GraphQLSource[R]]
) {

  private def cachedGroupKey(route: EntityRoute, cache: PlanExecutionCache): EntityGroupKey =
    PlanExecutionCache.memoize(cache.groupKeys, route.id)(entityGroupKey(route))

  private def preparedLookup(
    route: EntityRoute,
    mapping: SchemaMapping,
    cache: PlanExecutionCache
  ): PreparedLookup =
    PlanExecutionCache.memoize(cache.lookups, route.id) {
      val executableFields = route.fields.map(graph.executableEntityField(route.source, route.entityType, _))
      PreparedLookup(
        executableFields,
        executableFields.map(mapping.rootFieldToSource).flatMap(fieldSelection),
        mapping.entityFieldsResponseMapper(executableFields),
        graph.responseNameRestorer(route.fields, executableFields)
      )
    }

  def execute(
    routes: List[EntityRoute],
    roots: Map[RouteId, ResponseValue],
    blocked: Map[RouteId, Set[List[PathValue]]],
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, List[EntityResult]] = {
    val grouped    = mutable.LinkedHashMap.empty[EntityGroupKey, mutable.ListBuffer[EntityRoute]]
    routes.foreach { route =>
      val key = cachedGroupKey(route, cache)
      grouped.get(key) match {
        case Some(group) => group += route
        case None        => grouped.put(key, mutable.ListBuffer(route))
      }
    }
    val candidates = mutable.HashMap.empty[(RouteId, Vector[String]), List[(List[PathValue], ResponseValue)]]
    routes.foreach { route =>
      val key = (route.root, route.mergePath)
      if (!candidates.contains(key)) {
        val collected = new mutable.ListBuffer[(List[PathValue], ResponseValue)]
        entityCandidates(roots.getOrElse(route.root, NullValue), route.mergePath.toList, Nil, collected)
        candidates.put(key, collected.toList)
      }
    }
    grouped.values.toList match {
      case group :: Nil => executeGroup(group, blocked, candidates, resolvedRequest, cache).map(_ :: Nil)
      case groups       =>
        ZIO.foreachPar(groups)(group => executeGroup(group, blocked, candidates, resolvedRequest, cache))
    }
  }

  private def executeGroup(
    group: mutable.ListBuffer[EntityRoute],
    blocked: Map[RouteId, Set[List[PathValue]]],
    candidates: mutable.HashMap[(RouteId, Vector[String]), List[(List[PathValue], ResponseValue)]],
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, EntityResult] = {
    val route  = group.head
    val routes = group.toList
    val batch  = prepareBatch(routes, candidates, blocked, cache)

    if (batch.entries.isEmpty) ZIO.succeed(EntityResult(Nil, batch.errors, batch.routes, batch.blocked))
    else {
      def failure = EntityResult(
        Nil,
        batch.errors ::: routes.map(route => RemoteError.at(routePath(route))),
        batch.routes,
        blockAll(batch)
      )

      graph
        .mapping(route.source)
        .flatMap(mapping =>
          buildLookup(route, routes, batch, resolvedRequest, mapping, cache).map(mapping -> _)
        ) match {
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
                    lookup.toClient(response, graph),
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
    val ordered   = correlationKeys(
      route.keys.map(key => key.field -> key),
      usedNames,
      "_caliban_gateway_entity_key"
    )
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
          IdentitySelections(
            correlationKeys(
              configured.map { case (responseField, keyField) =>
                keyField -> RequiredSelection(responseField, responseField)
              },
              usedNames,
              "_caliban_gateway_lookup_key"
            ),
            None
          )
        )
    }

  private def correlationKeys(
    fields: List[(String, RequiredSelection)],
    usedNames: Set[String],
    aliasBase: String
  ): List[CorrelationKey] =
    fields
      .foldLeft((List.empty[CorrelationKey], usedNames)) { case ((keys, names), (keyField, selection)) =>
        val alias = privateAlias(aliasBase, names)
        (CorrelationKey(keyField, selection.copy(responseName = alias)) :: keys, names + alias)
      }
      ._1
      .reverse

  private def buildLookup(
    route: EntityRoute,
    routes: List[EntityRoute],
    batch: EntityBatch,
    resolvedRequest: GraphQLRequest,
    mapping: SchemaMapping,
    cache: PlanExecutionCache
  ): Option[LookupCall] = {
    val prepared         = preparedLookup(route, mapping, cache)
    val executableFields = prepared.executableFields
    val sourceSelections = prepared.sourceSelections

    def responseToClient(correlation: EntityCorrelation): ResponseValue => ResponseValue = {
      val requiredToClient = mapping.requiredResponseMapper(route.entityType, correlation.required)
      value => requiredToClient(prepared.fieldsToClient(value))
    }

    def sourceSelectionsFor(correlation: EntityCorrelation): List[Selection] =
      sourceSelections ::: correlation.required
        .map(value => requiredSelection(mapping.requiredSelectionToSource(route.entityType, value)))

    def lookupExecution(
      operation: OperationDefinition,
      variables: Option[Map[String, InputValue]],
      correlation: EntityCorrelation,
      response: LookupResponse
    ): LookupCall =
      LookupCall(
        request(operation, variables, resolvedRequest),
        correlation,
        response,
        executableFields,
        responseToClient(correlation),
        prepared.restorer
      )

    route.lookup.operation match {
      case ComposedGraph.LookupOperation.FederationEntities(correlationKey)                                           =>
        val correlation =
          if (
            correlationKey.nonEmpty &&
            batch.entries.map(entry => correlationIdentity(route, entry.identity)).distinct.size == batch.entries.size
          )
            federationCorrelation(route, routes)
          else EntityCorrelation.Ordered
        val variables   = Map(
          "representations" -> InputListValue(
            batch.entries
              .map(entry => mapping.representationToSource(route.entityType, federationRepresentation(route, entry)))
              .toList
          )
        )
        val entityField = Selection.Field(
          None,
          "_entities",
          Map("representations" -> VariableValue("representations")),
          Nil,
          List(
            Selection.InlineFragment(
              Some(NamedType(mapping.sourceType(route.entityType), nonNull = false)),
              Nil,
              sourceSelectionsFor(correlation)
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
        Some(lookupExecution(operation, Some(variables), correlation, LookupResponse.ListRoot("_entities")))
      case ComposedGraph.LookupOperation.GraphQLQuery(field, mappings, result: ComposedGraph.LookupResult.ListResult) =>
        val correlation = graphqlCorrelation(route, routes, result)
        evaluateArguments(mappings, batch, None).map { arguments =>
          val alias           = "_caliban_gateway_lookup"
          val sourceArguments = mapping.lookupArgumentsToSource(field, arguments)
          val selection       = Selection.Field(
            Some(alias),
            mapping.lookupFieldToSource(field),
            sourceArguments,
            Nil,
            sourceSelectionsFor(correlation),
            0
          )
          val operation       = OperationDefinition(
            OperationType.Query,
            Some("__GatewayLookup"),
            Nil,
            Nil,
            List(selection)
          )
          lookupExecution(operation, None, correlation, LookupResponse.ListRoot(alias))
        }
      case ComposedGraph.LookupOperation.GraphQLQuery(field, mappings, ComposedGraph.LookupResult.Single)             =>
        val correlation = EntityCorrelation.Ordered
        val selections  = traverseOption(batch.entries.zipWithIndex) { case (entry, index) =>
          evaluateArguments(mappings, batch, Some(entry)).map { arguments =>
            val alias = s"_caliban_gateway_lookup_$index"
            Selection.Field(
              Some(alias),
              mapping.lookupFieldToSource(field),
              mapping.lookupArgumentsToSource(field, arguments),
              Nil,
              sourceSelectionsFor(correlation),
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
          lookupExecution(operation, None, correlation, LookupResponse.Aliases(indices.toMap))
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

  private def evaluateArguments(
    arguments: Map[String, ComposedGraph.LookupArgument],
    batch: EntityBatch,
    current: Option[EntityBatchEntry]
  ): Option[Map[String, InputValue]] =
    traverseOption(arguments.toList) { case (name, argument) =>
      evaluateArgument(argument, batch, current).map(name -> _)
    }
      .map(_.toMap)

  private def evaluateArgument(
    argument: ComposedGraph.LookupArgument,
    batch: EntityBatch,
    current: Option[EntityBatchEntry]
  ): Option[InputValue] =
    argument match {
      case ComposedGraph.LookupArgument.Key(field, expectedType) =>
        current.flatMap { entry =>
          var result: Option[InputValue] = None
          var remaining                  = entry.identity.keys
          while ((remaining ne Nil) && result.isEmpty) {
            val head = remaining.head
            if (head._1 == field) result = Some(head._2)
            remaining = remaining.tail
          }
          result
        }.map {
          case StringValue(value) if expectedType.kind == __TypeKind.ENUM =>
            EnumValue(value)
          case value                                                      => value
        }
      case ComposedGraph.LookupArgument.ObjectMapping(fields)    =>
        traverseOption(fields) { case (name, value) =>
          evaluateArgument(value, batch, current).map(name -> _)
        }
          .map(values => InputObjectValue(values.toMap))
      case ComposedGraph.LookupArgument.Batch(value)             =>
        traverseOption(batch.entries)(entry => evaluateArgument(value, batch, Some(entry)))
          .map(InputListValue.apply)
    }

  private def prepareBatch(
    routes: List[EntityRoute],
    candidates: mutable.HashMap[(RouteId, Vector[String]), List[(List[PathValue], ResponseValue)]],
    blocked: Map[RouteId, Set[List[PathValue]]],
    cache: PlanExecutionCache
  ): EntityBatch = {
    val entries                                               =
      mutable.LinkedHashMap.empty[Representation, mutable.ListBuffer[EntityLocation]]
    val errors                                                = mutable.ListBuffer.empty[CalibanError]
    val skipped                                               = mutable.Map.empty[RouteId, mutable.Set[List[PathValue]]]
    def skip(route: EntityRoute, path: List[PathValue]): Unit =
      skipped.getOrElseUpdate(route.id, mutable.Set.empty) += path

    routes.foreach { route =>
      val blockedPaths = PathIndex(
        route.dependencies.iterator.flatMap(dependency => blocked.getOrElse(dependency, Set.empty).iterator)
      )
      val objectType   = graph.isObjectType(route.entityType)
      candidates.getOrElse((route.root, route.mergePath), Nil).foreach {
        case (_, NullValue)           => ()
        case (path, obj: ObjectValue) =>
          if (blockedPaths.containsPrefixOf(path))
            skip(route, path)
          else if (
            objectType && route.typename.exists { selection =>
              obj.getOrNull(selection.responseName) match {
                case StringValue(runtimeType) => runtimeType != route.entityType
                case _                        => false
              }
            }
          )
            skip(route, path)
          else
            sourceRepresentation(route, obj, cache) match {
              case Some(representation) =>
                entries.get(representation) match {
                  case Some(locations) => locations += EntityLocation(route, path)
                  case None            =>
                    entries.put(representation, mutable.ListBuffer(EntityLocation(route, path)))
                }
              case None                 =>
                errors += missingRepresentation(route, path)
                skip(route, path)
            }
        case (path, _)                =>
          errors += missingRepresentation(route, path)
          skip(route, path)
      }
    }

    EntityBatch(
      entries.iterator.map { case (representation, locations) =>
        EntityBatchEntry(representation.identity, representation.requirements.toMap, locations.toList)
      }.toVector,
      errors.toList,
      skipped.iterator.map { case (route, paths) => route -> paths.toSet }.toMap,
      routes.map(_.id).toSet
    )
  }

  private def routeIdentitySelections(route: EntityRoute, cache: PlanExecutionCache): IdentitySelections =
    PlanExecutionCache.memoize(cache.identities, route.id)(
      IdentitySelections(route.keys.map(key => CorrelationKey(key.field, key)), route.typename)
    )

  private def sourceRepresentation(
    route: EntityRoute,
    value: ObjectValue,
    cache: PlanExecutionCache
  ): Option[Representation] = {
    val fields = IndexedFields(value)
    for {
      identity     <- readIdentity(route.entityType, routeIdentitySelections(route, cache), fields)
      requirements <- readRequirements(route.requirements, identity.typename, fields)
    } yield Representation(identity, requirements)
  }

  private def readRequirements(
    requirements: List[RequiredSelection],
    runtimeType: String,
    value: IndexedFields
  ): Option[List[(String, InputValue)]] =
    if (requirements.isEmpty) Some(Nil)
    else {
      val collected = List.newBuilder[(String, InputValue)]
      var remaining = requirements
      while (remaining ne Nil) {
        val selection = remaining.head
        if (appliesTo(selection, runtimeType)) {
          val input = value.get(selection.responseName) match {
            case Some(field) => selectedInput(selection, field, allowNull = true)
            case None        => None
          }
          input match {
            case Some(result) => collected += (selection.field -> result)
            case None         => return None
          }
        }
        remaining = remaining.tail
      }
      Some(collected.result())
    }

  private def readIdentity(
    entityType: String,
    selections: IdentitySelections,
    value: IndexedFields
  ): Option[EntityIdentity] = {
    val typename = selections.typename match {
      case Some(selection) =>
        value.get(selection.responseName) match {
          case Some(StringValue(value)) => value
          case _                        => null
        }
      case None            => entityType
    }
    if (typename eq null) None
    else
      selections.keys match {
        case key :: Nil =>
          value.get(key.selection.responseName).flatMap(selectedInput(key.selection, _)) match {
            case Some(input) => Some(EntityIdentity(typename, (key.keyField -> input) :: Nil))
            case None        => None
          }
        case keys       =>
          traverseOption(keys)(key =>
            value
              .get(key.selection.responseName)
              .flatMap(selectedInput(key.selection, _))
              .map(key.keyField -> _)
          )
            .map(EntityIdentity(typename, _))
      }
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
        case obj: ObjectValue  =>
          selectedObject(selection.children, selection.runtimeTypeAlias, obj, allowNull)
        case ListValue(values) =>
          traverseOption(values)(selectedInput(selection, _, allowNull)).map(InputListValue.apply)
        case _                 => None
      }

  private def responseInput(value: ResponseValue): Option[InputValue] =
    value match {
      case input: InputValue   => Some(input)
      case ObjectValue(fields) =>
        traverseOption(fields) { case (name, nested) => responseInput(nested).map(name -> _) }
          .map(values => InputObjectValue(values.toMap))
      case ListValue(values)   => traverseOption(values)(responseInput).map(InputListValue.apply)
      case _                   => None
    }

  private def selectedObject(
    selections: List[RequiredSelection],
    runtimeTypeAlias: Option[String],
    value: ObjectValue,
    allowNull: Boolean
  ): Option[InputObjectValue] = {
    val fields     = IndexedFields(value)
    val applicable = runtimeTypeAlias match {
      case None        => Some(selections)
      case Some(alias) =>
        fields.get(alias).collect { case StringValue(runtimeType) =>
          selections.filter(appliesTo(_, runtimeType))
        }
    }
    applicable.flatMap(values =>
      traverseOption(values)(selection =>
        fields
          .get(selection.responseName)
          .flatMap(selectedInput(selection, _, allowNull))
          .map(selection.field -> _)
      )
        .map(values => InputObjectValue(values.toMap))
    )
  }

  private def appliesTo(selection: RequiredSelection, runtimeType: String): Boolean =
    selection.conditions.forall(_.contains(runtimeType))

  private def entityCandidates(
    value: ResponseValue,
    fields: List[String],
    reversedPath: List[PathValue],
    collected: mutable.ListBuffer[(List[PathValue], ResponseValue)]
  ): Unit =
    value match {
      case ObjectValue(values) if fields ne Nil =>
        val head      = fields.head
        val tail      = fields.tail
        var remaining = values
        var found     = false
        while (!found && (remaining ne Nil)) {
          val field = remaining.head
          if (field._1 == head) {
            found = true
            entityCandidates(field._2, tail, PathValue.Key(head) :: reversedPath, collected)
          }
          remaining = remaining.tail
        }
      case ListValue(values)                    =>
        var index     = 0
        var remaining = values
        while (remaining ne Nil) {
          entityCandidates(remaining.head, fields, PathValue.Index(index) :: reversedPath, collected)
          index += 1
          remaining = remaining.tail
        }
      case NullValue                            => ()
      case other                                => collected += (reversedPath.reverse -> other)
    }

  private def correlateResponse(
    route: EntityRoute,
    batch: EntityBatch,
    lookup: LookupCall,
    response: GraphQLResponse[CalibanError],
    errorPolicy: GraphQLSource.ErrorPolicy
  ): EntityResult = {
    val expected       = lookup.correlation match {
      case _: EntityCorrelation.Keyed =>
        batch.entries.iterator.zipWithIndex.map { case (entry, index) =>
          correlationIdentity(route, entry.identity) -> index
        }.toMap
      case EntityCorrelation.Ordered  => Map.empty[EntityIdentity, Int]
    }
    val resolved       = mutable.Set.empty[Int]
    val patches        = mutable.LinkedHashMap.empty[Int, ResponseValue]
    val protocolErrors = mutable.ListBuffer.empty[CalibanError]
    val blocked        = mutableBlocked(batch.blocked)
    val values         = lookup.response.values(response.data)

    values.foreach {
      case (index, NullValue)          =>
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
      case (index, value: ObjectValue) =>
        val resolvedIndex = lookup.correlation match {
          case EntityCorrelation.Ordered      => batch.entries.lift(index).map(_ => index)
          case keyed: EntityCorrelation.Keyed =>
            readIdentity(route.entityType, keyed.identity, IndexedFields(value))
              .map(correlationIdentity(route, _))
              .flatMap(expected.get)
        }
        resolvedIndex match {
          case Some(entryIndex) if resolved.add(entryIndex) =>
            patches.put(entryIndex, value)
          case Some(_)                                      =>
            protocolErrors += duplicateEntityResult(route)
          case None                                         =>
            protocolErrors += unexpectedEntityResult(route)
        }
      case (_, _)                      =>
        protocolErrors += unexpectedEntityResult(route)
    }

    val missingBuilder = List.newBuilder[EntityBatchEntry]
    val mergedBuilder  = List.newBuilder[EntityPatch]
    var entryIndex     = 0
    batch.entries.foreach { entry =>
      if (!resolved.contains(entryIndex)) missingBuilder += entry
      else {
        val patch = patches.getOrElse(entryIndex, null)
        if (patch ne null)
          entry.locations.foreach(location => mergedBuilder += EntityPatch(location.route, location.path, patch))
      }
      entryIndex += 1
    }
    val missing        = missingBuilder.result()
    val merged         = mergedBuilder.result()
    missing.foreach(blockEntry(_, blocked))
    val relocated      = relocateErrors(route, batch, lookup, values.toMap, response.errors, errorPolicy)
    val unindexedError = response.errors.exists {
      case error: CalibanError.ExecutionError => lookup.response.errorIndex(error.path).isEmpty
      case _                                  => false
    }
    val missingErrors  =
      if (unindexedError) Nil
      else
        lookup.correlation match {
          case _: EntityCorrelation.ByKey                                  => Nil
          case EntityCorrelation.Ordered | _: EntityCorrelation.Federation =>
            missing.flatMap { entry =>
              entry.locations.map(location => missingEntityResult(location.route, location.path))
            }
        }
    val errors         = batch.errors :::
      relocated :::
      protocolErrors.toList :::
      missingErrors

    EntityResult(
      merged,
      errors,
      batch.routes,
      immutableBlocked(blocked)
    )
  }

  private def blockEntry(
    entry: EntityBatchEntry,
    blocked: mutable.Map[RouteId, mutable.Set[List[PathValue]]]
  ): Unit =
    entry.locations.foreach(location => blocked.getOrElseUpdate(location.route.id, mutable.Set.empty) += location.path)

  private def blockAll(batch: EntityBatch): Map[RouteId, Set[List[PathValue]]] = {
    val blocked = mutableBlocked(batch.blocked)
    batch.entries.foreach(blockEntry(_, blocked))
    immutableBlocked(blocked)
  }

  private def mutableBlocked(
    blocked: Map[RouteId, Set[List[PathValue]]]
  ): mutable.Map[RouteId, mutable.Set[List[PathValue]]] = {
    val result = mutable.Map.empty[RouteId, mutable.Set[List[PathValue]]]
    blocked.foreach { case (route, paths) => result.put(route, mutable.Set.empty ++= paths) }
    result
  }

  private def immutableBlocked(
    blocked: mutable.Map[RouteId, mutable.Set[List[PathValue]]]
  ): Map[RouteId, Set[List[PathValue]]] =
    blocked.iterator.map { case (route, paths) => route -> paths.toSet }.toMap

  private def relocateErrors(
    route: EntityRoute,
    batch: EntityBatch,
    lookup: LookupCall,
    values: Map[Int, ResponseValue],
    errors: List[CalibanError],
    errorPolicy: GraphQLSource.ErrorPolicy
  ): List[CalibanError] = {
    lazy val mergedPaths = mergePaths(route, batch)
    errors.flatMap {
      case error: CalibanError.ExecutionError =>
        lookup.response.errorIndex(error.path) match {
          case Some((index, tail)) =>
            val locations  = entityLocations(route, batch, lookup.correlation, values.get(index), index)
            val clientTail = graph.restoreResponsePath(route.fields, lookup.executableFields, tail)
            if (locations.isEmpty) mergedPaths.map(errorPolicy.unusableEntity(error, _))
            else
              locations.map { location =>
                if (clientTail.isEmpty || RemoteError.hasClientPath(location.route.fields, clientTail))
                  error.copy(path = location.path ::: clientTail, locationInfo = None)
                else errorPolicy.unusableEntity(error, location.path)
              }
          case None                =>
            mergedPaths.map(errorPolicy.unusableEntity(error, _))
        }
      case error                              => List(error)
    }
  }

  private def entityLocations(
    route: EntityRoute,
    batch: EntityBatch,
    correlation: EntityCorrelation,
    value: Option[ResponseValue],
    index: Int
  ): List[EntityLocation] =
    correlation match {
      case EntityCorrelation.Ordered      => batch.entries.lift(index).map(_.locations).getOrElse(Nil)
      case keyed: EntityCorrelation.Keyed =>
        val byIdentity = value.collect { case obj: ObjectValue =>
          readIdentity(route.entityType, keyed.identity, IndexedFields(obj))
        }.flatten
          .map(correlationIdentity(route, _))
          .flatMap(identity => batch.entries.find(entry => correlationIdentity(route, entry.identity) == identity))
          .map(_.locations)
        val positional = keyed match {
          case _: EntityCorrelation.Federation => batch.entries.lift(index).map(_.locations)
          case _: EntityCorrelation.ByKey      => None
        }
        byIdentity.orElse(positional).getOrElse(Nil)
    }

  private def correlationIdentity(route: EntityRoute, identity: EntityIdentity): EntityIdentity =
    route.lookup.representationType.fold(identity)(typename => identity.copy(typename = typename))

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

  private[internal] final case class PreparedLookup(
    executableFields: List[Field],
    sourceSelections: List[Selection],
    fieldsToClient: ResponseValue => ResponseValue,
    restorer: Option[Map[String, ComposedGraph.ResponseNameMapping]]
  )

  final case class EntityResult(
    patches: List[EntityPatch],
    errors: List[CalibanError],
    completed: Set[RouteId],
    blocked: Map[RouteId, Set[List[PathValue]]]
  )

  private[internal] final case class EntityGroupKey(
    source: String,
    entityType: String,
    lookup: ComposedGraph.EntityLookup,
    keys: List[RequiredSelection],
    requirements: List[RequiredSelection],
    selection: String
  ) {
    @transient @threadUnsafe
    final override lazy val hashCode: Int = Hash.caseClassHash(this)
  }

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
        else {
          val readyIds = ready.iterator.map(_.id).toSet
          count(
            pending.filterNot(route => readyIds.contains(route.id)),
            completed ++ readyIds,
            calls + ready.iterator.map(entityGroupKey).toSet.size
          )
        }
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
      route.selectionKey
    )

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

  private[internal] final case class CorrelationKey(keyField: String, selection: RequiredSelection)

  private[internal] final case class IdentitySelections(
    keys: List[CorrelationKey],
    typename: Option[RequiredSelection]
  )

  private final case class LookupCall(
    request: GraphQLRequest,
    correlation: EntityCorrelation,
    response: LookupResponse,
    executableFields: List[Field],
    responseToClient: ResponseValue => ResponseValue,
    restorer: Option[Map[String, ComposedGraph.ResponseNameMapping]]
  ) {
    def toClient(result: GraphQLResponse[CalibanError], graph: ComposedGraph): GraphQLResponse[CalibanError] =
      result.copy(data = response.map(result.data) { value =>
        val translated = responseToClient(value)
        restorer.fold(translated)(graph.restoreResponseNames(_, translated))
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
            fields.collectFirst { case (`root`, ListValue(values)) =>
              var index     = 0
              val collected = List.newBuilder[(Int, ResponseValue)]
              var remaining = values
              while (remaining ne Nil) {
                collected += (index -> remaining.head)
                index += 1
                remaining = remaining.tail
              }
              collected.result()
            }.getOrElse(Nil)
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

  private final case class EntityIdentity(typename: String, keys: List[(String, InputValue)]) {
    @transient @threadUnsafe
    final override lazy val hashCode: Int = namedValuesHash(typename.hashCode, keys)

    final override def equals(other: Any): Boolean =
      other match {
        case that: EntityIdentity => (this eq that) || (typename == that.typename && namedValuesEqual(keys, that.keys))
        case _                    => false
      }
  }

  private final case class Representation(identity: EntityIdentity, requirements: List[(String, InputValue)]) {
    @transient @threadUnsafe
    final override lazy val hashCode: Int = namedValuesHash(identity.hashCode, requirements)

    final override def equals(other: Any): Boolean =
      other match {
        case that: Representation =>
          (this eq that) || (identity == that.identity && namedValuesEqual(requirements, that.requirements))
        case _                    => false
      }
  }

  private def namedValuesHash(seed: Int, values: List[(String, InputValue)]): Int = {
    var hash      = seed
    var remaining = values
    while (remaining ne Nil) {
      val head = remaining.head
      hash = hash * 31 + head._1.hashCode
      hash = hash * 31 + head._2.hashCode
      remaining = remaining.tail
    }
    hash
  }

  private def namedValuesEqual(left: List[(String, InputValue)], right: List[(String, InputValue)]): Boolean = {
    var remainingLeft  = left
    var remainingRight = right
    while ((remainingLeft ne Nil) && (remainingRight ne Nil)) {
      val leftHead  = remainingLeft.head
      val rightHead = remainingRight.head
      if (leftHead._1 != rightHead._1 || leftHead._2 != rightHead._2) return false
      remainingLeft = remainingLeft.tail
      remainingRight = remainingRight.tail
    }
    (remainingLeft eq Nil) && (remainingRight eq Nil)
  }

  private final case class EntityLocation(route: EntityRoute, path: List[PathValue])

  private final case class EntityBatchEntry(
    identity: EntityIdentity,
    requirements: Map[String, InputValue],
    locations: List[EntityLocation]
  )

  private final case class EntityBatch(
    entries: Vector[EntityBatchEntry],
    errors: List[CalibanError],
    blocked: Map[RouteId, Set[List[PathValue]]],
    routes: Set[RouteId]
  )
}
