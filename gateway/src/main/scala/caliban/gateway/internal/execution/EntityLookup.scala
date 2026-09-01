package caliban.gateway.internal.execution

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, PathValue, ResponseValue }
import caliban.execution.Field
import caliban.gateway.internal.composition.{ ComposedGraph, SchemaMapping }
import caliban.gateway.internal.execution.EntityExecutor._
import caliban.gateway.internal.execution.EntityLookup._
import caliban.gateway.internal.planning.OperationPlan._
import caliban.gateway.traverseOption
import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue, VariableValue }
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.adt.{ Document, OperationType, Selection, VariableDefinition }
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.SourceMapper
import caliban.rendering.DocumentRenderer
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ EnumValue, NullValue, StringValue }

import scala.collection.mutable

/**
 * Builds entity lookup requests together with the rules for translating and correlating their responses.
 * Generated aliases and correlation selections stay private to each call.
 */
private[internal] final class EntityLookup(
  graph: ComposedGraph,
  responseMappings: Map[String, ResponseMapping]
) {
  def prepare(
    fetches: List[EntityFetch],
    batch: EntityBatch,
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  ): Option[Call] =
    for {
      fetch   <- fetches.headOption
      mapping <- graph.mapping(fetch.source)
      call    <- buildLookup(fetch, batch, resolvedRequest, mapping, cache)
    } yield call

  private def preparedLookup(
    fetch: EntityFetch,
    mapping: SchemaMapping,
    responseMapping: ResponseMapping,
    cache: PlanExecutionCache,
    contextValues: Map[ContextualArgument, InputValue]
  ): PreparedLookup = {
    def prepare: PreparedLookup = {
      val contextualFields = injectContextArguments(fetch.source, fetch.fields, contextValues)
      val executableFields = graph.executableEntityFields(fetch.source, fetch.entityType, contextualFields)
      PreparedLookup(
        executableFields,
        executableFields.map(mapping.rootFieldToSource).flatMap(fieldSelection),
        responseMapping.entityFieldsResponseMapper(executableFields),
        ResponseMapping.responseNameRestorer(fetch.fields, executableFields)
      )
    }
    if (contextValues.isEmpty) PlanExecutionCache.memoize(cache.lookups, fetch.id)(prepare) else prepare
  }

  private def federationCorrelation(
    fetch: EntityFetch,
    executableFields: List[Field]
  ): EntityCorrelation.Federation = {
    val usedNames = executableFields.iterator.map(_.aliasedName).toSet
    val ordered   = correlationKeys(
      fetch.keys.map(key => key.field -> key),
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
    fetch: EntityFetch,
    result: ComposedGraph.LookupResult.ByKey,
    executableFields: List[Field]
  ): EntityCorrelation = {
    val fields     = result.fields
    val usedNames  = executableFields.iterator.map(_.aliasedName).toSet
    val configured = fetch.keys.flatMap(key =>
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
    fetch: EntityFetch,
    batch: EntityBatch,
    resolvedRequest: GraphQLRequest,
    mapping: SchemaMapping,
    cache: PlanExecutionCache
  ): Option[Call] = {
    val responseMapping  = responseMappings(fetch.source)
    val contextValues    = batch.entries.headOption.map(_.contextArguments).getOrElse(Map.empty)
    val prepared         = preparedLookup(fetch, mapping, responseMapping, cache, contextValues)
    val executableFields = prepared.executableFields
    val sourceSelections = prepared.sourceSelections

    def responseToClient(correlation: EntityCorrelation): ResponseValue => ResponseValue = {
      val requiredToClient =
        responseMapping.requiredResponseMapper(fetch.entityType, correlation.required)
      value => requiredToClient(prepared.fieldsToClient(value))
    }

    def sourceSelectionsFor(correlation: EntityCorrelation): List[Selection] =
      sourceSelections ::: correlation.required
        .map(value => requiredSelection(responseMapping.requiredSelectionToSource(fetch.entityType, value)))

    def lookupExecution(
      operation: OperationDefinition,
      variables: Option[Map[String, InputValue]],
      correlation: EntityCorrelation,
      response: LookupResponse
    ): Call =
      new Call(
        fetch,
        batch,
        request(operation, variables, resolvedRequest),
        correlation,
        response,
        executableFields,
        responseToClient(correlation),
        prepared.restorer
      )

    fetch.lookup.operation match {
      case ComposedGraph.LookupOperation.FederationEntities(correlationKey)                                      =>
        val correlation =
          if (
            correlationKey.nonEmpty &&
            batch.entries.map(entry => correlationIdentity(fetch, entry.identity)).distinct.size == batch.entries.size
          )
            federationCorrelation(fetch, executableFields)
          else EntityCorrelation.Ordered
        val variables   = Map(
          "representations" -> InputListValue(
            batch.entries
              .map(entry => mapping.representationToSource(fetch.entityType, federationRepresentation(fetch, entry)))
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
              Some(NamedType(mapping.sourceType(fetch.entityType), nonNull = false)),
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
      case ComposedGraph.LookupOperation.GraphQLQuery(field, mappings, result: ComposedGraph.LookupResult.ByKey) =>
        val correlation = graphqlCorrelation(fetch, result, executableFields)
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
      case ComposedGraph.LookupOperation.GraphQLQuery(field, mappings, ComposedGraph.LookupResult.Single)        =>
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

  private def injectContextArguments(
    source: String,
    fields: List[Field],
    values: Map[ContextualArgument, InputValue]
  ): List[Field] =
    if (values.isEmpty) fields
    else
      fields.map { field =>
        val parent = field.parentType.flatMap(_.name).getOrElse("")
        val added  = values.iterator.collect {
          case (context, value) if context.parentType == parent && context.field == field.name =>
            val expected = graph
              .field(source, parent, field.name)
              .flatMap(_.allArgs.find(_.name == context.argument))
              .map(_._type)
            val input    = expected.fold(value)(coerceContextInput(value, _))
            context.argument -> input
        }.toMap
        field.copy(arguments = field.arguments ++ added, fields = injectContextArguments(source, field.fields, values))
      }

  private def coerceContextInput(value: InputValue, expected: __Type): InputValue =
    expected.kind match {
      case __TypeKind.NON_NULL => expected.ofType.fold(value)(coerceContextInput(value, _))
      case __TypeKind.LIST     =>
        (value, expected.ofType) match {
          case (InputValue.ListValue(values), Some(element)) =>
            InputValue.ListValue(values.map(coerceContextInput(_, element)))
          case _                                             => value
        }
      case __TypeKind.ENUM     =>
        value match {
          case StringValue(entry) => EnumValue(entry)
          case _                  => value
        }
      case _                   => value
    }

  private def federationRepresentation(fetch: EntityFetch, entry: EntityBatchEntry): InputObjectValue =
    InputObjectValue(
      entry.identity.keys.toMap ++ entry.requirements +
        ("__typename" -> StringValue(fetch.lookup.representationType.getOrElse(entry.identity.typename)))
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

  final class Call private[EntityLookup] (
    fetch: EntityFetch,
    batch: EntityBatch,
    val request: GraphQLRequest,
    correlation: EntityCorrelation,
    shape: LookupResponse,
    executableFields: List[Field],
    responseToClient: ResponseValue => ResponseValue,
    restorer: Option[Map[String, ResponseMapping.ResponseNameMapping]]
  ) {
    def complete(
      result: GraphQLResponse[CalibanError],
      errorPolicy: SubgraphExecutor.ErrorPolicy
    ): EntityResult =
      correlateResponse(toClient(result), errorPolicy)

    private def toClient(result: GraphQLResponse[CalibanError]): GraphQLResponse[CalibanError] =
      result.copy(data = shape.map(result.data) { value =>
        val translated = responseToClient(value)
        restorer.fold(translated)(ResponseMapping.restoreResponseNames(_, translated))
      })

    private def correlateResponse(
      response: GraphQLResponse[CalibanError],
      errorPolicy: SubgraphExecutor.ErrorPolicy
    ): EntityResult = {
      val expected        = correlation match {
        case _: EntityCorrelation.Keyed =>
          batch.entries.iterator.zipWithIndex.map { case (entry, index) =>
            correlationIdentity(fetch, entry.identity) -> index
          }.toMap
        case EntityCorrelation.Ordered  => Map.empty[EntityIdentity, Int]
      }
      val resolved        = mutable.Set.empty[Int]
      val patches         = mutable.LinkedHashMap.empty[Int, ResponseValue]
      val protocolErrors  = mutable.ListBuffer.empty[CalibanError]
      val blocked         = mutableBlocked(batch.blocked)
      val values          = shape.values(response.data)
      var federationNulls = 0

      values.foreach {
        case (index, NullValue)          =>
          correlation match {
            case EntityCorrelation.Ordered       =>
              batch.entries.lift(index) match {
                case Some(entry) if resolved.add(index) => blockEntry(entry, blocked)
                case Some(_)                            => protocolErrors += duplicateEntityResult(fetch)
                case None                               => protocolErrors += unexpectedEntityResult(fetch)
              }
            case _: EntityCorrelation.Federation => federationNulls += 1
            case _: EntityCorrelation.ByKey      =>
              protocolErrors += unexpectedEntityResult(fetch)
          }
        case (index, value: ObjectValue) =>
          val resolvedIndex = correlation match {
            case EntityCorrelation.Ordered      => batch.entries.lift(index).map(_ => index)
            case keyed: EntityCorrelation.Keyed =>
              keyed.identity
                .read(fetch.entityType, IndexedFields(value))
                .map(correlationIdentity(fetch, _))
                .flatMap(expected.get)
          }
          resolvedIndex match {
            case Some(entryIndex) if resolved.add(entryIndex) =>
              patches.put(entryIndex, value)
            case Some(_)                                      =>
              protocolErrors += duplicateEntityResult(fetch)
            case None                                         =>
              protocolErrors += unexpectedEntityResult(fetch)
          }
        case (_, _)                      =>
          protocolErrors += unexpectedEntityResult(fetch)
      }

      val missingBuilder = List.newBuilder[EntityBatchEntry]
      val mergedBuilder  = List.newBuilder[EntityPatch]
      var entryIndex     = 0
      batch.entries.foreach { entry =>
        if (!resolved.contains(entryIndex)) missingBuilder += entry
        else {
          val patch = patches.getOrElse(entryIndex, null)
          if (patch ne null)
            entry.locations.foreach(location => mergedBuilder += EntityPatch(location.fetch, location.path, patch))
        }
        entryIndex += 1
      }
      val missing        = missingBuilder.result()
      val merged         = mergedBuilder.result()
      missing.foreach(blockEntry(_, blocked))
      correlation match {
        case _: EntityCorrelation.Federation =>
          var surplusNulls = federationNulls - missing.size
          while (surplusNulls > 0) {
            protocolErrors += unexpectedEntityResult(fetch)
            surplusNulls -= 1
          }
        case _                               => ()
      }
      val relocated      = relocateErrors(values.toMap, response.errors, errorPolicy)
      val unindexedError = response.errors.exists {
        case error: CalibanError.ExecutionError => shape.errorIndex(error.path).isEmpty
        case _                                  => false
      }
      val missingErrors  =
        if (unindexedError) Nil
        else
          correlation match {
            case _: EntityCorrelation.ByKey                                  => Nil
            case _: EntityCorrelation.Federation if federationNulls > 0      =>
              List.fill(math.max(0, missing.size - federationNulls))(missingEntityResult(fetch, fetchPath(fetch)))
            case EntityCorrelation.Ordered | _: EntityCorrelation.Federation =>
              missing.flatMap { entry =>
                entry.locations.map(location => missingEntityResult(location.fetch, location.path))
              }
          }
      val errors         = batch.errors :::
        relocated :::
        protocolErrors.toList :::
        missingErrors

      EntityResult(
        merged,
        errors,
        batch.fetchIds,
        immutableBlocked(blocked)
      )
    }

    private def relocateErrors(
      values: Map[Int, ResponseValue],
      errors: List[CalibanError],
      errorPolicy: SubgraphExecutor.ErrorPolicy
    ): List[CalibanError] = {
      lazy val mergedPaths = mergePaths(fetch, batch)
      errors.flatMap {
        case error: CalibanError.ExecutionError =>
          shape.errorIndex(error.path) match {
            case Some((index, tail)) =>
              val locations  = entityLocations(fetch, batch, correlation, values.get(index), index)
              val clientTail = ResponseMapping.restoreResponsePath(fetch.fields, executableFields, tail)
              if (locations.isEmpty) mergedPaths.map(errorPolicy.unusableEntity(error, _))
              else
                locations.map { location =>
                  if (clientTail.isEmpty || RemoteError.hasClientPath(location.fetch.fields, clientTail))
                    error.copy(path = location.path ::: clientTail, locationInfo = None)
                  else errorPolicy.unusableEntity(error, location.path)
                }
            case None                =>
              mergedPaths.map(errorPolicy.unusableEntity(error, _))
          }
        case error                              => List(error)
      }
    }
  }

  private def entityLocations(
    fetch: EntityFetch,
    batch: EntityBatch,
    correlation: EntityCorrelation,
    value: Option[ResponseValue],
    index: Int
  ): List[EntityLocation] =
    correlation match {
      case EntityCorrelation.Ordered      => batch.entries.lift(index).map(_.locations).getOrElse(Nil)
      case keyed: EntityCorrelation.Keyed =>
        value.collect { case obj: ObjectValue =>
          keyed.identity.read(fetch.entityType, IndexedFields(obj))
        }.flatten
          .map(correlationIdentity(fetch, _))
          .flatMap(identity => batch.entries.find(entry => correlationIdentity(fetch, entry.identity) == identity))
          .map(_.locations)
          .getOrElse(Nil)
    }

  private def correlationIdentity(fetch: EntityFetch, identity: EntityIdentity): EntityIdentity =
    fetch.lookup.representationType.fold(identity)(typename => identity.copy(typename = typename))

  private def mergePaths(fetch: EntityFetch, batch: EntityBatch): List[List[PathValue]] = {
    val paths = mutable.LinkedHashSet.empty[List[PathValue]]
    batch.entries.foreach(_.locations.foreach(location => paths += fetchPath(location.fetch)))
    paths += fetchPath(fetch)
    paths.toList
  }

  private def duplicateEntityResult(fetch: EntityFetch): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      s"Entity lookup response contained a duplicate result for '${entityKey(fetch)}'.",
      path = fetchPath(fetch)
    )

  private def unexpectedEntityResult(fetch: EntityFetch): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      s"Entity lookup response contained an unexpected result for '${entityKey(fetch)}'.",
      path = fetchPath(fetch)
    )

  private def missingEntityResult(fetch: EntityFetch, path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      s"Entity lookup response omitted a result for '${entityKey(fetch)}'.",
      path = path
    )
}

private[internal] object EntityLookup {
  private[internal] final case class PreparedLookup(
    executableFields: List[Field],
    sourceSelections: List[Selection],
    fieldsToClient: ResponseValue => ResponseValue,
    restorer: Option[Map[String, ResponseMapping.ResponseNameMapping]]
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
}
