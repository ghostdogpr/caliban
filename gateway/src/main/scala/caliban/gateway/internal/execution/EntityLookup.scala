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
private[execution] final class EntityLookup(graph: ComposedGraph) {
  def prepare(
    fetch: EntityFetch,
    batch: EntityBatch,
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache,
    slot: Option[Int]
  ): Option[Call] = {
    val mapping       = graph.schemaMapping(fetch.source)
    val contextValues = batch.entries.headOption.map(_.contextArguments).getOrElse(Map.empty)
    val prepared      = prepareLookup(fetch, mapping, cache, contextValues)

    def expectedIdentities: Map[EntityIdentity, Int] =
      batch.entries.iterator.zipWithIndex.map { case (entry, index) =>
        correlationIdentity(fetch, entry.identity) -> index
      }.toMap

    def call(
      request: GraphQLRequest,
      variant: PreparedVariant,
      response: ResponseShape,
      expected: Map[EntityIdentity, Int],
      part: Option[CallPart] = None
    ): Call =
      new Call(fetch, batch, request, variant.correlation, response, variant.projection, expected, part)

    def lookupField(
      field: String,
      alias: String,
      arguments: Map[String, InputValue],
      variant: GraphQLVariant
    ): Selection.Field =
      Selection.Field(
        Some(alias),
        mapping.lookupFieldToSource(field),
        mapping.lookupArgumentsToSource(field, arguments),
        Nil,
        variant.sourceSelections,
        0
      )

    def queryCall(
      selections: List[Selection],
      variant: GraphQLVariant,
      response: ResponseShape,
      expected: Map[EntityIdentity, Int]
    ): Call = {
      val operation = OperationDefinition(OperationType.Query, Some("__GatewayLookup"), Nil, Nil, selections)
      val request   = GraphQLRequest(
        query = Some(render(operation)),
        operationName = operation.name,
        extensions = resolvedRequest.extensions
      )
      call(request, variant, response, expected)
    }

    prepared match {
      case PreparedLookup.Federation(ordered, keyed)       =>
        // Duplicate identities need positional correlation to distinguish their requirement values.
        val (variant, expected) = keyed match {
          case Some(keyed) =>
            val identities = expectedIdentities
            if (identities.size == batch.entries.size) keyed -> identities
            else ordered                                     -> Map.empty[EntityIdentity, Int]
          case None        => ordered -> Map.empty[EntityIdentity, Int]
        }
        val representations     = InputListValue(
          batch.entries
            .map(entry => mapping.representationToSource(fetch.entityType, federationRepresentation(fetch, entry)))
            .toList
        )
        val request             = GraphQLRequest(
          query = Some(variant.query),
          operationName = Some("__GatewayEntity"),
          variables = Some(Map("representations" -> representations)),
          extensions = resolvedRequest.extensions
        )
        slot match {
          case None       => Some(call(request, variant, ResponseShape.ListRoot("_entities"), expected))
          case Some(slot) =>
            val part = CallPart(slot, variant.selections, representations)
            Some(call(request, variant, ResponseShape.ListRoot(part.alias), expected, Some(part)))
        }
      case PreparedLookup.ByKey(field, mappings, variant)  =>
        evaluateArguments(mappings, batch, None).map { arguments =>
          val alias = "_caliban_gateway_lookup"
          queryCall(
            List(lookupField(field, alias, arguments, variant)),
            variant,
            ResponseShape.ListRoot(alias),
            expectedIdentities
          )
        }
      case PreparedLookup.Single(field, mappings, variant) =>
        val selections = traverseOption(batch.entries.zipWithIndex) { case (entry, index) =>
          evaluateArguments(mappings, batch, Some(entry)).map { arguments =>
            val alias = s"_caliban_gateway_lookup_$index"
            lookupField(field, alias, arguments, variant) -> (alias -> index)
          }
        }
        selections.map { generated =>
          val (values, indices) = generated.unzip
          queryCall(values, variant, ResponseShape.Aliases(indices.toMap), Map.empty[EntityIdentity, Int])
        }
    }
  }

  final class Call private[EntityLookup] (
    fetch: EntityFetch,
    batch: EntityBatch,
    val request: GraphQLRequest,
    correlation: EntityCorrelation,
    shape: ResponseShape,
    projection: ResponseProjection,
    expected: Map[EntityIdentity, Int],
    val part: Option[CallPart]
  ) {
    def complete(response: GraphQLResponse[CalibanError], errorPolicy: SubgraphExecutor.ErrorPolicy): EntityResult = {
      val values   = shape.values(response.data).map { case (index, value) => index -> projection(value) }
      val assigned = assign(values)

      val (missing, merged) = collectPatches(assigned.slots)

      val errors = batch.errors :::
        relocateErrors(values.toMap, response.errors, errorPolicy) :::
        assigned.protocolErrors :::
        surplusNullErrors(assigned.federationNulls, missing.size) :::
        missingErrors(response.errors, missing, assigned.federationNulls)

      EntityResult(merged, errors, blockEntries(batch.blocked, assigned.blocked ::: missing), batch.unmatched)
    }

    private def assign(values: List[(Int, ResponseValue)]): Assignment = {
      val slots           = new Slots(batch.entries.size)
      val protocolErrors  = List.newBuilder[CalibanError]
      val blocked         = List.newBuilder[EntityBatchEntry]
      var federationNulls = 0

      values.foreach {
        case (index, NullValue)          =>
          correlation match {
            case EntityCorrelation.Ordered       =>
              batch.entries.lift(index) match {
                case Some(entry) if slots.isEmpty(index) =>
                  slots.fill(index, NullValue)
                  blocked += entry
                case Some(_)                             => protocolErrors += duplicateEntityResult(fetch)
                case None                                => protocolErrors += unexpectedEntityResult(fetch)
              }
            case _: EntityCorrelation.Federation => federationNulls += 1
            case _: EntityCorrelation.ByKey      => protocolErrors += unexpectedEntityResult(fetch)
          }
        case (index, value: ObjectValue) =>
          entryIndex(index, Some(value)) match {
            case Some(entry) if slots.isEmpty(entry) => slots.fill(entry, value)
            case Some(_)                             => protocolErrors += duplicateEntityResult(fetch)
            case None                                => protocolErrors += unexpectedEntityResult(fetch)
          }
        case (_, _)                      =>
          protocolErrors += unexpectedEntityResult(fetch)
      }

      Assignment(slots, protocolErrors.result(), blocked.result(), federationNulls)
    }

    private def collectPatches(slots: Slots): (List[EntityBatchEntry], List[EntityPatch]) = {
      val missing = List.newBuilder[EntityBatchEntry]
      val merged  = List.newBuilder[EntityPatch]
      var index   = 0
      batch.entries.foreach { entry =>
        if (slots.isEmpty(index)) missing += entry
        else {
          val patch = slots(index)
          if (patch != NullValue)
            entry.locations.foreach(location => merged += EntityPatch(location.fetch, location.path, patch))
        }
        index += 1
      }
      (missing.result(), merged.result())
    }

    private def surplusNullErrors(federationNulls: Int, missing: Int): List[CalibanError] =
      correlation match {
        case _: EntityCorrelation.Federation =>
          List.fill(math.max(0, federationNulls - missing))(unexpectedEntityResult(fetch))
        case _                               => Nil
      }

    private def missingErrors(
      responseErrors: List[CalibanError],
      missing: List[EntityBatchEntry],
      federationNulls: Int
    ): List[CalibanError] = {
      val unindexedError = responseErrors.exists {
        case error: CalibanError.ExecutionError => shape.errorIndex(error.path).isEmpty
        case _                                  => false
      }
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
    }

    private def relocateErrors(
      values: Map[Int, ResponseValue],
      errors: List[CalibanError],
      errorPolicy: SubgraphExecutor.ErrorPolicy
    ): List[CalibanError] = {
      lazy val fallbackPaths = mergePaths
      errors.flatMap {
        case error: CalibanError.ExecutionError =>
          shape.errorIndex(error.path) match {
            case Some((index, tail)) =>
              val locations  = entityLocations(index, values.get(index))
              val clientTail = projection.path(tail)
              if (locations.isEmpty) fallbackPaths.map(errorPolicy.entityFallback(error, _))
              else
                locations.map { location =>
                  if (clientTail.isEmpty || RemoteError.hasClientPath(location.fetch.fields, clientTail))
                    error.copy(path = location.path ::: clientTail, locationInfo = None)
                  else errorPolicy.entityFallback(error, location.path)
                }
            case None                =>
              fallbackPaths.map(errorPolicy.entityFallback(error, _))
          }
        case error                              => List(error)
      }
    }

    private def entityLocations(index: Int, value: Option[ResponseValue]): List[EntityLocation] =
      entryIndex(index, value).flatMap(batch.entries.lift).map(_.locations).getOrElse(Nil)

    private def entryIndex(index: Int, value: Option[ResponseValue]): Option[Int] =
      correlation.entryIndex(fetch, expected, index, value).filter(batch.entries.isDefinedAt)

    private def mergePaths: List[List[PathValue]] = {
      val paths = mutable.LinkedHashSet.empty[List[PathValue]]
      batch.entries.foreach(_.locations.foreach(location => paths += fetchPath(location.fetch)))
      paths += fetchPath(fetch)
      paths.toList
    }
  }

  private def prepareLookup(
    fetch: EntityFetch,
    mapping: SchemaMapping,
    cache: PlanExecutionCache,
    contextValues: Map[ContextualArgument, InputValue]
  ): PreparedLookup = {
    def prepare: PreparedLookup = {
      val contextualFields = injectContextArguments(fetch.source, fetch.fields, contextValues)
      val executableFields = graph.prepareEntityFields(fetch.source, fetch.entityType, contextualFields)
      val sourceSelections = executableFields.flatMap(field => fieldSelection(mapping.fieldToSource(field)))

      def selections(correlation: EntityCorrelation): List[Selection] =
        sourceSelections ::: correlation.required
          .map(value => requiredSelection(mapping.requiredSelectionToSource(fetch.entityType, value)))

      def projection(correlation: EntityCorrelation): ResponseProjection =
        mapping.responseProjection(fetch.fields, executableFields, correlation.required)

      def federation(correlation: EntityCorrelation): FederationVariant = {
        val fragments = federationFragments(fetch, mapping, selections(correlation))
        FederationVariant(
          correlation,
          projection(correlation),
          render(federationOperation(fragments)),
          renderSelections(fragments)
        )
      }

      def graphql(correlation: EntityCorrelation): GraphQLVariant =
        GraphQLVariant(correlation, selections(correlation), projection(correlation))

      fetch.lookup.operation match {
        case ComposedGraph.LookupOperation.FederationEntities(correlatesByKey)                                     =>
          PreparedLookup.Federation(
            federation(EntityCorrelation.Ordered),
            if (correlatesByKey) Some(federation(federationCorrelation(fetch, executableFields))) else None
          )
        case ComposedGraph.LookupOperation.GraphQLQuery(field, arguments, ComposedGraph.LookupResult.Single)       =>
          PreparedLookup.Single(field, arguments, graphql(EntityCorrelation.Ordered))
        case ComposedGraph.LookupOperation.GraphQLQuery(field, arguments, byKey: ComposedGraph.LookupResult.ByKey) =>
          PreparedLookup.ByKey(field, arguments, graphql(graphqlCorrelation(fetch, byKey, executableFields)))
      }
    }
    if (contextValues.isEmpty) cache.lookup(fetch.id)(prepare) else prepare
  }

  private def federationCorrelation(fetch: EntityFetch, executableFields: List[Field]): EntityCorrelation.Federation = {
    val usedNames = executableFields.iterator.map(_.aliasedName).toSet
    val ordered   = correlationKeys(fetch.keys.map(key => key.field -> key), usedNames, "_caliban_gateway_entity_key")
    val names     = usedNames ++ ordered.iterator.map(_.selection.responseName)
    EntityCorrelation.Federation(
      IdentitySelections(
        ordered,
        Some(RequiredSelection("__typename", privateAlias("_caliban_gateway_entity_typename", names)))
      )
    )
  }

  private def graphqlCorrelation(
    fetch: EntityFetch,
    result: ComposedGraph.LookupResult.ByKey,
    executableFields: List[Field]
  ): EntityCorrelation.ByKey = {
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

  private def federationFragments(
    fetch: EntityFetch,
    mapping: SchemaMapping,
    sourceSelections: List[Selection]
  ): List[Selection] =
    List(
      Selection.InlineFragment(
        Some(NamedType(mapping.sourceType(fetch.entityType), nonNull = false)),
        Nil,
        sourceSelections
      )
    )

  private def federationOperation(fragments: List[Selection]): OperationDefinition = {
    val entityField = Selection.Field(
      None,
      "_entities",
      Map("representations" -> VariableValue("representations")),
      Nil,
      fragments,
      0
    )
    OperationDefinition(
      OperationType.Query,
      Some("__GatewayEntity"),
      List(
        VariableDefinition("representations", ListType(NamedType("_Any", nonNull = true), nonNull = true), None, Nil)
      ),
      Nil,
      List(entityField)
    )
  }

  private def render(operation: OperationDefinition): String =
    DocumentRenderer.renderCompact(Document(operation :: Nil, SourceMapper.empty))

  private def renderSelections(selections: List[Selection]): String =
    DocumentRenderer.selectionsRenderer.renderCompact(selections)

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
          Selection.InlineFragment(Some(NamedType(target, nonNull = false)), Nil, field.toSelection :: Nil)
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
              .sourceField(source, parent, field.name)
              .flatMap(_.allArgs.find(_.name == context.argument))
              .map(_._type)
            val input    = expected.fold(value)(coerceInput(value, _))
            context.argument -> input
        }.toMap
        field.copy(arguments = field.arguments ++ added, fields = injectContextArguments(source, field.fields, values))
      }

  private def coerceInput(value: InputValue, expected: __Type): InputValue =
    expected.kind match {
      case __TypeKind.NON_NULL => expected.ofType.fold(value)(coerceInput(value, _))
      case __TypeKind.LIST     =>
        (value, expected.ofType) match {
          case (InputValue.ListValue(values), Some(element)) =>
            InputValue.ListValue(values.map(coerceInput(_, element)))
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
        current
          .flatMap(_.identity.keys.collectFirst { case (`field`, value) => value })
          .map(coerceInput(_, expectedType))
      case ComposedGraph.LookupArgument.ObjectMapping(fields)    =>
        traverseOption(fields) { case (name, value) =>
          evaluateArgument(value, batch, current).map(name -> _)
        }
          .map(values => InputObjectValue(values.toMap))
      case ComposedGraph.LookupArgument.Batch(value)             =>
        traverseOption(batch.entries)(entry => evaluateArgument(value, batch, Some(entry)))
          .map(InputListValue.apply)
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

private[execution] object EntityLookup {
  def combine(parts: List[CallPart], resolvedRequest: GraphQLRequest): GraphQLRequest =
    GraphQLRequest(
      query = Some(
        parts.map(part => s"$$${part.variable}:[_Any!]!").mkString("query __GatewayEntity(", ",", ")") +
          parts.map(_.field).mkString("{", " ", "}")
      ),
      operationName = Some("__GatewayEntity"),
      variables = Some(parts.map(part => part.variable -> part.representations).toMap),
      extensions = resolvedRequest.extensions
    )

  def partResponse(response: GraphQLResponse[CalibanError], part: CallPart): GraphQLResponse[CalibanError] =
    if (response.errors.isEmpty) response
    else
      response.copy(errors = response.errors.filter {
        case error: CalibanError.ExecutionError =>
          error.path match {
            case PathValue.Key(alias) :: _ if CallPart.isPartAlias(alias) => alias == part.alias
            case _                                                        => true
          }
        case _                                  => true
      })

  sealed trait PreparedLookup

  object PreparedLookup {
    final case class Federation(ordered: FederationVariant, keyed: Option[FederationVariant]) extends PreparedLookup

    final case class Single(
      field: String,
      arguments: Map[String, ComposedGraph.LookupArgument],
      variant: GraphQLVariant
    ) extends PreparedLookup

    final case class ByKey(
      field: String,
      arguments: Map[String, ComposedGraph.LookupArgument],
      variant: GraphQLVariant
    ) extends PreparedLookup
  }

  sealed trait PreparedVariant {
    def correlation: EntityCorrelation
    def projection: ResponseProjection
  }

  final case class FederationVariant(
    correlation: EntityCorrelation,
    projection: ResponseProjection,
    query: String,
    selections: String
  ) extends PreparedVariant

  final case class GraphQLVariant(
    correlation: EntityCorrelation,
    sourceSelections: List[Selection],
    projection: ResponseProjection
  ) extends PreparedVariant

  final case class CallPart(slot: Int, selections: String, representations: InputValue) {
    val alias: String    = s"${CallPart.AliasPrefix}$slot"
    val variable: String = s"_caliban_gateway_representations_$slot"
    def field: String    = s"$alias:_entities(representations:$$$variable)$selections"
  }

  object CallPart {
    def isPartAlias(alias: String): Boolean = alias.startsWith(AliasPrefix)

    private val AliasPrefix = "_caliban_gateway_entities_"
  }

  private def correlationIdentity(fetch: EntityFetch, identity: EntityIdentity): EntityIdentity =
    fetch.lookup.representationType.fold(identity)(typename => identity.copy(typename = typename))

  private final class Slots(size: Int) {
    private val values = new Array[ResponseValue](size)

    def isEmpty(index: Int): Boolean = values(index) eq null

    def fill(index: Int, value: ResponseValue): Unit = values(index) = value

    def apply(index: Int): ResponseValue = values(index)
  }

  private final case class Assignment(
    slots: Slots,
    protocolErrors: List[CalibanError],
    blocked: List[EntityBatchEntry],
    federationNulls: Int
  )

  sealed trait EntityCorrelation {
    def required: List[RequiredSelection]

    def entryIndex(
      fetch: EntityFetch,
      expected: Map[EntityIdentity, Int],
      index: Int,
      value: Option[ResponseValue]
    ): Option[Int]
  }

  object EntityCorrelation {
    case object Ordered extends EntityCorrelation {
      val required: List[RequiredSelection] = Nil

      def entryIndex(
        fetch: EntityFetch,
        expected: Map[EntityIdentity, Int],
        index: Int,
        value: Option[ResponseValue]
      ): Option[Int] = Some(index)
    }

    sealed trait Keyed extends EntityCorrelation {
      def identity: IdentitySelections

      def required: List[RequiredSelection] =
        identity.keys.map(_.selection) ::: identity.typename.toList

      def entryIndex(
        fetch: EntityFetch,
        expected: Map[EntityIdentity, Int],
        index: Int,
        value: Option[ResponseValue]
      ): Option[Int] =
        value.collect { case obj: ObjectValue => identity.read(fetch.entityType, IndexedFields(obj)) }.flatten
          .map(correlationIdentity(fetch, _))
          .flatMap(expected.get)
    }

    final case class Federation(identity: IdentitySelections) extends Keyed
    final case class ByKey(identity: IdentitySelections)      extends Keyed
  }

  private sealed trait ResponseShape {
    def values(data: ResponseValue): List[(Int, ResponseValue)]
    def errorIndex(path: List[PathValue]): Option[(Int, List[PathValue])]
  }

  private object ResponseShape {
    final case class ListRoot(root: String) extends ResponseShape {
      def values(data: ResponseValue): List[(Int, ResponseValue)] =
        data match {
          case ObjectValue(fields) =>
            fields.collectFirst { case (`root`, ListValue(values)) =>
              values.zipWithIndex.map(_.swap)
            }.getOrElse(Nil)
          case _                   => Nil
        }

      def errorIndex(path: List[PathValue]): Option[(Int, List[PathValue])] =
        path match {
          case PathValue.Key(`root`) :: PathValue.Index(index) :: tail if index >= 0 => Some(index -> tail)
          case _                                                                     => None
        }

    }

    final case class Aliases(indices: Map[String, Int]) extends ResponseShape {
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

    }
  }
}
