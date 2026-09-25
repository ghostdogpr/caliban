package caliban.gateway.internal.execution

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, PathValue, ResponseValue }
import caliban.execution.Field
import caliban.gateway.internal.PrivateAliases
import caliban.gateway.internal.composition.{ ComposedGraph, SchemaMapping }
import caliban.gateway.internal.execution.EntityExecutor._
import caliban.gateway.internal.execution.EntityLookup._
import caliban.gateway.internal.execution.PlanExecutor.renderOperation
import caliban.gateway.internal.planning.OperationPlan._
import caliban.gateway._
import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue, VariableValue }
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.adt.{ OperationType, Selection, VariableDefinition }
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.rendering.DocumentRenderer
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ EnumValue, NullValue, StringValue }

import scala.collection.mutable

/**
 * Builds entity lookup requests together with the rules for translating and correlating their responses.
 * Generated aliases and correlation selections stay private to each call.
 */
private[execution] final class EntityLookup(graph: ComposedGraph) {
  def prepare(fetch: EntityFetch, batch: EntityBatch, cache: PlanExecutionCache): Option[(GraphQLRequest, Call)] = {
    val mapping = graph.schemaMapping(fetch.source)
    fetch.lookup.operation match {
      case ComposedGraph.LookupOperation.FederationEntities                                                      =>
        val variant = federationVariant(fetch, mapping, batch, cache)
        val request = GraphQLRequest(
          query = Some(variant.query),
          operationName = Some(EntityOperationName),
          variables = Some(Map(RepresentationsVariable -> representations(fetch, mapping, batch)))
        )
        Some(request -> new Call(fetch, batch, variant, ResponseShape.ListRoot(EntitiesField), Map.empty))
      case ComposedGraph.LookupOperation.GraphQLQuery(field, arguments, ComposedGraph.LookupResult.Single)       =>
        val variant = graphqlVariant(fetch, mapping, batch, cache)(_ => EntityCorrelation.Ordered)
        val aliases = Vector.tabulate(batch.entries.size)(index => s"${LookupAlias}_$index")
        traverseOption(batch.entries.zip(aliases)) { case (entry, alias) =>
          evaluateArguments(arguments, batch, Some(entry)).map(lookupField(mapping, field, alias, _, variant))
        }.map(fields =>
          lookupRequest(fields) -> new Call(fetch, batch, variant, ResponseShape.Aliases(aliases), Map.empty)
        )
      case ComposedGraph.LookupOperation.GraphQLQuery(field, arguments, byKey: ComposedGraph.LookupResult.ByKey) =>
        val variant = graphqlVariant(fetch, mapping, batch, cache)(graphqlCorrelation(fetch, byKey, _))
        evaluateArguments(arguments, batch, None).map { values =>
          lookupRequest(List(lookupField(mapping, field, LookupAlias, values, variant))) ->
            new Call(fetch, batch, variant, ResponseShape.ListRoot(LookupAlias), expectedIdentities(fetch, batch))
        }
    }
  }

  def preparePart(fetch: EntityFetch, batch: EntityBatch, cache: PlanExecutionCache, slot: Int): (CallPart, Call) = {
    val mapping = graph.schemaMapping(fetch.source)
    val variant = federationVariant(fetch, mapping, batch, cache)
    val part    = CallPart(slot, variant.selections, representations(fetch, mapping, batch))
    part -> new Call(fetch, batch, variant, ResponseShape.ListRoot(part.alias), Map.empty)
  }

  private def representations(fetch: EntityFetch, mapping: SchemaMapping, batch: EntityBatch): InputValue =
    InputListValue(
      batch.entries
        .map(entry => mapping.representationToSource(fetch.entityType, federationRepresentation(fetch, entry)))
        .toList
    )

  private def lookupField(
    mapping: SchemaMapping,
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

  private def lookupRequest(selections: List[Selection]): GraphQLRequest = {
    val operation = OperationDefinition(OperationType.Query, Some(LookupOperationName), Nil, Nil, selections)
    GraphQLRequest(query = Some(renderOperation(operation)), operationName = operation.name)
  }

  final class Call private[EntityLookup] (
    fetch: EntityFetch,
    batch: EntityBatch,
    variant: PreparedVariant,
    shape: ResponseShape,
    expected: Map[EntityIdentity, Int]
  ) {
    private val correlation = variant.correlation
    private val projection  = variant.projection

    def complete(response: GraphQLResponse[CalibanError], errorPolicy: SubgraphExecutor.ErrorPolicy): EntityResult = {
      val values   = shape.values(response.data).map { case (index, value) => index -> projection(value) }
      val assigned = assign(values)

      val (missing, merged) = collectPatches(assigned.slots)

      val errors = batch.errors :::
        (if (response.errors.isEmpty) Nil else relocateErrors(values.toMap, response.errors, errorPolicy)) :::
        assigned.protocolErrors :::
        missingErrors(response.errors, missing)

      EntityResult(merged, errors, blockEntries(batch.blocked, assigned.blocked ::: missing), batch.unmatched)
    }

    private def assign(values: List[(Int, ResponseValue)]): Assignment = {
      val slots          = new Slots(batch.entries.size)
      val protocolErrors = List.newBuilder[CalibanError]
      val blocked        = List.newBuilder[EntityBatchEntry]

      values.foreach {
        case (index, NullValue)          =>
          correlation match {
            case EntityCorrelation.Ordered  =>
              batch.entries.lift(index) match {
                case Some(entry) =>
                  slots.fill(index, NullValue)
                  blocked += entry
                case None        => protocolErrors += unexpectedEntityResult(fetch)
              }
            case _: EntityCorrelation.ByKey => protocolErrors += unexpectedEntityResult(fetch)
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

      Assignment(slots, protocolErrors.result(), blocked.result())
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

    private def missingErrors(
      responseErrors: List[CalibanError],
      missing: List[EntityBatchEntry]
    ): List[CalibanError] = {
      val unindexedError = responseErrors.exists {
        case error: CalibanError.ExecutionError => shape.errorIndex(error.path).isEmpty
        case _                                  => false
      }
      if (unindexedError) Nil
      else
        correlation match {
          case _: EntityCorrelation.ByKey => Nil
          case EntityCorrelation.Ordered  =>
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

  private def federationVariant(
    fetch: EntityFetch,
    mapping: SchemaMapping,
    batch: EntityBatch,
    cache: PlanExecutionCache
  ): FederationVariant = {
    def prepare: FederationVariant = {
      val executableFields = executableEntityFields(fetch, batch)
      val fragments        = federationFragments(fetch, mapping, sourceSelections(mapping, executableFields))
      FederationVariant(
        ResponseProjection.compile(fetch.fields, executableFields, Nil, mapping.typeNames),
        renderOperation(federationOperation(fragments)),
        renderSelections(fragments)
      )
    }
    if (contextValues(batch).isEmpty) cache.federationLookup(fetch.id)(prepare) else prepare
  }

  private def graphqlVariant(fetch: EntityFetch, mapping: SchemaMapping, batch: EntityBatch, cache: PlanExecutionCache)(
    correlate: List[Field] => EntityCorrelation
  ): GraphQLVariant = {
    def prepare: GraphQLVariant = {
      val executableFields = executableEntityFields(fetch, batch)
      val correlation      = correlate(executableFields)
      GraphQLVariant(
        correlation,
        sourceSelections(mapping, executableFields) ::: correlation.required
          .map(value => requiredSelection(mapping.requiredSelectionToSource(fetch.entityType, value))),
        ResponseProjection.compile(fetch.fields, executableFields, correlation.required, mapping.typeNames)
      )
    }
    if (contextValues(batch).isEmpty) cache.graphqlLookup(fetch.id)(prepare) else prepare
  }

  private def contextValues(batch: EntityBatch): Map[ContextualArgument, InputValue] =
    batch.entries.headOption.fold(Map.empty[ContextualArgument, InputValue])(_.contextArguments)

  private def executableEntityFields(fetch: EntityFetch, batch: EntityBatch): List[Field] =
    graph.prepareEntityFields(
      fetch.source,
      fetch.entityType,
      injectContextArguments(fetch.source, fetch.fields, contextValues(batch))
    )

  private def sourceSelections(mapping: SchemaMapping, executableFields: List[Field]): List[Selection] =
    executableFields.flatMap(field => fieldSelection(mapping.fieldToSource(field)))

  private def graphqlCorrelation(
    fetch: EntityFetch,
    result: ComposedGraph.LookupResult.ByKey,
    executableFields: List[Field]
  ): EntityCorrelation.ByKey = {
    val fields     = result.fields
    val usedNames  = responseNames(executableFields)
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
          usedNames
        ),
        None
      )
    )
  }

  private def correlationKeys(
    fields: List[(String, RequiredSelection)],
    usedNames: Set[String]
  ): List[CorrelationKey] = {
    val aliases = new PrivateAliases(usedNames)
    fields.map { case (keyField, selection) =>
      CorrelationKey(keyField, selection.copy(responseName = aliases.next(LookupKeyAliasBase)))
    }
  }

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
      EntitiesField,
      Map(RepresentationsArgument -> VariableValue(RepresentationsVariable)),
      Nil,
      fragments,
      0
    )
    OperationDefinition(
      OperationType.Query,
      Some(EntityOperationName),
      List(
        VariableDefinition(
          RepresentationsVariable,
          ListType(NamedType(AnyType, nonNull = true), nonNull = true),
          None,
          Nil
        )
      ),
      Nil,
      List(entityField)
    )
  }

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
        val parent = parentTypeName(field)
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
        (TypenameField -> StringValue(fetch.lookup.representationType.getOrElse(entry.identity.typename)))
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
    lookupResponseError(fetch, "contained a duplicate result", fetchPath(fetch))

  private def unexpectedEntityResult(fetch: EntityFetch): CalibanError.ExecutionError =
    lookupResponseError(fetch, "contained an unexpected result", fetchPath(fetch))

  private def missingEntityResult(fetch: EntityFetch, path: List[PathValue]): CalibanError.ExecutionError =
    lookupResponseError(fetch, "omitted a result", path)

  private def lookupResponseError(
    fetch: EntityFetch,
    detail: String,
    path: List[PathValue]
  ): CalibanError.ExecutionError =
    CalibanError.ExecutionError(s"Entity lookup response $detail for '${entityKey(fetch)}'.", path = path)
}

private[execution] object EntityLookup {
  def combine(parts: List[CallPart]): GraphQLRequest =
    GraphQLRequest(
      query = Some(
        parts.map(part => s"$$${part.variable}:[$AnyType!]!").mkString("query " + EntityOperationName + "(", ",", ")") +
          parts.map(_.field).mkString("{", " ", "}")
      ),
      operationName = Some(EntityOperationName),
      variables = Some(parts.map(part => part.variable -> part.representations).toMap)
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

  sealed trait PreparedVariant {
    def correlation: EntityCorrelation
    def projection: ResponseProjection
  }

  final case class FederationVariant(projection: ResponseProjection, query: String, selections: String)
      extends PreparedVariant {
    def correlation: EntityCorrelation = EntityCorrelation.Ordered
  }

  final case class GraphQLVariant(
    correlation: EntityCorrelation,
    sourceSelections: List[Selection],
    projection: ResponseProjection
  ) extends PreparedVariant

  final case class CallPart(slot: Int, selections: String, representations: InputValue) {
    val alias: String    = s"${CallPart.AliasPrefix}$slot"
    val variable: String = s"${CallPart.VariablePrefix}$slot"
    def field: String    = s"$alias:$EntitiesField($RepresentationsArgument:$$$variable)$selections"
  }

  object CallPart {
    def isPartAlias(alias: String): Boolean = alias.startsWith(AliasPrefix)

    private final val AliasPrefix    = "_caliban_gateway_entities_"
    private final val VariablePrefix = "_caliban_gateway_representations_"
  }

  private final val EntityOperationName     = "__GatewayEntity"
  private final val LookupOperationName     = "__GatewayLookup"
  private final val RepresentationsVariable = "representations"
  private final val LookupAlias             = "_caliban_gateway_lookup"
  private final val LookupKeyAliasBase      = "_caliban_gateway_lookup_key"

  private def expectedIdentities(fetch: EntityFetch, batch: EntityBatch): Map[EntityIdentity, Int] =
    batch.entries.iterator.zipWithIndex.map { case (entry, index) =>
      correlationIdentity(fetch, entry.identity) -> index
    }.toMap

  private def correlationIdentity(fetch: EntityFetch, identity: EntityIdentity): EntityIdentity =
    fetch.lookup.representationType.fold(identity)(typename => identity.copy(typename = typename))

  private final class Slots(size: Int) {
    private val values = new Array[ResponseValue](size)

    def isEmpty(index: Int): Boolean = values(index) eq null

    def fill(index: Int, value: ResponseValue): Unit = values(index) = value

    def apply(index: Int): ResponseValue = values(index)
  }

  private final case class Assignment(slots: Slots, protocolErrors: List[CalibanError], blocked: List[EntityBatchEntry])

  sealed trait EntityCorrelation {
    def required: List[RequiredSelection]

    private[execution] def entryIndex(
      fetch: EntityFetch,
      expected: Map[EntityIdentity, Int],
      index: Int,
      value: Option[ResponseValue]
    ): Option[Int]
  }

  object EntityCorrelation {
    case object Ordered extends EntityCorrelation {
      val required: List[RequiredSelection] = Nil

      private[execution] def entryIndex(
        fetch: EntityFetch,
        expected: Map[EntityIdentity, Int],
        index: Int,
        value: Option[ResponseValue]
      ): Option[Int] = Some(index)
    }

    final case class ByKey(identity: IdentitySelections) extends EntityCorrelation {
      def required: List[RequiredSelection] =
        identity.keys.map(_.selection) ::: identity.typename.toList

      private[execution] def entryIndex(
        fetch: EntityFetch,
        expected: Map[EntityIdentity, Int],
        index: Int,
        value: Option[ResponseValue]
      ): Option[Int] =
        value.collect { case obj: ObjectValue => identity.read(fetch.entityType, IndexedFields(obj)) }.flatten
          .map(correlationIdentity(fetch, _))
          .flatMap(expected.get)
    }
  }

  private sealed trait ResponseShape {
    def values(data: ResponseValue): List[(Int, ResponseValue)]
    def errorIndex(path: List[PathValue]): Option[(Int, List[PathValue])]
  }

  private object ResponseShape {
    final case class ListRoot(root: String) extends ResponseShape {
      def values(data: ResponseValue): List[(Int, ResponseValue)] =
        data match {
          case obj: ObjectValue =>
            obj.getOrNull(root) match {
              case ListValue(values) => values.zipWithIndex.map(_.swap)
              case _                 => Nil
            }
          case _                => Nil
        }

      def errorIndex(path: List[PathValue]): Option[(Int, List[PathValue])] =
        path match {
          case PathValue.Key(`root`) :: PathValue.Index(index) :: tail if index >= 0 => Some(index -> tail)
          case _                                                                     => None
        }

    }

    final case class Aliases(aliases: Vector[String]) extends ResponseShape {
      private lazy val indices: Map[String, Int] = aliases.iterator.zipWithIndex.toMap

      def values(data: ResponseValue): List[(Int, ResponseValue)] =
        data match {
          case obj: ObjectValue =>
            val fields = IndexedFields(obj)
            aliases.iterator.zipWithIndex.flatMap { case (alias, index) => fields.get(alias).map(index -> _) }.toList
          case _                => Nil
        }

      def errorIndex(path: List[PathValue]): Option[(Int, List[PathValue])] =
        path match {
          case PathValue.Key(alias) :: tail => indices.get(alias).map(_ -> tail)
          case _                            => None
        }

    }
  }
}
