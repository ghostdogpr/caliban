package caliban.gateway.internal.execution

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, PathValue, ResponseValue }
import caliban.execution.Field
import caliban.gateway.internal.PrivateAliases
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.composition.ComposedGraph.Source
import caliban.gateway.internal.execution.EntityExecutor._
import caliban.gateway.internal.execution.EntityLookup._
import caliban.gateway.internal.execution.PlanExecutor.renderOperation
import caliban.gateway.internal.execution.ResponseMerge.{ Overwrite, Patch }
import caliban.gateway.internal.planning.OperationPlan._
import caliban.gateway._
import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.adt.{ OperationType, Selection }
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Type.NamedType
import caliban.rendering.DocumentRenderer
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ EnumValue, NullValue, StringValue }

/**
 * Builds entity lookup requests together with the rules for translating and correlating their responses.
 * Generated aliases and correlation selections stay private to each call.
 */
private[internal] object EntityLookup {
  def prepare(batch: EntityBatch): Option[(GraphQLRequest, Call)] =
    batch.variant match {
      case variant: FederationVariant                                   =>
        val request = GraphQLRequest(
          query = Some(variant.query),
          operationName = Some(EntityOperationName),
          variables = Some(Map(RepresentationsVariable -> representations(batch)))
        )
        Some(request -> new Call(batch, variant.projection, ResponseShape.Ordered(EntitiesField)))
      case SingleVariant(projection, field, arguments, selections)      =>
        val aliases = Vector.tabulate(batch.entries.size)(index => s"${LookupAlias}_$index")
        traverseOption(batch.entries.zip(aliases)) { case (entry, alias) =>
          argumentValues(arguments)(keyValue(entry)).map(lookupField(field, alias, _, selections))
        }.map(fields => lookupRequest(fields) -> new Call(batch, projection, ResponseShape.Aliases(aliases)))
      case ByKeyVariant(projection, field, arguments, keys, selections) =>
        val values = argumentValues(arguments) { key =>
          traverseOption(batch.entries)(entry => argumentValue(key)(keyValue(entry))).map(InputListValue(_))
        }
        values.map { values =>
          lookupRequest(List(lookupField(field, LookupAlias, values, selections))) ->
            new Call(batch, projection, ResponseShape.Keyed(LookupAlias, keys))
        }
    }

  def preparePart(batch: EntityBatch, variant: FederationVariant, slot: Int): (CallPart, Call) = {
    val part = CallPart(slot, variant.selections, representations(batch))
    part -> new Call(batch, variant.projection, ResponseShape.Ordered(part.alias))
  }

  private def representations(batch: EntityBatch): InputValue = {
    val target   = batch.fetch.target
    val typename = if (target.source.isInterfaceObject(target.entityType)) Some(target.entityType) else None
    InputListValue(
      batch.entries
        .map(entry =>
          target.source.mapping.representationToSource(target.entityType, federationRepresentation(typename, entry))
        )
        .toList
    )
  }

  private def lookupField(
    field: String,
    alias: String,
    arguments: Map[String, InputValue],
    selections: List[Selection]
  ): Selection.Field =
    Selection.Field(Some(alias), field, arguments, Nil, selections, 0)

  private def lookupRequest(selections: List[Selection]): GraphQLRequest = {
    val operation = OperationDefinition(OperationType.Query, Some(LookupOperationName), Nil, Nil, selections)
    GraphQLRequest(query = Some(renderOperation(operation)), operationName = operation.name)
  }

  final class Call private[EntityLookup] (
    batch: EntityBatch,
    projection: ResponseProjection,
    shape: ResponseShape
  ) {
    private val fetch = batch.fetch

    def complete(response: GraphQLResponse[CalibanError], errorPolicy: SubgraphExecutor.ErrorPolicy): EntityResult = {
      val values         = shape.values(response.data).map { case (index, value) => index -> projection(value) }
      val slots          = new Array[ResponseValue](batch.entries.size)
      val protocolErrors = assign(values, slots)

      lazy val byIndex = values.toMap
      val attributed   = response.errors.map { error =>
        val execution = executionError(error)
        execution -> attribute(execution, byIndex)
      }
      // An error attributed to an omitted result stands for a null result.
      attributed.foreach {
        case (_, Some((entry, _))) if slots(entry) eq null => slots(entry) = NullValue
        case _                                             =>
      }

      val missing = List.newBuilder[EntityBatchEntry]
      val blocked = List.newBuilder[EntityLocation]
      val merged  = List.newBuilder[(FetchId, Patch)]
      var index   = 0
      batch.entries.foreach { entry =>
        if (slots(index) eq null) {
          missing += entry
          blocked ++= entry.locations
        } else {
          val patch = slots(index)
          if (patch eq NullValue) blocked ++= entry.locations
          else entry.locations.foreach(location => merged += location.fetch.root -> (location.path -> Overwrite(patch)))
        }
        index += 1
      }

      val errors =
        attributed.flatMap { case (error, attribution) => relocate(error, attribution, errorPolicy) } :::
          protocolErrors ::: missingErrors(attributed, missing.result())

      EntityResult(merged.result(), errors, blocked.result())
    }

    private def assign(values: List[(Int, ResponseValue)], slots: Array[ResponseValue]): List[CalibanError] = {
      val protocolErrors = List.newBuilder[CalibanError]
      values.foreach { case (index, value) =>
        entryIndex(index, Some(value)) match {
          case Some(entry) if slots(entry) eq null =>
            value match {
              case NullValue | _: ObjectValue => slots(entry) = value
              case _                          =>
                slots(entry) = NullValue
                protocolErrors ++= batch.mergePaths.map(unexpectedEntityResult(fetch, _))
            }
          case Some(_)                             => protocolErrors ++= batch.mergePaths.map(duplicateEntityResult(fetch, _))
          case None                                => protocolErrors ++= batch.mergePaths.map(unexpectedEntityResult(fetch, _))
        }
      }
      protocolErrors.result()
    }

    private def missingErrors(
      attributed: List[(CalibanError.ExecutionError, Option[(Int, List[PathValue])])],
      missing: List[EntityBatchEntry]
    ): List[CalibanError] =
      shape match {
        case _: ResponseShape.Keyed => Nil
        case _                      =>
          if (attributed.exists(_._2.isEmpty)) Nil
          else
            missing.flatMap { entry =>
              entry.locations.map(location => missingEntityResult(location.fetch, location.path))
            }
      }

    private def relocate(
      error: CalibanError.ExecutionError,
      attribution: Option[(Int, List[PathValue])],
      errorPolicy: SubgraphExecutor.ErrorPolicy
    ): List[CalibanError] =
      attribution match {
        case Some((entry, tail)) =>
          val clientTail = projection.path(tail)
          batch.entries(entry).locations.map { location =>
            if (clientTail.isEmpty) error.copy(path = location.path)
            else
              errorPolicy
                .place(location.fetch.fields, location.path, error, clientTail)
                .getOrElse(errorPolicy.entityFallback(error, location.path))
          }
        case None                =>
          batch.mergePaths.map(errorPolicy.entityFallback(error, _))
      }

    private def attribute(
      error: CalibanError.ExecutionError,
      values: => Map[Int, ResponseValue]
    ): Option[(Int, List[PathValue])] =
      shape.errorIndex(error.path).flatMap { case (index, tail) =>
        entryIndex(index, values.get(index)).map(_ -> tail)
      }

    private lazy val expected = batch.entries.iterator.map(_.identity).zipWithIndex.toMap

    private def entryIndex(index: Int, value: Option[ResponseValue]): Option[Int] =
      (shape match {
        case ResponseShape.Keyed(_, keys) =>
          value.collect { case obj: ObjectValue =>
            readIdentity(fetch.target.entityType, keys, IndexedFields(obj))
          }.flatten
            .flatMap(expected.get)
        case _                            => Some(index)
      }).filter(batch.entries.isDefinedAt)

  }

  def variant(fetch: EntityFetch, contexts: Map[ContextualArgument, InputValue]): Variant = {
    val target     = fetch.target
    val mapping    = target.source.mapping
    val executable = target.source.prepareFields(
      target.entityType,
      injectContextArguments(target.source, fetch.fields, contexts)
    )
    val projection = ResponseProjection.compile(fetch.fields, executable, mapping.typeNames)
    val selections = executable.flatMap(field => targetedSelections(mapping.fieldToSource(field)))
    target.lookup.operation match {
      case ComposedGraph.LookupOperation.FederationEntities       =>
        val fragment =
          Selection.InlineFragment(
            Some(NamedType(mapping.sourceType(target.entityType), nonNull = false)),
            Nil,
            selections
          )
        FederationVariant(projection, DocumentRenderer.selectionsRenderer.renderCompact(fragment :: Nil))
      case ComposedGraph.LookupOperation.Single(field, arguments) =>
        SingleVariant(projection, field, arguments, selections)
      case ComposedGraph.LookupOperation.ByKey(field, arguments)  =>
        val aliases = new PrivateAliases(responseNames(executable))
        val keys    = target.keys.map(key => RequiredSelection(key.field, aliases.next(LookupKeyAliasBase)))
        val fields  = keys.map(key => requiredSelection(mapping.requiredSelectionToSource(target.entityType, key)))
        ByKeyVariant(projection, field, arguments, keys, selections ::: fields)
    }
  }

  private def requiredSelection(value: RequiredSelection): Selection =
    Selection.Field(
      if (value.responseName == value.field) None else Some(value.responseName),
      value.field,
      Map.empty,
      Nil,
      value.children.map(requiredSelection),
      0
    )

  private def injectContextArguments(
    source: Source,
    fields: List[Field],
    values: Map[ContextualArgument, InputValue]
  ): List[Field] =
    if (values.isEmpty) fields
    else
      fields.map { field =>
        val parent = parentTypeName(field)
        val added  = values.iterator.collect {
          case (context, value) if context.at.typeName == parent && context.at.fieldName == field.name =>
            val expected = source
              .sourceField(parent, field.name)
              .flatMap(_.allArgs.find(_.name == context.at.argumentName))
              .map(_._type)
            val input    = expected.fold(value)(coerceInput(value, _))
            context.at.argumentName -> input
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

  private def federationRepresentation(typename: Option[String], entry: EntityBatchEntry): InputObjectValue =
    InputObjectValue(
      entry.identity.keys.toMap ++ entry.requirements +
        (TypenameField -> StringValue(typename.getOrElse(entry.identity.typename)))
    )

  private def argumentValues[A](arguments: List[(String, Lookup.Argument[A])])(
    leaf: A => Option[InputValue]
  ): Option[Map[String, InputValue]] =
    traverseOption(arguments) { case (name, argument) => argumentValue(argument)(leaf).map(name -> _) }.map(_.toMap)

  private def argumentValue[A](argument: Lookup.Argument[A])(leaf: A => Option[InputValue]): Option[InputValue] =
    argument match {
      case Lookup.Argument.Leaf(value)           => leaf(value)
      case Lookup.Argument.ObjectMapping(fields) => argumentValues(fields)(leaf).map(InputObjectValue(_))
    }

  private def keyValue(entry: EntityBatchEntry)(key: ComposedGraph.KeyArgument): Option[InputValue] =
    entry.identity.keys.collectFirst { case (key.field, value) => coerceInput(value, key.expectedType) }

  private def executionError(error: CalibanError): CalibanError.ExecutionError =
    error match {
      case error: CalibanError.ExecutionError  => error.copy(locationInfo = None)
      case error: CalibanError.ValidationError => CalibanError.ExecutionError(error.msg, extensions = error.extensions)
      case error: CalibanError.ParsingError    => CalibanError.ExecutionError(error.msg, extensions = error.extensions)
    }

  private def duplicateEntityResult(fetch: EntityFetch, path: List[PathValue]): CalibanError.ExecutionError =
    lookupResponseError(fetch, "contained a duplicate result", path)

  private def unexpectedEntityResult(fetch: EntityFetch, path: List[PathValue]): CalibanError.ExecutionError =
    lookupResponseError(fetch, "contained an unexpected result", path)

  private def missingEntityResult(fetch: EntityFetch, path: List[PathValue]): CalibanError.ExecutionError =
    lookupResponseError(fetch, "omitted a result", path)

  private def lookupResponseError(
    fetch: EntityFetch,
    detail: String,
    path: List[PathValue]
  ): CalibanError.ExecutionError =
    CalibanError.ExecutionError(s"Entity lookup response $detail for '${entityKey(fetch)}'.", path = path)

  def combine(parts: List[CallPart]): GraphQLRequest =
    GraphQLRequest(
      query = Some(entitiesQuery(parts.map(part => part.variable -> part.field))),
      operationName = Some(EntityOperationName),
      variables = Some(parts.map(part => part.variable -> part.representations).toMap)
    )

  def partResponse(
    response: GraphQLResponse[CalibanError],
    part: CallPart,
    aliases: Set[String]
  ): GraphQLResponse[CalibanError] =
    response.copy(errors = response.errors.filter {
      case error: CalibanError.ExecutionError =>
        error.path match {
          case PathValue.Key(alias) :: _ if aliases(alias) => alias == part.alias
          case _                                           => true
        }
      case _                                  => true
    })

  sealed trait Variant { def projection: ResponseProjection }

  final case class FederationVariant(projection: ResponseProjection, selections: String) extends Variant {
    lazy val query: String =
      entitiesQuery(List(RepresentationsVariable -> entitiesField(RepresentationsVariable, selections)))
  }

  final case class SingleVariant(
    projection: ResponseProjection,
    field: String,
    arguments: List[(String, Lookup.Argument[ComposedGraph.KeyArgument])],
    selections: List[Selection]
  ) extends Variant

  final case class ByKeyVariant(
    projection: ResponseProjection,
    field: String,
    arguments: List[(String, Lookup.Argument[Lookup.Argument[ComposedGraph.KeyArgument]])],
    keys: List[RequiredSelection],
    selections: List[Selection]
  ) extends Variant

  final case class CallPart(slot: Int, selections: String, representations: InputValue) {
    val alias: String    = s"${CallPart.AliasPrefix}$slot"
    val variable: String = s"${CallPart.VariablePrefix}$slot"
    def field: String    = s"$alias:${entitiesField(variable, selections)}"
  }

  object CallPart {
    private final val AliasPrefix    = "_caliban_gateway_entities_"
    private final val VariablePrefix = "_caliban_gateway_representations_"
  }

  private def entitiesQuery(fields: List[(String, String)]): String =
    fields.map { case (variable, _) => s"$$$variable:[$AnyType!]!" }
      .mkString(s"query $EntityOperationName(", ",", ")") +
      fields.map(_._2).mkString("{", " ", "}")

  private def entitiesField(variable: String, selections: String): String =
    s"$EntitiesField($RepresentationsArgument:$$$variable)$selections"

  private final val EntityOperationName     = "__GatewayEntity"
  private final val LookupOperationName     = "__GatewayLookup"
  private final val RepresentationsVariable = "representations"
  private final val LookupAlias             = "_caliban_gateway_lookup"
  private final val LookupKeyAliasBase      = "_caliban_gateway_lookup_key"

  private sealed trait ResponseShape {
    def values(data: ResponseValue): List[(Int, ResponseValue)]
    def errorIndex(path: List[PathValue]): Option[(Int, List[PathValue])]
  }

  private object ResponseShape {
    sealed abstract class ListRoot(root: String) extends ResponseShape {
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
          case PathValue.Key(`root`) :: PathValue.Index(index) :: tail => Some(index -> tail)
          case _                                                       => None
        }
    }

    final case class Ordered(root: String) extends ListRoot(root)

    final case class Keyed(root: String, keys: List[RequiredSelection]) extends ListRoot(root)

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
