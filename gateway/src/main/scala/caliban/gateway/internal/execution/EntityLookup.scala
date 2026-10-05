package caliban.gateway.internal.execution

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, PathValue, ResponseValue }
import caliban.execution.Field
import caliban.gateway.internal.PrivateAliases
import caliban.gateway.internal.composition.{ ComposedGraph, FieldSelectionMap }
import caliban.gateway.internal.composition.ComposedGraph.Source
import caliban.gateway.internal.composition.DirectiveComposition.ArgumentCoordinate
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

import scala.collection.mutable

/**
 * Builds entity lookup requests together with the rules for translating and correlating their responses.
 * Generated aliases and correlation selections stay private to each call.
 */
private[internal] object EntityLookup {
  def prepare(batch: EntityBatch): Option[(GraphQLRequest, Call)] =
    batch.variant match {
      case variant: FederationVariant                                    =>
        val request = GraphQLRequest(
          query = Some(variant.query),
          operationName = Some(EntityOperationName),
          variables = Some(Map(RepresentationsVariable -> representations(batch)))
        )
        Some(request -> new Call(batch, variant.projection, ResponseShape.Ordered(EntitiesField)))
      case SingleVariant(projection, path, field, arguments, selections) =>
        val aliases  = Vector.tabulate(batch.entries.size)(index => s"${LookupAlias}_$index")
        val selected = entrySelections(batch, selections)
        // An entity whose key does not map to the arguments, as under a type condition it fails, has no result.
        val fields   = batch.entries.iterator
          .zip(aliases.iterator)
          .flatMap { case (entry, alias) =>
            argumentValues(arguments)(keyValue(entry.representation.identity.keys))
              .map(lookupField(field, Some(alias), _, selected(entry)))
          }
          .toList
        Some(fields).filter(_.nonEmpty).map { fields =>
          val nested =
            path.foldRight[List[Selection]](fields)((name, inner) => lookupField(name, None, Map.empty, inner) :: Nil)
          lookupRequest(nested) -> new Call(batch, projection, ResponseShape.Aliases(path, aliases))
        }
      case ByKeyVariant(projection, field, arguments, keys, selections)  =>
        val values = argumentValues(arguments) { key =>
          traverseOption(batch.entries)(entry => argumentValue(key)(keyValue(entry.representation.identity.keys)))
            .map(InputListValue(_))
        }
        values.map { values =>
          lookupRequest(List(lookupField(field, Some(LookupAlias), values, selections))) ->
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
    alias: Option[String],
    arguments: Map[String, InputValue],
    selections: List[Selection]
  ): Selection.Field =
    Selection.Field(alias, field, arguments, Nil, selections, 0)

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

    private lazy val expected = batch.entries.iterator.map(_.representation.identity).zipWithIndex.toMap

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

  /**
   * The lookup for a fetch with `arguments` injected. A field whose `@require` argument has no value is left out.
   */
  def variant(fetch: EntityFetch, arguments: Map[ArgumentCoordinate, InputValue]): Variant = {
    val target     = fetch.target
    val mapping    = target.source.mapping
    val executable = executableFields(fetch, arguments)
    val projection = ResponseProjection.compile(fetch.fields, executable, mapping.typeNames)
    val selections = lookupSelections(fetch, executable, arguments)
    target.lookup.operation match {
      case ComposedGraph.LookupOperation.FederationEntities           =>
        val fragment =
          Selection.InlineFragment(
            Some(NamedType(mapping.sourceType(target.entityType), nonNull = false)),
            Nil,
            selections
          )
        FederationVariant(projection, DocumentRenderer.selectionsRenderer.renderCompact(fragment :: Nil))
      case ComposedGraph.LookupOperation.Single(path, field, args, _) =>
        SingleVariant(projection, path, field, args, selections)
      case ComposedGraph.LookupOperation.ByKey(field, args)           =>
        val aliases = new PrivateAliases(responseNames(executable))
        val keys    = target.keys.map(key => RequiredSelection(key.field, aliases.next(LookupKeyAliasBase)))
        val fields  = keys.map(key => requiredSelection(mapping.requiredSelectionToSource(target.entityType, key)))
        ByKeyVariant(projection, field, args, keys, selections ::: fields)
    }
  }

  private def executableFields(fetch: EntityFetch, arguments: Map[ArgumentCoordinate, InputValue]): List[Field] =
    fetch.target.source.prepareFields(
      fetch.target.entityType,
      injectArguments(fetch.target.source, fetch.fields, arguments)
    )

  private def lookupSelections(
    fetch: EntityFetch,
    executable: List[Field],
    arguments: Map[ArgumentCoordinate, InputValue]
  ): List[Selection] = {
    val target     = fetch.target
    val filled     =
      if (target.requiredArguments.isEmpty) executable
      else executable.filterNot(field => target.requiredArguments.exists(missing(_, field, arguments)))
    val selections = filled.flatMap(field => targetedSelections(target.source.mapping.fieldToSource(field)))
    target.lookup.operation match {
      case ComposedGraph.LookupOperation.Single(_, _, _, Some(name)) =>
        val unwrapped = selections.flatMap {
          case Selection.InlineFragment(Some(NamedType(`name`, _)), Nil, inner) => inner
          case selection                                                        => selection :: Nil
        }
        Selection.InlineFragment(Some(NamedType(name, nonNull = false)), Nil, unwrapped) :: Nil
      case _                                                         => selections
    }
  }

  def missing(argument: RequiredArgument, field: Field, arguments: Map[ArgumentCoordinate, InputValue]): Boolean =
    argument.at.fieldName == field.name && argument.at.typeName == parentTypeName(field) &&
      !arguments.contains(argument.at)

  /**
   * Each entity's selections, with the values of its `@require` arguments injected.
   */
  private def entrySelections(batch: EntityBatch, shared: List[Selection]): EntityBatchEntry => List[Selection] =
    if (batch.fetch.target.requiredArguments.isEmpty) _ => shared
    else {
      val selections = mutable.HashMap.empty[List[(ArgumentCoordinate, InputValue)], List[Selection]]
      entry =>
        selections.getOrElseUpdate(
          entry.representation.arguments, {
            val arguments = batch.arguments ++ entry.representation.arguments
            lookupSelections(batch.fetch, executableFields(batch.fetch, arguments), arguments)
          }
        )
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

  private def injectArguments(
    source: Source,
    fields: List[Field],
    values: Map[ArgumentCoordinate, InputValue]
  ): List[Field] =
    if (values.isEmpty) fields
    else
      fields.map { field =>
        val parent = parentTypeName(field)
        val added  = values.iterator.collect {
          case (at, value) if at.typeName == parent && at.fieldName == field.name =>
            val expected = source.sourceField(parent, field.name).flatMap(_.allArgs.find(_.name == at.argumentName))
            at.argumentName -> expected.flatMap(argument => coerce(value, argument._type)).getOrElse(value)
        }.toMap
        field.copy(arguments = field.arguments ++ added, fields = injectArguments(source, field.fields, values))
      }

  /**
   * Coerces a value read from a response to an input type, where enum values arrive as strings. `None` when the value
   * does not fit the type, as a null for a non-null type or a missing required input field.
   */
  def coerce(value: InputValue, expected: __Type): Option[InputValue] =
    (expected.kind, value) match {
      case (__TypeKind.NON_NULL, NullValue)                    => None
      case (__TypeKind.NON_NULL, _)                            => expected.ofType.fold(Option(value))(coerce(value, _))
      case (_, NullValue)                                      => Some(NullValue)
      case (__TypeKind.LIST, InputListValue(values))           =>
        expected.ofType.fold(Option(value))(element =>
          traverseOption(values)(coerce(_, element)).map(InputListValue(_))
        )
      case (__TypeKind.LIST, single)                           =>
        expected.ofType.fold(Option(value))(element => coerce(single, element).map(item => InputListValue(item :: Nil)))
      case (__TypeKind.ENUM, StringValue(entry))               => Some(EnumValue(entry))
      case (__TypeKind.INPUT_OBJECT, InputObjectValue(fields)) =>
        traverseOption(expected.allInputFields) { definition =>
          fields.get(definition.name) match {
            case Some(field) => coerce(field, definition._type).map(value => Some(definition.name -> value))
            case None        => if (isRequiredInput(definition)) None else Some(None)
          }
        }.map(values => InputObjectValue(values.flatten.toMap))
      case _                                                   => Some(value)
    }

  private def federationRepresentation(typename: Option[String], entry: EntityBatchEntry): InputObjectValue =
    InputObjectValue(
      entry.representation.identity.keys.toMap ++ entry.representation.requirements +
        (TypenameField -> StringValue(typename.getOrElse(entry.representation.identity.typename)))
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

  private def keyValue(keys: List[(String, InputValue)])(key: ComposedGraph.KeyArgument): Option[InputValue] =
    FieldSelectionMap.evaluate(key.value, keys).flatMap(coerce(_, key.expectedType))

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
    path: List[String],
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

    final case class Aliases(path: List[String], aliases: Vector[String]) extends ResponseShape {
      private lazy val indices: Map[String, Int] = aliases.iterator.zipWithIndex.toMap

      def values(data: ResponseValue): List[(Int, ResponseValue)] =
        path.foldLeft[ResponseValue](data) {
          case (obj: ObjectValue, name) => obj.getOrNull(name)
          case (_, _)                   => NullValue
        } match {
          case obj: ObjectValue =>
            val fields = IndexedFields(obj)
            aliases.iterator.zipWithIndex.flatMap { case (alias, index) => fields.get(alias).map(index -> _) }.toList
          case _                => Nil
        }

      def errorIndex(errorPath: List[PathValue]): Option[(Int, List[PathValue])] =
        errorPath.drop(path.size) match {
          case PathValue.Key(alias) :: tail if errorPath.take(path.size) == path.map(PathValue.Key(_)) =>
            indices.get(alias).map(_ -> tail)
          case _                                                                                       => None
        }
    }
  }
}
