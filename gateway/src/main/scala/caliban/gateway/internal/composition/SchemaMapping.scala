package caliban.gateway.internal.composition

import caliban.execution.Field
import caliban.gateway.{ Lookup, OperationRootNames, SchemaTransformation }
import caliban.gateway.SchemaTransformation._
import caliban.{ CalibanError, GraphQLResponse, InputValue, ResponseValue }
import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.ResponseValue.{ ListValue => ResponseListValue, ObjectValue => ResponseObjectValue }
import caliban.gateway.internal.planning.OperationPlan.RequiredSelection
import caliban.introspection.adt.{ __Field, __InputValue, __Type, __TypeKind }
import caliban.parsing.adt.{ Definition, Directive, Document, Selection, Type }
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension._
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.Parser
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType
import caliban.Value.{ EnumValue, NullValue, StringValue }

private[gateway] final class SchemaMapping private (
  private[internal] val originalRootType: RootType,
  mappings: SchemaMapping.Mappings
) {
  import SchemaMapping._

  private val typeNames                = mappings.typeNames
  private val fieldNames               = mappings.fieldNames
  private val argumentNames            = mappings.argumentNames
  val hiddenTypes                      = mappings.hiddenTypes
  val hiddenFields                     = mappings.hiddenFields
  val hiddenArguments                  = mappings.hiddenArguments
  val hiddenInputFields                = mappings.hiddenInputFields
  private[internal] val renamesNothing = mappings.renamesNothing

  private val sourceRootNames         = OperationRootNames(originalRootType)
  private[gateway] lazy val rootNames = sourceRootNames.mapSource(clientType)
  private val sourceQueryName         = sourceRootNames.source("Query").getOrElse("Query")

  private def clientOwners(sourceType: String): List[String] =
    clientType(sourceType) :: sourceRootNames.composedAll(sourceType)

  private val sourceTypes     = typeNames.map(_.swap)
  private val sourceFields    = fieldNames.iterator.map { case ((tpe, field), renamed) =>
    clientOwners(tpe).map(owner => (owner, renamed) -> field)
  }.flatten.toMap
  private val sourceArguments = argumentNames.iterator.map { case ((tpe, field, argument), renamed) =>
    clientOwners(tpe).map(owner => (owner, clientField(tpe, field), renamed) -> argument)
  }.flatten.toMap
  val nonEmpty: Boolean       = mappings.nonEmpty

  def clientType(name: String): String =
    typeNames.getOrElse(name, name)

  def composedType(name: String): String =
    sourceRootNames.composed(name) match {
      case `name`   => clientType(name)
      case composed => composed
    }

  def sourceType(name: String): String =
    sourceTypes.getOrElse(name, name)

  def clientField(typeName: String, field: String): String =
    fieldNames.getOrElse(typeName -> field, field)

  def sourceField(typeName: String, field: String): String =
    sourceFields.getOrElse(typeName -> field, field)

  def clientArgument(typeName: String, field: String, argument: String): String =
    argumentNames.getOrElse((typeName, field, argument), argument)

  def transform(document: Document): Document =
    if (nonEmpty) {
      val (names, provides) = SchemaComposer.fieldSetDirectiveNames(document)
      val fieldSets         = FieldSetDirectives(names, provides)
      Document(document.definitions.map(transformDefinition(_, fieldSets)), document.sourceMapper)
    } else document

  def transform(lookup: Lookup): Lookup =
    if (nonEmpty) transformLookup(lookup) else lookup

  private def transformLookup(lookup: Lookup): Lookup = {
    def argument(value: Lookup.Argument): Lookup.Argument = value match {
      case Lookup.Argument.Key(field)            => Lookup.Argument.key(clientField(lookup.typeName, field))
      case Lookup.Argument.ObjectMapping(fields) =>
        Lookup.Argument.obj(fields.map { case (name, nested) => name -> argument(nested) }: _*)
      case Lookup.Argument.Batch(value)          => Lookup.Argument.batch(argument(value))
    }
    val arguments                                         = lookup.arguments.map { case (name, value) =>
      clientArgument(sourceQueryName, lookup.field, name) -> argument(value)
    }
    val typeName                                          = clientType(lookup.typeName)
    val field                                             = clientField(sourceQueryName, lookup.field)
    val keys                                              = lookup.keyFields.map(clientField(lookup.typeName, _))

    lookup match {
      case _: Lookup.Single         => Lookup.single(typeName, keys, field, arguments: _*)
      case value: Lookup.ListLookup =>
        val correlation = value.correlation.iterator.map { case (response, key) =>
          clientField(lookup.typeName, response) -> clientField(lookup.typeName, key)
        }.toMap
        Lookup.list(typeName, keys, field, correlation, arguments: _*)
    }
  }

  def rootFieldToSource(field: Field): Field = {
    val clientParent       = field.parentType.flatMap(_.innerType.name).getOrElse("")
    val sourceParent       = sourceRootNames.source(clientParent).getOrElse(sourceType(clientParent))
    val sourceName         = sourceField(clientParent, field.name)
    val composedDirectives = field.parentType
      .flatMap(parent => Option(parent.innerType.getFieldOrNull(field.name)))
      .flatMap(_.directives)
      .getOrElse(Nil)
    val arguments          = field.arguments.map { case (name, value) =>
      sourceArguments.getOrElse((clientParent, field.name, name), name) -> value
    }
    val alias              =
      if (field.alias.isEmpty && sourceName != field.name) Some(field.name)
      else field.alias

    field.copy(
      name = sourceName,
      alias = alias,
      parentType = originalRootType.types.get(sourceParent),
      fields = field.fields.map(rootFieldToSource),
      targets = field.targets.map(_.map(sourceType)),
      arguments = arguments,
      directives =
        if (composedDirectives.isEmpty) field.directives
        else field.directives.filterNot(composedDirectives.contains)
    )
  }

  def lookupFieldToSource(field: String): String =
    sourceField(sourceQueryName, field)

  def lookupArgumentsToSource(field: String, arguments: Map[String, InputValue]): Map[String, InputValue] =
    arguments.map { case (name, value) =>
      sourceArguments.getOrElse((sourceQueryName, field, name), name) -> value
    }

  def representationToSource(typeName: String, value: InputObjectValue): InputObjectValue =
    if (renamesNothing) value
    else mapRepresentation(typeName, value)

  private def mapRepresentation(typeName: String, value: InputObjectValue): InputObjectValue =
    InputObjectValue(value.fields.map {
      case ("__typename", StringValue(name)) => "__typename" -> StringValue(sourceType(name))
      case (name, nested)                    =>
        val fieldName                                             = sourceField(typeName, name)
        def translate(tpe: __Type, input: InputValue): InputValue = tpe.kind match {
          case __TypeKind.NON_NULL                      => tpe.ofType.fold(input)(translate(_, input))
          case __TypeKind.LIST                          =>
            input match {
              case InputListValue(values) =>
                InputListValue(values.map(value => tpe.ofType.fold(value)(translate(_, value))))
              case other                  => other
            }
          case __TypeKind.OBJECT | __TypeKind.INTERFACE =>
            input match {
              case obj: InputObjectValue => mapRepresentation(clientType(tpe.name.getOrElse("")), obj)
              case other                 => other
            }
          case _                                        => input
        }
        fieldName -> sourceFieldDefinition(sourceType(typeName), fieldName).fold(nested)(field =>
          translate(field._type, nested)
        )
    })

  private def transformDefinition(definition: Definition, fieldSets: FieldSetDirectives): Definition =
    definition match {
      case value: SchemaDefinition          =>
        value.copy(
          directives = transformDirectives(value.directives, Nil, fieldSets),
          query = value.query.map(clientType),
          mutation = value.mutation.map(clientType),
          subscription = value.subscription.map(clientType)
        )
      case value: DirectiveDefinition       =>
        value.copy(args = value.args.map(transformInputValueDefinition(_, fieldSets)))
      case value: ObjectTypeDefinition      =>
        value.copy(
          name = clientType(value.name),
          implements = value.implements.map(transformNamedType),
          directives = transformDirectives(value.directives, value.name :: Nil, fieldSets),
          fields = value.fields.map(transformFieldDefinition(value.name, _, fieldSets))
        )
      case value: InterfaceTypeDefinition   =>
        value.copy(
          name = clientType(value.name),
          implements = value.implements.map(transformNamedType),
          directives = transformDirectives(value.directives, value.name :: Nil, fieldSets),
          fields = value.fields.map(transformFieldDefinition(value.name, _, fieldSets))
        )
      case value: InputObjectTypeDefinition =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, fieldSets),
          fields = value.fields.map(transformInputValueDefinition(_, fieldSets))
        )
      case value: EnumTypeDefinition        =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, fieldSets),
          enumValuesDefinition = transformEnumValues(value.enumValuesDefinition, fieldSets)
        )
      case value: UnionTypeDefinition       =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, fieldSets),
          memberTypes = value.memberTypes.map(clientType)
        )
      case value: ScalarTypeDefinition      =>
        value.copy(name = clientType(value.name), directives = transformDirectives(value.directives, Nil, fieldSets))
      case value: SchemaExtension           =>
        value.copy(
          directives = transformDirectives(value.directives, Nil, fieldSets),
          query = value.query.map(clientType),
          mutation = value.mutation.map(clientType),
          subscription = value.subscription.map(clientType)
        )
      case value: ObjectTypeExtension       =>
        value.copy(
          name = clientType(value.name),
          implements = value.implements.map(transformNamedType),
          directives = transformDirectives(value.directives, value.name :: Nil, fieldSets),
          fields = value.fields.map(transformFieldDefinition(value.name, _, fieldSets))
        )
      case value: InterfaceTypeExtension    =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, value.name :: Nil, fieldSets),
          fields = value.fields.map(transformFieldDefinition(value.name, _, fieldSets))
        )
      case value: InputObjectTypeExtension  =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, fieldSets),
          fields = value.fields.map(transformInputValueDefinition(_, fieldSets))
        )
      case value: EnumTypeExtension         =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, fieldSets),
          enumValuesDefinition = transformEnumValues(value.enumValuesDefinition, fieldSets)
        )
      case value: UnionTypeExtension        =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, fieldSets),
          memberTypes = value.memberTypes.map(clientType)
        )
      case value: ScalarTypeExtension       =>
        value.copy(name = clientType(value.name), directives = transformDirectives(value.directives, Nil, fieldSets))
      case other                            => other
    }

  private def transformEnumValues(
    values: List[EnumValueDefinition],
    fieldSets: FieldSetDirectives
  ): List[EnumValueDefinition] =
    values.map(value =>
      value.copy(
        directives = transformDirectives(value.directives, Nil, fieldSets)
      )
    )

  private def transformFieldDefinition(
    typeName: String,
    field: FieldDefinition,
    fieldSets: FieldSetDirectives
  ): FieldDefinition = {
    val sourceDefinition = sourceFieldDefinition(typeName, field.name)
    val outputType       = sourceDefinition.flatMap(_._type.innerType.name).toList
    field.copy(
      name = clientField(typeName, field.name),
      args = field.args.map { argument =>
        transformInputValueDefinition(argument, fieldSets)
          .copy(name = clientArgument(typeName, field.name, argument.name))
      },
      ofType = transformType(field.ofType),
      directives = transformDirectives(field.directives, typeName :: outputType, fieldSets)
    )
  }

  private def transformDirectives(
    directives: List[Directive],
    candidateTypes: List[String],
    fieldSets: FieldSetDirectives
  ): List[Directive] =
    directives.map { directive =>
      if (!fieldSets.names.contains(directive.name)) directive
      else
        directive.arguments.get("fields") match {
          case Some(StringValue(value)) =>
            SchemaComposer
              .parseFieldSet(value)
              .flatMap(selections =>
                fieldSetStart(directive.name, candidateTypes, selections, fieldSets.provides).map(_ -> selections)
              )
              .fold(directive) { case (startType, selections) =>
                directive.copy(arguments =
                  directive.arguments.updated(
                    "fields",
                    StringValue(
                      renderFieldSet(
                        selections.map(transformFieldSetSelection(startType, _))
                      )
                    )
                  )
                )
              }
          case _                        => directive
        }
    }

  private def fieldSetStart(
    directive: String,
    candidates: List[String],
    selections: List[Selection],
    provides: Set[String]
  ): Option[String] = {
    val ordered =
      if (provides.contains(directive)) candidates.reverse
      else candidates
    ordered.find { candidate =>
      val fields = originalRootType.types.get(candidate).toList.flatMap(_.allFields).map(_.name).toSet
      selections.forall {
        case field: Selection.Field => fields.contains(field.name)
        case _                      => true
      }
    }
  }

  private def transformFieldSetSelection(parentType: String, selection: Selection): Selection =
    selection match {
      case field: Selection.Field             =>
        val definition = sourceFieldDefinition(parentType, field.name)
        val arguments  = field.arguments.map { case (name, value) =>
          clientArgument(parentType, field.name, name) -> value
        }
        val childType  = definition.flatMap(_._type.innerType.name).getOrElse("")
        field.copy(
          name = clientField(parentType, field.name),
          arguments = arguments,
          selectionSet = field.selectionSet.map(transformFieldSetSelection(childType, _))
        )
      case fragment: Selection.InlineFragment =>
        val nextType = fragment.typeCondition.map(_.name).getOrElse(parentType)
        fragment.copy(
          typeCondition = fragment.typeCondition.map(transformNamedType),
          selectionSet = fragment.selectionSet.map(transformFieldSetSelection(nextType, _))
        )
      case other                              => other
    }

  private def renderFieldSet(selections: List[Selection]): String =
    DocumentRenderer.selectionsRenderer.renderCompact(selections).stripPrefix("{").stripSuffix("}")

  private def transformInputValueDefinition(
    value: InputValueDefinition,
    fieldSets: FieldSetDirectives
  ): InputValueDefinition =
    value.copy(
      ofType = transformType(value.ofType),
      directives = transformDirectives(value.directives, Nil, fieldSets)
    )

  private def transformType(tpe: Type): Type =
    tpe match {
      case NamedType(name, nonNull)  => NamedType(clientType(name), nonNull)
      case ListType(ofType, nonNull) => ListType(transformType(ofType), nonNull)
    }

  private def transformNamedType(tpe: NamedType): NamedType =
    tpe.copy(name = clientType(tpe.name))

  private[internal] def sourceFieldDefinition(typeName: String, field: String): Option[__Field] =
    originalRootType.types.get(typeName).flatMap(tpe => Option(tpe.getFieldOrNull(field)))

  private[internal] def requiredSelectionToSource(
    parentType: String,
    selection: RequiredSelection
  ): RequiredSelection = {
    val sourceParent = sourceType(parentType)
    val sourceName   = sourceField(parentType, selection.field)
    val childType    = sourceFieldDefinition(sourceParent, sourceName).flatMap(_._type.innerType.name).getOrElse("")
    RequiredSelection(
      sourceName,
      selection.responseName,
      selection.children.map(requiredSelectionToSource(clientType(childType), _))
    )
  }

  private[internal] def rootResponseMapper(
    fields: List[Field]
  ): GraphQLResponse[CalibanError] => GraphQLResponse[CalibanError] =
    if (renamesNothing) identity
    else {
      val mapData = objectResponseMapper(fields)
      response => response.copy(data = mapData(response.data))
    }

  private[internal] def entityFieldsResponseMapper(fields: List[Field]): ResponseValue => ResponseValue =
    if (renamesNothing) identityResponse else objectResponseMapper(fields)

  private val identityResponse: ResponseValue => ResponseValue = identity

  private val typenameResponseMapper: ResponseValue => ResponseValue = {
    case StringValue(name) => StringValue(clientType(name))
    case other             => other
  }

  private def mapSelectedObject(
    selected: java.util.HashMap[String, ResponseValue => ResponseValue],
    values: List[(String, ResponseValue)]
  ): ResponseObjectValue =
    ResponseObjectValue(values.map { case (name, nested) =>
      val mapper = selected.get(name)
      name -> (if (mapper eq null) nested else mapper(nested))
    })

  private def recursiveSelectedResponseMapper(
    selected: java.util.HashMap[String, ResponseValue => ResponseValue]
  ): ResponseValue => ResponseValue = {
    def map(value: ResponseValue): ResponseValue =
      value match {
        case ResponseObjectValue(values) => mapSelectedObject(selected, values)
        case ResponseListValue(values)   => ResponseListValue(values.map(map))
        case other                       => other
      }

    map
  }

  private def objectResponseMapper(fields: List[Field]): ResponseValue => ResponseValue = {
    val selected  = new java.util.HashMap[String, ResponseValue => ResponseValue]
    var remaining = fields
    while (remaining ne Nil) {
      val field = remaining.head
      addResponseMapper(selected, field.aliasedName, fieldResponseMapper(field))
      remaining = remaining.tail
    }
    recursiveSelectedResponseMapper(selected)
  }

  private def fieldResponseMapper(field: Field): ResponseValue => ResponseValue =
    if (field.name == "__typename") typenameResponseMapper
    else if (field.fields.nonEmpty) objectResponseMapper(field.fields)
    else identityResponse

  private[internal] def requiredResponseMapper(
    typeName: String,
    selections: List[RequiredSelection]
  ): ResponseValue => ResponseValue =
    if (renamesNothing || selections.isEmpty) identityResponse
    else {
      val selected  = new java.util.HashMap[String, ResponseValue => ResponseValue]
      var remaining = selections
      while (remaining ne Nil) {
        val selection = remaining.head
        val mapper    =
          if (selection.field == "__typename") typenameResponseMapper
          else {
            val sourceName = sourceField(typeName, selection.field)
            if (selection.children.isEmpty) identityResponse
            else {
              val childName = sourceFieldDefinition(sourceType(typeName), sourceName)
                .flatMap(_._type.innerType.name)
                .map(clientType)
                .getOrElse("")
              requiredResponseMapper(childName, selection.children)
            }
          }
        addResponseMapper(selected, selection.responseName, mapper)
        remaining = remaining.tail
      }
      recursiveSelectedResponseMapper(selected)
    }

  private def addResponseMapper(
    selected: java.util.HashMap[String, ResponseValue => ResponseValue],
    name: String,
    mapper: ResponseValue => ResponseValue
  ): Unit = {
    val existing = selected.get(name)
    selected.put(name, if (existing eq null) mapper else mapper.compose(existing))
  }

}

private[gateway] object SchemaMapping {

  private final case class FieldSetDirectives(names: Set[String], provides: Set[String])

  private final case class CoordinateContext(
    types: Map[String, __Type],
    operationRoots: Set[String],
    transportTypes: Set[String]
  ) {
    def isFederation: Boolean = transportTypes.nonEmpty
  }

  private sealed trait Coordinate {
    def id: String
    def display: String
    def targetScope: String
    def plural: String
    final def exists(context: CoordinateContext): Boolean = targetExists(context, currentName)
    def targetExists(context: CoordinateContext, target: String): Boolean

    def restrictions(change: Change, context: CoordinateContext, prefix: String): List[String]

    final def collision(context: CoordinateContext, target: String, prefix: String): Option[String] =
      if (target != currentName && targetExists(context, target))
        Some(s"$prefix $display is transformed to existing ${targetDescription(target)}.")
      else None

    def currentName: String
    def targetDescription(target: String): String
  }

  private final case class TypeCoordinate(name: String) extends Coordinate {
    val id          = s"type:$name"
    val display     = s"Type '$name'"
    val targetScope = "type"
    val plural      = "Types"
    val currentName = name

    def targetExists(context: CoordinateContext, target: String): Boolean = context.types.contains(target)
    def targetDescription(target: String): String                         = s"type '$target'"

    def restrictions(change: Change, context: CoordinateContext, prefix: String): List[String] = {
      val operation =
        if (!context.operationRoots.contains(name)) Nil
        else
          change.renamed.fold(
            List(s"$prefix Operation root type '$name' cannot be hidden.")
          )(_ => List(s"$prefix Operation root type '$name' cannot be renamed."))
      val transport =
        if (context.transportTypes.contains(name))
          List(s"$prefix Federation transport type '$name' cannot be transformed.")
        else
          change.renamed
            .filter(context.transportTypes)
            .toList
            .map(target =>
              s"$prefix Type '$name' cannot be transformed to reserved Federation transport type '$target'."
            )
      operation ::: transport
    }
  }

  private final case class FieldCoordinate(typeName: String, name: String) extends Coordinate {
    val id          = s"field:$typeName.$name"
    val display     = s"Field '$typeName.$name'"
    val targetScope = s"field:$typeName"
    val plural      = "Fields"
    val currentName = name

    def targetExists(context: CoordinateContext, target: String): Boolean =
      context.types.get(typeName).exists(_.allFields.exists(_.name == target))
    def targetDescription(target: String): String                         = s"field '$target'"

    def restrictions(change: Change, context: CoordinateContext, prefix: String): List[String] = {
      val rootTransport = context.isFederation && context.operationRoots.contains(typeName)
      if (context.transportTypes.contains(typeName) || rootTransport && federationRootFields.contains(name))
        List(s"$prefix Federation transport field '$typeName.$name' cannot be transformed.")
      else
        change.renamed
          .filter(target => rootTransport && federationRootFields.contains(target))
          .toList
          .map(target =>
            s"$prefix Field '$typeName.$name' cannot be transformed to reserved Federation transport field '$target'."
          )
    }
  }

  private final case class ArgumentCoordinate(typeName: String, field: String, name: String) extends Coordinate {
    val id          = s"argument:$typeName.$field.$name"
    val display     = s"Argument '$typeName.$field($name:)'"
    val targetScope = s"argument:$typeName.$field"
    val plural      = "Arguments"
    val currentName = name

    private def definition(context: CoordinateContext, target: String = name): Option[__InputValue] =
      context.types
        .get(typeName)
        .flatMap(tpe => Option(tpe.getFieldOrNull(field)))
        .flatMap(_.allArgs.find(_.name == target))

    def targetExists(context: CoordinateContext, target: String): Boolean =
      definition(context, target).nonEmpty
    def targetDescription(target: String): String                         = s"argument '$target'"

    def restrictions(change: Change, context: CoordinateContext, prefix: String): List[String] = {
      val required  =
        if (
          change.renamed.isEmpty && definition(context)
            .exists(value => !value._type.isNullable && value.defaultValue.isEmpty)
        )
          List(s"$prefix Required argument '$typeName.$field($name:)' cannot be hidden.")
        else Nil
      val transport =
        if (
          context.transportTypes.contains(typeName) ||
          context.isFederation && context.operationRoots.contains(typeName) && federationRootFields.contains(field)
        ) List(s"$prefix Federation transport argument '$typeName.$field($name:)' cannot be transformed.")
        else Nil
      required ::: transport
    }
  }

  private final case class InputFieldCoordinate(typeName: String, name: String) extends Coordinate {
    val id          = s"input:$typeName.$name"
    val display     = s"Input field '$typeName.$name'"
    val targetScope = s"input:$typeName"
    val plural      = "Input fields"
    val currentName = name

    private def definition(context: CoordinateContext): Option[__InputValue] =
      context.types.get(typeName).flatMap(_.allInputFields.find(_.name == name))

    def targetExists(context: CoordinateContext, target: String): Boolean =
      context.types.get(typeName).exists(_.allInputFields.exists(_.name == target))
    def targetDescription(target: String): String                         = s"field '$target'"

    def restrictions(change: Change, context: CoordinateContext, prefix: String): List[String] = {
      val required  =
        if (
          change.renamed.isEmpty && definition(context)
            .exists(value => !value._type.isNullable && value.defaultValue.isEmpty)
        )
          List(s"$prefix Required input field '$typeName.$name' cannot be hidden.")
        else Nil
      val transport =
        if (context.transportTypes.contains(typeName))
          List(s"$prefix Federation transport input field '$typeName.$name' cannot be transformed.")
        else Nil
      required ::: transport
    }
  }

  private final case class Change(coordinate: Coordinate, renamed: Option[String])

  private val federationRootFields = Set("_entities", "_service")

  private final case class Mappings(
    typeNames: Map[String, String] = Map.empty,
    fieldNames: Map[(String, String), String] = Map.empty,
    argumentNames: Map[(String, String, String), String] = Map.empty,
    hiddenTypeSources: Set[String] = Set.empty,
    hiddenFieldSources: Set[(String, String)] = Set.empty,
    hiddenArgumentSources: Set[(String, String, String)] = Set.empty,
    hiddenInputFieldSources: Set[(String, String)] = Set.empty
  ) {
    def renamesNothing: Boolean =
      typeNames.isEmpty && fieldNames.isEmpty && argumentNames.isEmpty

    def nonEmpty: Boolean =
      !renamesNothing || hiddenTypeSources.nonEmpty || hiddenFieldSources.nonEmpty || hiddenArgumentSources.nonEmpty ||
        hiddenInputFieldSources.nonEmpty

    def add(change: Change): Mappings =
      (change.coordinate, change.renamed) match {
        case (TypeCoordinate(name), Some(renamed))                 => copy(typeNames = typeNames.updated(name, renamed))
        case (TypeCoordinate(name), None)                          => copy(hiddenTypeSources = hiddenTypeSources + name)
        case (FieldCoordinate(tpe, name), Some(renamed))           =>
          copy(fieldNames = fieldNames.updated(tpe -> name, renamed))
        case (FieldCoordinate(tpe, name), None)                    =>
          copy(hiddenFieldSources = hiddenFieldSources + (tpe -> name))
        case (ArgumentCoordinate(tpe, field, name), Some(renamed)) =>
          copy(argumentNames = argumentNames.updated((tpe, field, name), renamed))
        case (ArgumentCoordinate(tpe, field, name), None)          =>
          copy(hiddenArgumentSources = hiddenArgumentSources + ((tpe, field, name)))
        case (InputFieldCoordinate(tpe, name), _)                  =>
          copy(hiddenInputFieldSources = hiddenInputFieldSources + (tpe -> name))
      }

    def renamedType(name: String): String               = typeNames.getOrElse(name, name)
    def renamedField(tpe: String, name: String): String = fieldNames.getOrElse(tpe -> name, name)

    def hiddenTypes: Set[String]                       = hiddenTypeSources.map(renamedType)
    def hiddenFields: Set[(String, String)]            = hiddenFieldSources.map { case (tpe, name) =>
      renamedType(tpe) -> renamedField(tpe, name)
    }
    def hiddenArguments: Set[(String, String, String)] = hiddenArgumentSources.map { case (tpe, field, name) =>
      (renamedType(tpe), renamedField(tpe, field), argumentNames.getOrElse((tpe, field, name), name))
    }
    def hiddenInputFields: Set[(String, String)]       = hiddenInputFieldSources.map { case (tpe, name) =>
      renamedType(tpe) -> name
    }

  }

  private def normalize(transformation: SchemaTransformation): Change =
    transformation match {
      case RenameType(name, renamed)                 => Change(TypeCoordinate(name), Some(renamed))
      case HideType(name)                            => Change(TypeCoordinate(name), None)
      case RenameField(tpe, name, renamed)           => Change(FieldCoordinate(tpe, name), Some(renamed))
      case HideField(tpe, name)                      => Change(FieldCoordinate(tpe, name), None)
      case RenameArgument(tpe, field, name, renamed) =>
        Change(ArgumentCoordinate(tpe, field, name), Some(renamed))
      case HideArgument(tpe, field, name)            => Change(ArgumentCoordinate(tpe, field, name), None)
      case HideInputField(tpe, name)                 => Change(InputFieldCoordinate(tpe, name), None)
    }

  def compile(
    source: String,
    rootType: RootType,
    document: Document,
    federation: Boolean,
    transformations: List[SchemaTransformation]
  ): Either[List[String], SchemaMapping] = {
    val prefix         = s"[$source]"
    val operationRoots =
      rootType.queryType.name.toSet ++ rootType.mutationType.flatMap(_.name).toSet ++
        rootType.subscriptionType.flatMap(_.name).toSet
    val context        = CoordinateContext(
      rootType.types,
      operationRoots,
      SchemaComposer.federationTransportTypes(document, federation)
    )
    val changes        = transformations.map(normalize)
    val mappings       = changes.foldLeft(Mappings())(_.add(_))
    val renames        = changes.collect { case Change(coordinate, Some(renamed)) => coordinate -> renamed }

    val missing                    = changes.collect {
      case Change(coordinate, _) if !coordinate.exists(context) =>
        s"$prefix ${coordinate.display} does not exist."
    }
    val restrictions               = changes.flatMap(change => change.coordinate.restrictions(change, context, prefix))
    val hiddenReferences           = referencedHiddenCoordinates(
      document,
      rootType,
      mappings.hiddenInputFieldSources
    )
    val hiddenInputFieldReferences = hiddenReferences.toList.sorted.map { case (tpe, field) =>
      s"$prefix Hidden input field '$tpe.$field' is referenced by a directive or default value."
    }
    val invalidRenameTargets       = renames.flatMap { case (coordinate, target) =>
      if (Parser.parseName(target).isLeft)
        List(s"$prefix ${coordinate.display} cannot be transformed to invalid GraphQL name '$target'.")
      else if (target.startsWith("__"))
        List(s"$prefix ${coordinate.display} cannot be transformed to reserved GraphQL name '$target'.")
      else Nil
    }
    val collisions                 = renames.flatMap { case (coordinate, target) => coordinate.collision(context, target, prefix) }
    val transformedCollisions      = renames.groupBy { case (coordinate, target) =>
      coordinate.targetScope -> target
    }.collect {
      case ((_, target), values) if values.map(_._1.id).distinct.size > 1 =>
        val coordinates = values.map(_._1).distinct.sortBy(_.id)
        val references  = coordinates.map(_.display.dropWhile(_ != '\'')).mkString(", ")
        s"$prefix ${coordinates.headOption.map(_.plural).getOrElse("Coordinates")} $references are both transformed to '$target'."
    }.toList
    val conflictingTransformations = changes
      .groupBy(_.coordinate.id)
      .collect {
        case (_, values) if values.map(_.renamed).distinct.size > 1 =>
          s"$prefix Coordinate ${values.headOption.map(_.coordinate.display.dropWhile(_ != '\'')).getOrElse("''")} has conflicting transformations."
      }
      .toList
    val diagnostics                =
      (missing ::: restrictions ::: hiddenInputFieldReferences ::: invalidRenameTargets :::
        collisions ::: transformedCollisions ::: conflictingTransformations).distinct.sorted

    if (diagnostics.nonEmpty) Left(diagnostics)
    else Right(new SchemaMapping(rootType, mappings))
  }

  private[gateway] final case class InputCoordinateReferences(
    inputTypes: Set[String] = Set.empty,
    enumValues: Set[(String, String)] = Set.empty,
    inputFields: Set[(String, String)] = Set.empty
  ) {
    def ++(that: InputCoordinateReferences): InputCoordinateReferences =
      InputCoordinateReferences(
        inputTypes ++ that.inputTypes,
        enumValues ++ that.enumValues,
        inputFields ++ that.inputFields
      )
  }

  private[gateway] def inputCoordinateReferences(tpe: __Type, value: InputValue): InputCoordinateReferences = {
    def loop(expected: __Type, input: InputValue): InputCoordinateReferences =
      expected.kind match {
        case __TypeKind.NON_NULL     => expected.ofType.fold(InputCoordinateReferences())(loop(_, input))
        case __TypeKind.LIST         =>
          expected.ofType.fold(InputCoordinateReferences()) { nested =>
            input match {
              case InputListValue(values) => values.foldLeft(InputCoordinateReferences())(_ ++ loop(nested, _))
              case NullValue              => InputCoordinateReferences()
              case singleton              => loop(nested, singleton)
            }
          }
        case __TypeKind.INPUT_OBJECT =>
          val typeName = expected.name.getOrElse("")
          val own      = InputCoordinateReferences(inputTypes = Set(typeName))
          input match {
            case InputObjectValue(fields) =>
              fields.iterator.foldLeft(own) { case (result, (name, nested)) =>
                val fieldReference  = InputCoordinateReferences(inputFields = Set(typeName -> name))
                val nestedReference = expected.allInputFields
                  .find(_.name == name)
                  .fold(InputCoordinateReferences())(field => loop(field._type, nested))
                result ++ fieldReference ++ nestedReference
              }
            case _                        => own
          }
        case __TypeKind.ENUM         =>
          val typeName  = expected.name.getOrElse("")
          val valueName = input match {
            case EnumValue(name)   => Some(name)
            case StringValue(name) => Some(name)
            case _                 => None
          }
          InputCoordinateReferences(
            inputTypes = Set(typeName),
            enumValues = valueName.map(typeName -> _).toSet
          )
        case _                       =>
          InputCoordinateReferences(inputTypes = expected.name.toSet)
      }

    loop(tpe, value)
  }

  private def referencedHiddenCoordinates(
    document: Document,
    rootType: RootType,
    hiddenInputFields: Set[(String, String)]
  ): Set[(String, String)] = {
    def directiveValues(directives: List[Directive]): List[(__Type, InputValue)] =
      directives.flatMap { directive =>
        rootType.additionalDirectives.find(_.name == directive.name).toList.flatMap { definition =>
          directive.arguments.toList.flatMap { case (name, value) =>
            definition.allArgs.find(_.name == name).map(argument => argument._type -> value)
          }
        }
      }

    def inputValues(value: __InputValue): List[(__Type, InputValue)] =
      directiveValues(value.directives.getOrElse(Nil)) ::: value.parsedDefaultValue.map(value._type -> _).toList

    val schemaDirectives = document.schemaDefinition.toList.flatMap(_.directives)
    val typeValues       = rootType.types.valuesIterator.flatMap { tpe =>
      directiveValues(tpe.directives.getOrElse(Nil)) :::
        tpe.allFields.flatMap(field =>
          directiveValues(field.directives.getOrElse(Nil)) ::: field.allArgs.flatMap(inputValues)
        ) ::: tpe.allInputFields.flatMap(inputValues) :::
        tpe.allEnumValues.flatMap(value => directiveValues(value.directives.getOrElse(Nil)))
    }
    val definitionValues = rootType.additionalDirectives.iterator.flatMap(_.allArgs.flatMap(inputValues))
    val found            = (directiveValues(schemaDirectives).iterator ++ typeValues ++ definitionValues).map {
      case (tpe, value) => inputCoordinateReferences(tpe, value)
    }
      .foldLeft(InputCoordinateReferences())(_ ++ _)

    found.inputFields.filter(hiddenInputFields)
  }
}
