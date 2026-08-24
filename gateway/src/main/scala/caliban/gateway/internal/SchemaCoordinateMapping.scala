package caliban.gateway.internal

import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ EnumValue, NullValue, StringValue }
import caliban.execution.Field
import caliban.gateway.{ Lookup, SchemaTransformation }
import caliban.gateway.SchemaTransformation._
import caliban.gateway.internal.OperationPlanner._
import caliban.introspection.adt.{ __Field, __InputValue, __Type, __TypeKind }
import caliban.parsing.Parser
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.parsing.adt.Definition.TypeSystemExtension._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Definition, Directive, Document, Selection, Type }
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType
import caliban.{ CalibanError, GraphQLResponse, InputValue, ResponseValue }

private[gateway] final class SchemaCoordinateMapping private (
  originalRootType: RootType,
  mappings: SchemaCoordinateMapping.CoordinateMappings
) {
  import SchemaCoordinateMapping._

  private val typeNames       = mappings.typeNames
  private val fieldNames      = mappings.fieldNames
  private val argumentNames   = mappings.argumentNames
  private val inputFieldNames = mappings.inputFieldNames
  private val enumValueNames  = mappings.enumValueNames
  val hiddenTypes             = mappings.hiddenTypes
  val hiddenFields            = mappings.hiddenFields
  val hiddenArguments         = mappings.hiddenArguments
  val hiddenInputFields       = mappings.hiddenInputFields
  val hiddenEnumValues        = mappings.hiddenEnumValues
  private val renamesNothing  = mappings.renamesNothing

  private val sourceRootTypes =
    Map("Query" -> originalRootType.queryType) ++
      originalRootType.mutationType.map("Mutation" -> _).toMap ++
      originalRootType.subscriptionType.map("Subscription" -> _).toMap
  private val sourceRootNames = sourceRootTypes.iterator.flatMap { case (operation, tpe) =>
    tpe.name.map(_ -> operation)
  }.toMap
  private val sourceQueryName = originalRootType.queryType.name.getOrElse("Query")
  private val clientQueryName = clientType(sourceQueryName)

  private def clientOwners(sourceType: String): List[String] =
    clientType(sourceType) :: sourceRootNames.get(sourceType).toList

  private val sourceTypes       = typeNames.map(_.swap)
  private val sourceFields      = fieldNames.iterator.map { case ((tpe, field), renamed) =>
    clientOwners(tpe).map(owner => (owner, renamed) -> field)
  }.flatten.toMap
  private val sourceArguments   = argumentNames.iterator.map { case ((tpe, field, argument), renamed) =>
    clientOwners(tpe).map(owner => (owner, clientField(tpe, field), renamed) -> argument)
  }.flatten.toMap
  private val sourceInputFields = inputFieldNames.iterator.map { case ((tpe, field), renamed) =>
    (clientType(tpe), renamed) -> field
  }.toMap
  private val sourceEnumValues  = enumValueNames.iterator.map { case ((tpe, value), renamed) =>
    (clientType(tpe), renamed) -> value
  }.toMap

  val nonEmpty: Boolean = mappings.nonEmpty

  def clientType(name: String): String =
    typeNames.getOrElse(name, name)

  def composedType(name: String): String =
    sourceRootNames.getOrElse(name, clientType(name))

  def sourceType(name: String): String =
    sourceTypes.getOrElse(name, name)

  def clientField(typeName: String, field: String): String =
    fieldNames.getOrElse(typeName -> field, field)

  def sourceField(typeName: String, field: String): String =
    sourceFields.getOrElse(typeName -> field, field)

  def clientArgument(typeName: String, field: String, argument: String): String =
    argumentNames.getOrElse((typeName, field, argument), argument)

  def clientInputField(typeName: String, field: String): String =
    inputFieldNames.getOrElse(typeName -> field, field)

  def clientEnumValue(typeName: String, value: String): String =
    enumValueNames.getOrElse(typeName -> value, value)

  def transform(document: Document): Document =
    if (nonEmpty) {
      val (names, provides) = SchemaComposition.fieldSetDirectiveNames(document)
      val fieldSets         = FieldSetDirectives(names, provides)
      Document(document.definitions.map(transformDefinition(_, fieldSets)), document.sourceMapper)
    } else document

  def transform(lookup: Lookup): Lookup =
    if (nonEmpty) transformLookup(lookup) else lookup

  private def transformLookup(lookup: Lookup): Lookup = {
    def argument(value: Lookup.Argument, expected: Option[__Type]): Lookup.Argument =
      value match {
        case Lookup.Argument.Key(field)            => Lookup.Argument.key(clientField(lookup.typeName, field))
        case Lookup.Argument.ObjectMapping(fields) =>
          val inputType = expected.map(nullable).flatMap(value => value.name.flatMap(originalRootType.types.get))
          Lookup.Argument.obj(fields.map { case (name, nested) =>
            val definition = inputType.flatMap(_.allInputFields.find(_.name == name))
            clientInputField(inputType.flatMap(_.name).getOrElse(""), name) -> argument(
              nested,
              definition.map(_._type)
            )
          }: _*)
        case Lookup.Argument.Batch(value)          =>
          Lookup.Argument.batch(argument(value, expected.flatMap(listElement)))
      }

    val sourceLookup = Option(originalRootType.queryType.getFieldOrNull(lookup.field))
    val arguments    = lookup.arguments.iterator.map { case (name, value) =>
      val definition = sourceLookup.flatMap(_.allArgs.find(_.name == name))
      clientArgument(sourceQueryName, lookup.field, name) -> argument(value, definition.map(_._type))
    }.toList
    val typeName     = clientType(lookup.typeName)
    val field        = clientField(sourceQueryName, lookup.field)
    val keys         = lookup.keyFields.map(clientField(lookup.typeName, _))

    lookup match {
      case _: Lookup.Single         => Lookup.single(typeName, keys, field, arguments: _*)
      case value: Lookup.ListLookup =>
        val correlation = value.correlation match {
          case Lookup.Correlation.Ordered       => Lookup.Correlation.ordered
          case Lookup.Correlation.ByKey(fields) =>
            Lookup.Correlation.byKey(fields.iterator.map { case (response, key) =>
              clientField(lookup.typeName, response) -> clientField(lookup.typeName, key)
            }.toMap)
        }
        Lookup.list(typeName, keys, field, correlation, arguments: _*)
    }
  }

  def rootFieldToSource(field: Field): Field = {
    val clientParent = field.parentType.flatMap(_.innerType.name).getOrElse("")
    val sourceParent = sourceRootTypes.get(clientParent).flatMap(_.name).getOrElse(sourceType(clientParent))
    val sourceName   = sourceField(clientParent, field.name)
    val definition   = sourceFieldDefinition(sourceParent, sourceName)
    val arguments    = translateArguments(
      definition.toList.flatMap(_.allArgs),
      field.arguments,
      name => sourceArguments.getOrElse((clientParent, field.name, name), name),
      ToSource
    )
    val alias        =
      if (field.alias.isEmpty && sourceName != field.name) Some(field.name)
      else field.alias

    field.copy(
      name = sourceName,
      alias = alias,
      fields = field.fields.map(rootFieldToSource),
      targets = field.targets.map(_.map(sourceType)),
      arguments = arguments
    )
  }

  def requiredSelectionToSource(parentType: String, selection: RequiredSelection): RequiredSelection = {
    val sourceParent = sourceType(parentType)
    val sourceName   = sourceField(parentType, selection.field)
    val childType    = sourceFieldDefinition(sourceParent, sourceName).flatMap(_._type.innerType.name).getOrElse("")
    selection.copy(
      field = sourceName,
      children = selection.children.map(requiredSelectionToSource(clientType(childType), _)),
      conditions = selection.conditions.map(_.map(sourceType))
    )
  }

  def sourceLookupField(field: String): String =
    sourceField(clientQueryName, field)

  def sourceLookupArguments(field: String, arguments: Map[String, InputValue]): Map[String, InputValue] = {
    val sourceName = sourceField(clientQueryName, field)
    val definition = sourceFieldDefinition(sourceQueryName, sourceName)
    translateArguments(
      definition.toList.flatMap(_.allArgs),
      arguments,
      name => sourceArguments.getOrElse((clientQueryName, field, name), name),
      ToSource
    )
  }

  def representationToSource(typeName: String, value: InputObjectValue): InputObjectValue = {
    if (renamesNothing) return value
    val sourceName = sourceType(typeName)
    val definition = originalRootType.types.get(sourceName)
    InputObjectValue(value.fields.iterator.map {
      case ("__typename", StringValue(name)) => "__typename" -> StringValue(sourceType(name))
      case (name, nested)                    =>
        val fieldName  = sourceField(typeName, name)
        val translated = definition
          .flatMap(tpe => Option(tpe.getFieldOrNull(fieldName)))
          .fold(nested)(field => mapInputValue(field._type, nested, ToSource))
        fieldName -> translated
    }.toMap)
  }

  private[internal] def rootResponseMapper(
    fields: List[Field]
  ): GraphQLResponse[CalibanError] => GraphQLResponse[CalibanError] =
    if (renamesNothing) identity
    else {
      val mapData = objectResponseMapper(fields)
      response => response.copy(data = mapData(response.data))
    }

  private[internal] def entityFieldsResponseMapper(
    typeName: String,
    fields: List[Field]
  ): ResponseValue => ResponseValue =
    if (renamesNothing) identityResponse
    else
      originalRootType.types
        .get(sourceType(typeName))
        .fold(identityResponse)(responseValueMapper(_, fields))

  private val identityResponse: ResponseValue => ResponseValue = identity

  private val typenameResponseMapper: ResponseValue => ResponseValue = {
    case StringValue(name) => StringValue(clientType(name))
    case other             => other
  }

  private def mapSelectedObject(
    selected: java.util.HashMap[String, ResponseValue => ResponseValue],
    values: List[(String, ResponseValue)]
  ): ObjectValue =
    ObjectValue(values.map { case (name, nested) =>
      val mapper = selected.get(name)
      name -> (if (mapper eq null) nested else mapper(nested))
    })

  private def shallowSelectedResponseMapper(
    selected: java.util.HashMap[String, ResponseValue => ResponseValue]
  ): ResponseValue => ResponseValue = {
    case ObjectValue(values) => mapSelectedObject(selected, values)
    case other               => other
  }

  private def recursiveSelectedResponseMapper(
    selected: java.util.HashMap[String, ResponseValue => ResponseValue]
  ): ResponseValue => ResponseValue = {
    def map(value: ResponseValue): ResponseValue =
      value match {
        case ObjectValue(values) => mapSelectedObject(selected, values)
        case ListValue(values)   => ListValue(values.map(map))
        case other               => other
      }

    map
  }

  private def objectResponseMapper(fields: List[Field]): ResponseValue => ResponseValue = {
    val selected  = new java.util.HashMap[String, ResponseValue => ResponseValue]
    var remaining = fields
    while (remaining ne Nil) {
      val field = remaining.head
      selected.put(field.aliasedName, fieldResponseMapper(field))
      remaining = remaining.tail
    }
    shallowSelectedResponseMapper(selected)
  }

  private def fieldResponseMapper(field: Field): ResponseValue => ResponseValue =
    if (field.name == "__typename")
      typenameResponseMapper
    else responseValueMapper(field.fieldType, field.fields)

  private def responseValueMapper(tpe: __Type, fields: List[Field]): ResponseValue => ResponseValue =
    tpe.kind match {
      case __TypeKind.NON_NULL                                         =>
        tpe.ofType.fold(identityResponse)(responseValueMapper(_, fields))
      case __TypeKind.LIST                                             =>
        val mapItem = tpe.ofType.fold(identityResponse)(responseValueMapper(_, fields))
        value =>
          value match {
            case ListValue(values) => ListValue(values.map(mapItem))
            case other             => other
          }
      case __TypeKind.ENUM                                             =>
        val typeName = tpe.name.getOrElse("")
        value =>
          value match {
            case StringValue(name) => StringValue(clientEnumValue(sourceType(typeName), name))
            case EnumValue(name)   => EnumValue(clientEnumValue(sourceType(typeName), name))
            case other             => other
          }
      case __TypeKind.OBJECT | __TypeKind.INTERFACE | __TypeKind.UNION => objectResponseMapper(fields)
      case _                                                           => identityResponse
    }

  private[internal] def requiredResponseMapper(
    typeName: String,
    selections: List[RequiredSelection]
  ): ResponseValue => ResponseValue =
    if (selections.isEmpty) identityResponse
    else {
      val selected  = new java.util.HashMap[String, ResponseValue => ResponseValue]
      val source    = originalRootType.types.get(sourceType(typeName))
      var remaining = selections
      while (remaining ne Nil) {
        val selection = remaining.head
        val mapper    =
          if (selection.field == "__typename")
            typenameResponseMapper
          else {
            val sourceName = sourceField(typeName, selection.field)
            val childType  = source.flatMap(tpe => Option(tpe.getFieldOrNull(sourceName))).map(_._type)
            if (selection.children.isEmpty) childType.fold(identityResponse)(responseValueMapper(_, Nil))
            else {
              val childName = childType.flatMap(_.innerType.name).map(clientType).getOrElse("")
              requiredResponseMapper(childName, selection.children)
            }
          }
        selected.put(selection.responseName, mapper)
        remaining = remaining.tail
      }

      recursiveSelectedResponseMapper(selected)
    }

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
        val source = originalRootType.additionalDirectives.find(_.name == value.name)
        value.copy(args = value.args.map { argument =>
          transformInputValueDefinition(
            "",
            argument,
            fieldSets,
            source.flatMap(_.allArgs.find(_.name == argument.name)).map(_._type)
          )
        })
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
          fields = value.fields.map(transformInputValueDefinition(value.name, _, fieldSets))
        )
      case value: EnumTypeDefinition        =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, fieldSets),
          enumValuesDefinition = transformEnumValues(value.name, value.enumValuesDefinition, fieldSets)
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
          fields = value.fields.map(transformInputValueDefinition(value.name, _, fieldSets))
        )
      case value: EnumTypeExtension         =>
        value.copy(
          name = clientType(value.name),
          directives = transformDirectives(value.directives, Nil, fieldSets),
          enumValuesDefinition = transformEnumValues(value.name, value.enumValuesDefinition, fieldSets)
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
    typeName: String,
    values: List[EnumValueDefinition],
    fieldSets: FieldSetDirectives
  ): List[EnumValueDefinition] =
    values.map(value =>
      value.copy(
        enumValue = clientEnumValue(typeName, value.enumValue),
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
        transformInputValueDefinition(
          "",
          argument,
          fieldSets,
          sourceDefinition.flatMap(_.allArgs.find(_.name == argument.name)).map(_._type)
        ).copy(name = clientArgument(typeName, field.name, argument.name))
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
      val definitions = originalRootType.additionalDirectives.find(_.name == directive.name).toList.flatMap(_.allArgs)
      val arguments   = translateArguments(definitions, directive.arguments, identity, ToClient)
      val transformed = directive.copy(arguments = arguments)

      if (!fieldSets.names.contains(directive.name)) transformed
      else
        transformed.arguments.get("fields") match {
          case Some(StringValue(value)) =>
            SchemaComposition
              .parseFieldSet(value)
              .flatMap(selections =>
                fieldSetStart(directive.name, candidateTypes, selections, fieldSets.provides).map(_ -> selections)
              )
              .fold(transformed) { case (startType, selections) =>
                transformed.copy(arguments =
                  transformed.arguments.updated(
                    "fields",
                    StringValue(
                      renderFieldSet(
                        selections.map(transformFieldSetSelection(startType, _))
                      )
                    )
                  )
                )
              }
          case _                        => transformed
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
        val arguments  = translateArguments(
          definition.toList.flatMap(_.allArgs),
          field.arguments,
          clientArgument(parentType, field.name, _),
          ToClient
        )
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
    typeName: String,
    value: InputValueDefinition,
    fieldSets: FieldSetDirectives,
    knownSourceType: Option[__Type] = None
  ): InputValueDefinition = {
    val sourceType = knownSourceType.orElse(
      originalRootType.types.get(typeName).flatMap(_.allInputFields.find(_.name == value.name)).map(_._type)
    )
    value.copy(
      name = clientInputField(typeName, value.name),
      ofType = transformType(value.ofType),
      defaultValue = value.defaultValue.map(input => sourceType.fold(input)(inputToClient(_, input))),
      directives = transformDirectives(value.directives, Nil, fieldSets)
    )
  }

  private def transformType(tpe: Type): Type =
    tpe match {
      case NamedType(name, nonNull)  => NamedType(clientType(name), nonNull)
      case ListType(ofType, nonNull) => ListType(transformType(ofType), nonNull)
    }

  private def transformNamedType(tpe: NamedType): NamedType =
    tpe.copy(name = clientType(tpe.name))

  private def sourceFieldDefinition(typeName: String, field: String): Option[__Field] =
    originalRootType.types.get(typeName).flatMap(tpe => Option(tpe.getFieldOrNull(field)))

  private def inputToClient(tpe: __Type, value: InputValue): InputValue =
    mapInputValue(tpe, value, ToClient)

  private def translateArguments(
    definitions: List[__InputValue],
    arguments: Map[String, InputValue],
    rename: String => String,
    direction: MappingDirection
  ): Map[String, InputValue] =
    arguments.iterator.map { case (name, value) =>
      val translatedName = rename(name)
      val sourceName     = if (direction == ToClient) name else translatedName
      val translated     = definitions
        .find(_.name == sourceName)
        .fold(value)(definition => mapInputValue(definition._type, value, direction))
      translatedName -> translated
    }.toMap

  private def nullable(tpe: __Type): __Type =
    if (tpe.kind == __TypeKind.NON_NULL) tpe.ofType.map(nullable).getOrElse(tpe) else tpe

  private def listElement(tpe: __Type): Option[__Type] = {
    val value = nullable(tpe)
    if (value.kind == __TypeKind.LIST) value.ofType.map(nullable) else None
  }

  private def mapInputValue(tpe: __Type, value: InputValue, direction: MappingDirection): InputValue =
    tpe.kind match {
      case __TypeKind.NON_NULL => tpe.ofType.fold(value)(mapInputValue(_, value, direction))
      case __TypeKind.LIST     =>
        value match {
          case InputListValue(values) =>
            InputListValue(values.map(item => tpe.ofType.fold(item)(mapInputValue(_, item, direction))))
          case other                  => tpe.ofType.fold(other)(mapInputValue(_, other, direction))
        }
      case kind
          if kind == __TypeKind.INPUT_OBJECT ||
            (direction == ToSource && (kind == __TypeKind.OBJECT || kind == __TypeKind.INTERFACE)) =>
        value match {
          case InputObjectValue(fields) =>
            val sourceTypeName = tpe.name.getOrElse("")
            val clientName     = clientType(sourceTypeName)
            val definition     = originalRootType.types.get(sourceTypeName)
            InputObjectValue(fields.iterator.map {
              case ("__typename", StringValue(name)) if kind != __TypeKind.INPUT_OBJECT =>
                "__typename" -> StringValue(sourceType(name))
              case (name, nested)                                                       =>
                val sourceName =
                  if (direction == ToClient) name
                  else if (kind == __TypeKind.INPUT_OBJECT) sourceInputFields.getOrElse(clientName -> name, name)
                  else sourceField(clientName, name)
                val outputName =
                  if (direction == ToClient) clientInputField(sourceTypeName, sourceName)
                  else sourceName
                val translated = definition.flatMap { tpe =>
                  if (kind == __TypeKind.INPUT_OBJECT) tpe.allInputFields.find(_.name == sourceName).map(_._type)
                  else Option(tpe.getFieldOrNull(sourceName)).map(_._type)
                }
                  .fold(nested)(mapInputValue(_, nested, direction))
                outputName -> translated
            }.toMap)
          case other                    => other
        }
      case __TypeKind.ENUM     =>
        val sourceName = tpe.name.getOrElse("")
        value match {
          case EnumValue(name)   =>
            if (direction == ToClient) EnumValue(clientEnumValue(sourceName, name))
            else EnumValue(sourceEnumValues.getOrElse(clientType(sourceName) -> name, name))
          case StringValue(name) =>
            if (direction == ToClient) EnumValue(clientEnumValue(sourceName, name))
            else EnumValue(sourceEnumValues.getOrElse(clientType(sourceName) -> name, name))
          case other             => other
        }
      case _                   => value
    }
}

private[gateway] object SchemaCoordinateMapping {

  private final case class FieldSetDirectives(names: Set[String], provides: Set[String])
  private sealed trait MappingDirection
  private case object ToClient extends MappingDirection
  private case object ToSource extends MappingDirection

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

  private final case class EnumValueCoordinate(typeName: String, name: String) extends Coordinate {
    val id          = s"enum:$typeName.$name"
    val display     = s"Enum value '$typeName.$name'"
    val targetScope = s"enum:$typeName"
    val plural      = "Enum values"
    val currentName = name

    def targetExists(context: CoordinateContext, target: String): Boolean =
      context.types.get(typeName).exists(_.allEnumValues.exists(_.name == target))
    def targetDescription(target: String): String                         = s"value '$target'"

    def restrictions(change: Change, context: CoordinateContext, prefix: String): List[String] =
      if (context.transportTypes.contains(typeName))
        List(s"$prefix Federation transport enum value '$typeName.$name' cannot be transformed.")
      else Nil
  }

  private final case class Change(coordinate: Coordinate, renamed: Option[String])

  private val federationRootFields = Set("_entities", "_service")

  private final case class CoordinateMappings(
    typeNames: Map[String, String] = Map.empty,
    fieldNames: Map[(String, String), String] = Map.empty,
    argumentNames: Map[(String, String, String), String] = Map.empty,
    inputFieldNames: Map[(String, String), String] = Map.empty,
    enumValueNames: Map[(String, String), String] = Map.empty,
    hiddenTypeSources: Set[String] = Set.empty,
    hiddenFieldSources: Set[(String, String)] = Set.empty,
    hiddenArgumentSources: Set[(String, String, String)] = Set.empty,
    hiddenInputFieldSources: Set[(String, String)] = Set.empty,
    hiddenEnumValueSources: Set[(String, String)] = Set.empty
  ) {
    def renamesNothing: Boolean =
      typeNames.isEmpty && fieldNames.isEmpty && argumentNames.isEmpty && inputFieldNames.isEmpty && enumValueNames.isEmpty

    def nonEmpty: Boolean =
      !renamesNothing || hiddenTypeSources.nonEmpty || hiddenFieldSources.nonEmpty || hiddenArgumentSources.nonEmpty ||
        hiddenInputFieldSources.nonEmpty || hiddenEnumValueSources.nonEmpty

    def add(change: Change): CoordinateMappings =
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
        case (InputFieldCoordinate(tpe, name), Some(renamed))      =>
          copy(inputFieldNames = inputFieldNames.updated(tpe -> name, renamed))
        case (InputFieldCoordinate(tpe, name), None)               =>
          copy(hiddenInputFieldSources = hiddenInputFieldSources + (tpe -> name))
        case (EnumValueCoordinate(tpe, name), Some(renamed))       =>
          copy(enumValueNames = enumValueNames.updated(tpe -> name, renamed))
        case (EnumValueCoordinate(tpe, name), None)                =>
          copy(hiddenEnumValueSources = hiddenEnumValueSources + (tpe -> name))
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
      renamedType(tpe) -> inputFieldNames.getOrElse(tpe -> name, name)
    }
    def hiddenEnumValues: Set[(String, String)]        = hiddenEnumValueSources.map { case (tpe, name) =>
      renamedType(tpe) -> enumValueNames.getOrElse(tpe -> name, name)
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
      case RenameInputField(tpe, name, renamed)      => Change(InputFieldCoordinate(tpe, name), Some(renamed))
      case HideInputField(tpe, name)                 => Change(InputFieldCoordinate(tpe, name), None)
      case RenameEnumValue(tpe, name, renamed)       => Change(EnumValueCoordinate(tpe, name), Some(renamed))
      case HideEnumValue(tpe, name)                  => Change(EnumValueCoordinate(tpe, name), None)
    }

  def compile(
    source: String,
    rootType: RootType,
    document: Document,
    federation: Boolean,
    transformations: List[SchemaTransformation]
  ): Either[List[String], SchemaCoordinateMapping] = {
    val prefix         = s"[$source]"
    val operationRoots =
      rootType.queryType.name.toSet ++ rootType.mutationType.flatMap(_.name).toSet ++
        rootType.subscriptionType.flatMap(_.name).toSet
    val context        = CoordinateContext(
      rootType.types,
      operationRoots,
      SchemaComposition.federationTransportTypes(document, federation)
    )
    val changes        = transformations.map(normalize)
    val mappings       = changes.foldLeft(CoordinateMappings())(_.add(_))
    val renames        = changes.collect { case Change(coordinate, Some(renamed)) => coordinate -> renamed }

    val missing                    = changes.collect {
      case Change(coordinate, _) if !coordinate.exists(context) =>
        s"$prefix ${coordinate.display} does not exist."
    }
    val restrictions               = changes.flatMap(change => change.coordinate.restrictions(change, context, prefix))
    val hiddenReferences           = referencedHiddenCoordinates(
      document,
      rootType,
      mappings.hiddenEnumValueSources,
      mappings.hiddenInputFieldSources
    )
    val hiddenEnumReferences       = hiddenReferences.enumValues.toList.sorted.map { case (tpe, value) =>
      s"$prefix Hidden enum value '$tpe.$value' is referenced by a directive or default value."
    }
    val hiddenInputFieldReferences = hiddenReferences.inputFields.toList.sorted.map { case (tpe, field) =>
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
      (missing ::: restrictions ::: hiddenEnumReferences ::: hiddenInputFieldReferences ::: invalidRenameTargets :::
        collisions ::: transformedCollisions ::: conflictingTransformations).distinct.sorted

    if (diagnostics.nonEmpty) Left(diagnostics)
    else
      Right(
        new SchemaCoordinateMapping(rootType, mappings)
      )
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
    hiddenEnums: Set[(String, String)],
    hiddenInputFields: Set[(String, String)]
  ): InputCoordinateReferences = {
    def directiveValues(directives: List[Directive]): List[(__Type, InputValue)] =
      directives.flatMap { directive =>
        rootType.additionalDirectives.find(_.name == directive.name).toList.flatMap { definition =>
          directive.arguments.toList.flatMap { case (name, value) =>
            definition.allArgs.find(_.name == name).map(argument => argument._type -> value)
          }
        }
      }

    def fieldValues(parent: String, fields: List[FieldDefinition]): List[(__Type, InputValue)] = {
      val definitions = rootType.types.get(parent).toList.flatMap(_.allFields)
      fields.flatMap { field =>
        val arguments = definitions.find(_.name == field.name).toList.flatMap(_.allArgs)
        defaultValues(arguments, field.args.map(value => value.name -> value.defaultValue))
      }
    }

    def inputValues(parent: String, fields: List[InputValueDefinition]): List[(__Type, InputValue)] =
      defaultValues(
        rootType.types.get(parent).toList.flatMap(_.allInputFields),
        fields.map(value => value.name -> value.defaultValue)
      )

    def defaultValues(
      definitions: List[__InputValue],
      values: List[(String, Option[InputValue])]
    ): List[(__Type, InputValue)] =
      values.flatMap { case (name, defaultValue) =>
        definitions.find(_.name == name).toList.flatMap(definition => defaultValue.map(definition._type -> _))
      }

    def directives(definition: Definition): List[Directive] =
      definition match {
        case value: SchemaDefinition          => value.directives
        case value: DirectiveDefinition       => value.args.flatMap(_.directives)
        case value: ObjectTypeDefinition      =>
          value.directives ::: value.fields.flatMap(field => field.directives ::: field.args.flatMap(_.directives))
        case value: InterfaceTypeDefinition   =>
          value.directives ::: value.fields.flatMap(field => field.directives ::: field.args.flatMap(_.directives))
        case value: InputObjectTypeDefinition => value.directives ::: value.fields.flatMap(_.directives)
        case value: EnumTypeDefinition        => value.directives ::: value.enumValuesDefinition.flatMap(_.directives)
        case value: UnionTypeDefinition       => value.directives
        case value: ScalarTypeDefinition      => value.directives
        case value: SchemaExtension           => value.directives
        case value: ObjectTypeExtension       =>
          value.directives ::: value.fields.flatMap(field => field.directives ::: field.args.flatMap(_.directives))
        case value: InterfaceTypeExtension    =>
          value.directives ::: value.fields.flatMap(field => field.directives ::: field.args.flatMap(_.directives))
        case value: InputObjectTypeExtension  => value.directives ::: value.fields.flatMap(_.directives)
        case value: EnumTypeExtension         => value.directives ::: value.enumValuesDefinition.flatMap(_.directives)
        case value: UnionTypeExtension        => value.directives
        case value: ScalarTypeExtension       => value.directives
        case _                                => Nil
      }

    def defaults(definition: Definition): List[(__Type, InputValue)] =
      definition match {
        case value: DirectiveDefinition       =>
          val arguments = rootType.additionalDirectives.find(_.name == value.name).toList.flatMap(_.allArgs)
          defaultValues(arguments, value.args.map(argument => argument.name -> argument.defaultValue))
        case value: ObjectTypeDefinition      => fieldValues(value.name, value.fields)
        case value: InterfaceTypeDefinition   => fieldValues(value.name, value.fields)
        case value: InputObjectTypeDefinition => inputValues(value.name, value.fields)
        case value: ObjectTypeExtension       => fieldValues(value.name, value.fields)
        case value: InterfaceTypeExtension    => fieldValues(value.name, value.fields)
        case value: InputObjectTypeExtension  => inputValues(value.name, value.fields)
        case _                                => Nil
      }

    val found = document.definitions.iterator
      .flatMap(definition => directiveValues(directives(definition)) ::: defaults(definition))
      .map { case (tpe, value) => inputCoordinateReferences(tpe, value) }
      .foldLeft(InputCoordinateReferences())(_ ++ _)

    InputCoordinateReferences(
      enumValues = found.enumValues.filter(hiddenEnums),
      inputFields = found.inputFields.filter(hiddenInputFields)
    )
  }
}
