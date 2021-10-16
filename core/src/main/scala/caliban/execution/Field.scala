package caliban.execution

import caliban.InputValue
import caliban.Value
import caliban.Value.{ BooleanValue, NullValue }
import caliban.introspection.adt.{ __DeprecatedArgs, __Type, __TypeKind }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.FragmentDefinition
import caliban.parsing.adt.Selection.{ Field => F, FragmentSpread, InlineFragment }
import caliban.parsing.adt.{ Directive, LocationInfo, Selection, Type, VariableDefinition }
import caliban.schema.{ RootType, Types }

case class Field(
  name: String,
  fieldType: __Type,
  parentType: Option[__Type],
  alias: Option[String] = None,
  fields: List[Field] = Nil,
  condition: Option[List[String]] = None,
  arguments: Map[String, InputValue] = Map(),
  _locationInfo: () => LocationInfo = () => LocationInfo.origin,
  directives: List[Directive] = List.empty
) {
  lazy val locationInfo: LocationInfo = _locationInfo()
}

object Field {
  def apply(
    selectionSet: List[Selection],
    fragments: Map[String, FragmentDefinition],
    variableValues: Map[String, InputValue],
    variableDefinitions: List[VariableDefinition],
    fieldType: __Type,
    sourceMapper: SourceMapper,
    directives: List[Directive],
    rootType: RootType
  ): Field = {
    def loop(selectionSet: List[Selection], fieldType: __Type): Field = {
      val fieldList = List.newBuilder[Field]
      val innerType = Types.innerType(fieldType)
      selectionSet.foreach {
        case F(alias, name, arguments, directives, selectionSet, index)
            if checkDirectives(directives, variableValues) =>
          val selected = innerType
            .fields(__DeprecatedArgs(Some(true)))
            .flatMap(_.find(_.name == name))

          val schemaDirectives = selected.flatMap(_.directives).getOrElse(Nil)

          val t = selected.fold(Types.string)(_.`type`()) // default only case where it's not found is __typename

          val field = loop(selectionSet, t)
          fieldList +=
            Field(
              name,
              t,
              Some(innerType),
              alias,
              field.fields,
              None,
              resolveVariables(arguments, variableDefinitions, variableValues, rootType),
              () => sourceMapper.getLocation(index),
              directives ++ schemaDirectives
            )
        case FragmentSpread(name, directives) if checkDirectives(directives, variableValues)                        =>
          fragments
            .get(name)
            .foreach { f =>
              val t =
                innerType.possibleTypes.flatMap(_.find(_.name.contains(f.typeCondition.name))).getOrElse(fieldType)
              fieldList ++= loop(f.selectionSet, t).fields.map(field =>
                if (field.condition.isDefined) field
                else field.copy(condition = subtypeNames(f.typeCondition.name, rootType))
              )
            }
        case InlineFragment(typeCondition, directives, selectionSet) if checkDirectives(directives, variableValues) =>
          val t     = innerType.possibleTypes
            .flatMap(_.find(_.name.exists(typeCondition.map(_.name).contains)))
            .getOrElse(fieldType)
          val field = loop(selectionSet, t)
          typeCondition match {
            case None           => fieldList ++= field.fields
            case Some(typeName) =>
              fieldList ++= field.fields.map(field =>
                if (field.condition.isDefined) field else field.copy(condition = subtypeNames(typeName.name, rootType))
              )
          }
        case _                                                                                                      =>
      }
      Field("", fieldType, None, fields = fieldList.result())
    }

    loop(selectionSet, fieldType).copy(directives = directives)
  }

  private def resolveVariables(
    arguments: Map[String, InputValue],
    variableDefinitions: List[VariableDefinition],
    variableValues: Map[String, InputValue],
    rootType: RootType
  ): Map[String, InputValue] = {
    def resolveVariable(value: InputValue): InputValue =
      value match {
        case InputValue.ListValue(values)   => InputValue.ListValue(values.map(resolveVariable))
        case InputValue.ObjectValue(fields) =>
          InputValue.ObjectValue(fields.map { case (k, v) => k -> resolveVariable(v) })
        case InputValue.VariableValue(name) =>
          (for {
            definition  <- variableDefinitions.find(_.name == name)
            defaultValue = definition.defaultValue getOrElse NullValue
            value        = variableValues.getOrElse(name, defaultValue)
          } yield resolveEnumValues(value, definition, rootType)) getOrElse NullValue
        case value: Value                   => value
      }
    arguments.map { case (k, v) => k -> resolveVariable(v) }
  }

  // Since we cannot separate a String from an Enum when variables
  // are parsed, we need to translate from strings to enums here
  // if we have a valid enum field.
  private def resolveEnumValues(
    value: InputValue,
    definition: VariableDefinition,
    rootType: RootType
  ): InputValue = {
    val t = Type
      .innerType(definition.variableType)

    rootType.types
      .get(t)
      .map(_.kind)
      .flatMap { kind =>
        (kind, value) match {
          case (__TypeKind.ENUM, Value.StringValue(v)) =>
            Some(Value.EnumValue(v))
          case _                                       => None
        }
      }
      .getOrElse(value)
  }

  private def subtypeNames(typeName: String, rootType: RootType): Option[List[String]] =
    rootType.types
      .get(typeName)
      .map(t =>
        typeName ::
          t.possibleTypes
            .fold(List.empty[String])(_.flatMap(_.name.map(subtypeNames(_, rootType).getOrElse(Nil))).flatten)
      )

  private def checkDirectives(directives: List[Directive], variableValues: Map[String, InputValue]): Boolean =
    !checkDirective("skip", default = false, directives, variableValues) &&
      checkDirective("include", default = true, directives, variableValues)

  private def checkDirective(
    name: String,
    default: Boolean,
    directives: List[Directive],
    variableValues: Map[String, InputValue]
  ): Boolean =
    directives
      .find(_.name == name)
      .flatMap(_.arguments.get("if")) match {
      case Some(BooleanValue(value))            => value
      case Some(InputValue.VariableValue(name)) =>
        variableValues
          .get(name) match {
          case Some(BooleanValue(value)) => value
          case _                         => default
        }
      case _                                    => default
    }
}
