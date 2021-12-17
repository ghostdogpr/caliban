package caliban.execution

import scala.collection.mutable.ArrayBuffer
import caliban.Value.BooleanValue
import caliban.introspection.adt.{ __DeprecatedArgs, __Type }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.FragmentDefinition
import caliban.parsing.adt.Selection.{ Field => F, FragmentSpread, InlineFragment }
import caliban.parsing.adt.{ Directive, LocationInfo, Selection, VariableDefinition }
import caliban.schema.{ RootType, Types }
import caliban.{ InputValue, Value }

case class Field(
  name: String,
  fieldType: __Type,
  parentType: Option[__Type],
  alias: Option[String] = None,
  fields: List[Field] = Nil,
  condition: Option[Set[String]] = None,
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
      val fieldList  = ArrayBuffer.empty[Field]
      val map        = collection.mutable.Map.empty[(String, String), Int]
      var fieldIndex = 0

      def addField(f: Field, condition: Option[String]): Unit = {
        val name = f.alias.getOrElse(f.name)
        val key  = (name, condition.getOrElse(""))
        map.get(key) match {
          case None        =>
            // first time we see this field, add it to the array
            fieldList += f
            map.update(key, fieldIndex)
            fieldIndex = fieldIndex + 1
          case Some(index) =>
            // field already existed, merge it
            val existing = fieldList(index)
            fieldList(index) = existing.copy(
              fields = existing.fields ::: f.fields,
              condition = (existing.condition, f.condition) match {
                case (Some(v1), Some(v2)) => if (v1 == v2) existing.condition else Some(v1 ++ v2)
                case (Some(_), None)      => existing.condition
                case (None, Some(_))      => f.condition
                case (None, None)         => None
              }
            )
        }
      }

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

          addField(
            Field(
              name,
              t,
              Some(innerType),
              alias,
              field.fields,
              None,
              resolveVariables(arguments, variableDefinitions, variableValues),
              () => sourceMapper.getLocation(index),
              directives ++ schemaDirectives
            ),
            None
          )
        case FragmentSpread(name, directives) if checkDirectives(directives, variableValues)                        =>
          fragments
            .get(name)
            .foreach { f =>
              val t =
                innerType.possibleTypes.flatMap(_.find(_.name.contains(f.typeCondition.name))).getOrElse(fieldType)
              loop(f.selectionSet, t).fields
                .map(field =>
                  if (field.condition.isDefined) field
                  else field.copy(condition = subtypeNames(f.typeCondition.name, rootType))
                )
                .foreach(addField(_, Some(f.typeCondition.name)))
            }
        case InlineFragment(typeCondition, directives, selectionSet) if checkDirectives(directives, variableValues) =>
          val t     = innerType.possibleTypes
            .flatMap(_.find(_.name.exists(typeCondition.map(_.name).contains)))
            .getOrElse(fieldType)
          val field = loop(selectionSet, t)
          typeCondition match {
            case None           => if (field.fields.nonEmpty) fieldList ++= field.fields
            case Some(typeName) =>
              field.fields
                .map(field =>
                  if (field.condition.isDefined) field
                  else field.copy(condition = subtypeNames(typeName.name, rootType))
                )
                .foreach(addField(_, Some(typeName.name)))
          }
        case _                                                                                                      =>
      }
      Field("", fieldType, None, fields = fieldList.toList)
    }

    loop(selectionSet, fieldType).copy(directives = directives)
  }

  private def resolveVariables(
    arguments: Map[String, InputValue],
    variableDefinitions: List[VariableDefinition],
    variableValues: Map[String, InputValue]
  ): Map[String, InputValue] = {
    def resolveVariable(value: InputValue): Option[InputValue] =
      value match {
        case InputValue.ListValue(values)   =>
          Some(InputValue.ListValue(values.flatMap(resolveVariable)))
        case InputValue.ObjectValue(fields) =>
          Some(InputValue.ObjectValue(fields.flatMap { case (k, v) => resolveVariable(v).map(k -> _) }))
        case InputValue.VariableValue(name) =>
          for {
            definition <- variableDefinitions.find(_.name == name)
            value      <- variableValues.get(name).orElse(definition.defaultValue)
          } yield value
        case value: Value                   =>
          Some(value)
      }
    arguments.flatMap { case (k, v) => resolveVariable(v).map(k -> _) }
  }

  private def subtypeNames(typeName: String, rootType: RootType): Option[Set[String]] =
    rootType.types
      .get(typeName)
      .map(t =>
        t.possibleTypes
          .fold(Set.empty[String])(
            _.map(_.name.map(subtypeNames(_, rootType).getOrElse(Set.empty))).toSet.flatten.flatten
          ) + typeName
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
