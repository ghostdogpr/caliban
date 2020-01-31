package caliban.execution

import caliban.InputValue
import caliban.Value.BooleanValue
import caliban.introspection.adt.{ __DeprecatedArgs, __Type }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.FragmentDefinition
import caliban.parsing.adt.Selection.{ FragmentSpread, InlineFragment, Field => F }
import caliban.parsing.adt.{ Directive, LocationInfo, Selection }
import caliban.schema.Types

case class Field(
  name: String,
  fieldType: __Type,
  parentType: Option[__Type],
  alias: Option[String] = None,
  fields: List[Field] = Nil,
  conditionalFields: Map[String, List[Field]] = Map(),
  arguments: Map[String, InputValue] = Map(),
  locationInfo: LocationInfo = LocationInfo.origin
)

object Field {
  def apply(
    selectionSet: List[Selection],
    fragments: Map[String, FragmentDefinition],
    variableValues: Map[String, InputValue],
    fieldType: __Type,
    sourceMapper: SourceMapper
  ): Field = {

    def loop(selectionSet: List[Selection], fieldType: __Type): Field = {
      val innerType = Types.innerType(fieldType)
      val (fields, cFields) = selectionSet.map {
        case f @ F(alias, name, arguments, _, selectionSet, index) if checkDirectives(f.directives, variableValues) =>
          val t = innerType
            .fields(__DeprecatedArgs(Some(true)))
            .flatMap(_.find(_.name == name))
            .fold(Types.string)(_.`type`()) // default only case where it's not found is __typename
          val field = loop(selectionSet, t)
          (
            List(
              Field(
                name,
                t,
                Some(innerType),
                alias,
                field.fields,
                field.conditionalFields,
                arguments,
                sourceMapper.getLocation(index)
              )
            ),
            Map.empty[String, List[Field]]
          )
        case FragmentSpread(name, directives) if checkDirectives(directives, variableValues) =>
          lazy val default = (Nil, Map.empty[String, List[Field]])
          fragments
            .get(name)
            .fold(default) { f =>
              val t =
                innerType.possibleTypes.flatMap(_.find(_.name.contains(f.typeCondition.name))).getOrElse(fieldType)
              val field = loop(f.selectionSet, t)
              (Nil, combineMaps(List(field.conditionalFields, Map(f.typeCondition.name -> field.fields))))
            }
        case InlineFragment(typeCondition, directives, selectionSet) if checkDirectives(directives, variableValues) =>
          val t = innerType.possibleTypes
            .flatMap(_.find(_.name.exists(typeCondition.map(_.name).contains)))
            .getOrElse(fieldType)
          val field = loop(selectionSet, t)
          typeCondition match {
            case None           => (field.fields, field.conditionalFields)
            case Some(typeName) => (Nil, combineMaps(List(field.conditionalFields, Map(typeName.name -> field.fields))))
          }
        case _ => (Nil, Map.empty[String, List[Field]])
      }.unzip
      Field("", fieldType, None, fields = fields.flatten, conditionalFields = combineMaps(cFields))
    }

    loop(selectionSet, fieldType)
  }

  private def combineMaps[A, B](maps: List[Map[A, List[B]]]): Map[A, List[B]] =
    maps.foldLeft(Map.empty[A, List[B]]) {
      case (result, map) =>
        map.foldLeft(result) { case (result, (k, v)) => result.updated(k, result.getOrElse(k, Nil) ++ v) }
    }

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
      case Some(BooleanValue(value)) => value
      case Some(InputValue.VariableValue(name)) =>
        variableValues
          .get(name) match {
          case Some(BooleanValue(value)) => value
          case _                         => default
        }
      case _ => default
    }
}
