package caliban.execution

import caliban.InputValue
import caliban.Value.BooleanValue
import caliban.parsing.adt.ExecutableDefinition.FragmentDefinition
import caliban.parsing.adt.{ Directive, Selection }
import caliban.parsing.adt.Selection.{ Field => F, FragmentSpread, InlineFragment }

case class Field(
  name: String,
  alias: Option[String] = None,
  fields: List[Field] = Nil,
  conditionalFields: Map[String, List[Field]] = Map(),
  arguments: Map[String, InputValue] = Map()
)

object Field {
  def apply(
    selectionSet: List[Selection],
    fragments: Map[String, FragmentDefinition],
    variableValues: Map[String, InputValue]
  ): Field = {

    def loop(selectionSet: List[Selection]): Field = {
      val (fields, cFields) = selectionSet.map {
        case f @ F(alias, name, arguments, _, selectionSet) if checkDirectives(f.directives, variableValues) =>
          val field = loop(selectionSet)
          (
            List(Field(name, alias, field.fields, field.conditionalFields, arguments)),
            Map.empty[String, List[Field]]
          )
        case FragmentSpread(name, directives) if checkDirectives(directives, variableValues) =>
          fragments
            .get(name)
            .map { f =>
              val field = loop(f.selectionSet)
              (Nil, combineMaps(List(field.conditionalFields, Map(f.typeCondition.name -> field.fields))))
            }
            .getOrElse((Nil, Map.empty[String, List[Field]]))
        case InlineFragment(typeCondition, directives, selectionSet) if checkDirectives(directives, variableValues) =>
          val field = loop(selectionSet)
          typeCondition match {
            case None           => (field.fields, field.conditionalFields)
            case Some(typeName) => (Nil, combineMaps(List(field.conditionalFields, Map(typeName.name -> field.fields))))
          }
        case _ => (Nil, Map.empty[String, List[Field]])
      }.unzip
      Field("", fields = fields.flatten, conditionalFields = combineMaps(cFields))
    }

    loop(selectionSet)
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
