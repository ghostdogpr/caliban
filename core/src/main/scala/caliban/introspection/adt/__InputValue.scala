package caliban.introspection.adt

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition
import caliban.parsing.adt.Directive

case class __InputValue(
  name: String,
  description: Option[String],
  `type`: () => __Type,
  defaultValue: Option[String],
  directives: Option[List[Directive]] = None
) {
  def toInputValueDefinition: InputValueDefinition =
    InputValueDefinition(description, name, `type`().toType(), None, directives.getOrElse(Nil))
}
