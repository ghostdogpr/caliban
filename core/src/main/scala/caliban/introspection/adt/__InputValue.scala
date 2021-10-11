package caliban.introspection.adt

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition
import caliban.parsing.adt.Directive
import caliban.parsing.Parser

final case class __InputValue(
  name: String,
  description: Option[String],
  `type`: () => __Type,
  defaultValue: Option[String],
  directives: Option[List[Directive]] = None
) {
  def toInputValueDefinition: InputValueDefinition = {
    val default = defaultValue.flatMap(v => Parser.parseInputValue(v).toOption)
    InputValueDefinition(description, name, `type`().toType(), default, directives.getOrElse(Nil))
  }
}
