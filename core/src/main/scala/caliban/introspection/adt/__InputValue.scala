package caliban.introspection.adt

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition
import caliban.parsing.adt.Directive
import caliban.parsing.Parser

case class __InputValue(
  name: String,
  description: Option[String],
  `type`: () => __Type,
  defaultValue: Option[String],
  directives: Option[List[Directive]] = None
) {
  def toInputValueDefinition: InputValueDefinition = {
    val default = defaultValue.flatMap(v => Parser.parseInputValue(v).toOption)
    InputValueDefinition(description, name, _type.toType(), default, directives.getOrElse(Nil))
  }

  private[caliban] lazy val _type: __Type = `type`()

  private[caliban] def nullable: __InputValue = copy(`type` = () => _type.nullable)
}
