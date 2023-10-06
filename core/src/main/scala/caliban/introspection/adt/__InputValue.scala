package caliban.introspection.adt

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.InputValueDefinition
import caliban.parsing.adt.Directive
import caliban.parsing.Parser
import caliban.schema.Annotations.GQLExcluded

case class __InputValue(
  name: String,
  description: Option[String],
  `type`: () => __Type,
  defaultValue: Option[String],
  @GQLExcluded directives: Option[List[Directive]] = None,
  @GQLExcluded renameInput: String => String = identity
) {
  def toInputValueDefinition: InputValueDefinition = {
    val default = defaultValue.flatMap(v => Parser.parseInputValue(v).toOption)
    InputValueDefinition(description, name, _type.toType(), default, directives.getOrElse(Nil))
  }

  private[caliban] lazy val _type: __Type = `type`()
}
