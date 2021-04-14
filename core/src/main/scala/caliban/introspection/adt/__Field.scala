package caliban.introspection.adt

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.{ FieldDefinition, InputValueDefinition }
import caliban.parsing.adt.Directive

case class __Field(
  name: String,
  description: Option[String],
  args: List[__InputValue],
  `type`: () => __Type,
  isDeprecated: Boolean = false,
  deprecationReason: Option[String] = None,
  directives: Option[List[Directive]] = None
) {
  def toFieldDefinition: FieldDefinition =
    FieldDefinition(description, name, args.map(_.toInputValueDefinition), `type`().toType(), directives.getOrElse(Nil))

  def toInputValueDefinition: InputValueDefinition =
    InputValueDefinition(description, name, `type`().toType(), None, directives.getOrElse(Nil))
}
