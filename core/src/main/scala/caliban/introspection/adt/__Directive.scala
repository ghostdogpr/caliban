package caliban.introspection.adt

import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveDefinition

case class __Directive(
  name: String,
  description: Option[String],
  locations: Set[__DirectiveLocation],
  args: List[__InputValue],
  isRepeatable: Boolean
) {
  def toDirectiveDefinition: DirectiveDefinition =
    DirectiveDefinition(
      description,
      name,
      args.map(_.toInputValueDefinition),
      isRepeatable,
      locations.map(_.toDirectiveLocation)
    )

  @deprecated("Use isRepeatable")
  def repeatable: Boolean = isRepeatable
}
