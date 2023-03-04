package caliban.introspection.adt

import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveDefinition

case class __Directive(
  name: String,
  description: Option[String],
  locations: Set[__DirectiveLocation],
  args: List[__InputValue],
  repeatable: Boolean
) {
  def toDirectiveDefinition: DirectiveDefinition =
    DirectiveDefinition(
      description,
      name,
      args.map(_.toInputValueDefinition),
      repeatable,
      locations.map(_.toDirectiveLocation)
    )
}
