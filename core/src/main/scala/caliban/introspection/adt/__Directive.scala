package caliban.introspection.adt

import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveDefinition

case class __Directive(
  name: String,
  description: Option[String],
  locations: Set[__DirectiveLocation],
  args: __DeprecatedArgs => List[__InputValue],
  isRepeatable: Boolean
) {
  def toDirectiveDefinition: DirectiveDefinition =
    DirectiveDefinition(
      description,
      name,
      allArgs.map(_.toInputValueDefinition),
      isRepeatable,
      locations.map(_.toDirectiveLocation)
    )

  lazy val allArgs: List[__InputValue] =
    args(__DeprecatedArgs.include)
}
