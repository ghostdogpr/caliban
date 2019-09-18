package caliban.introspection.adt

case class __Directive(
  name: String,
  description: Option[String],
  locations: Set[__DirectiveLocation],
  args: List[__InputValue]
)
