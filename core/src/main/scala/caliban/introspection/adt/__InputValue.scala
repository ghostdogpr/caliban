package caliban.introspection.adt

import caliban.parsing.adt.Directive

case class __InputValue(
  name: String,
  description: Option[String],
  `type`: () => __Type,
  defaultValue: Option[String],
  directives: Option[List[Directive]] = None
)
