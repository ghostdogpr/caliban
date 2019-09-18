package caliban.introspection.adt

case class __Field(
  name: String,
  description: Option[String],
  args: List[__InputValue],
  `type`: () => __Type,
  isDeprecated: Boolean,
  deprecationReason: Option[String]
)
