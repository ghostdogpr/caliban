package caliban.introspection.adt

case class __EnumValue(
  name: String,
  description: Option[String] = None,
  isDeprecated: Boolean,
  deprecationReason: Option[String] = None
)
