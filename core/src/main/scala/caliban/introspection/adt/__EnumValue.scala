package caliban.introspection.adt

case class __EnumValue(
  name: String,
  description: Option[String],
  isDeprecated: Boolean,
  deprecationReason: Option[String]
)
