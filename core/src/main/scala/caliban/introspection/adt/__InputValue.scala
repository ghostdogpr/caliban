package caliban.introspection.adt

case class __InputValue(name: String, description: Option[String], `type`: () => __Type, defaultValue: Option[String])
