package caliban.introspection.adt

sealed trait SchemaElement

object SchemaElement {
  final case class Type(value: __Type)             extends SchemaElement
  final case class Field(value: __Field)           extends SchemaElement
  final case class InputValue(value: __InputValue) extends SchemaElement
  final case class EnumValue(value: __EnumValue)   extends SchemaElement
  final case class Directive(value: __Directive)   extends SchemaElement
}
