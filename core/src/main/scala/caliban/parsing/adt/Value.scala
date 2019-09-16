package caliban.parsing.adt

sealed trait Value

object Value {
  case object NullValue                              extends Value
  case class IntValue(value: Int)                    extends Value
  case class FloatValue(value: Float)                extends Value
  case class StringValue(value: String)              extends Value
  case class BooleanValue(value: Boolean)            extends Value
  case class EnumValue(value: String)                extends Value
  case class ListValue(values: List[Value])          extends Value
  case class ObjectValue(fields: Map[String, Value]) extends Value
  case class VariableValue(name: String)             extends Value
}
