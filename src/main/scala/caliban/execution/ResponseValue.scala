package caliban.execution

sealed trait ResponseValue

object ResponseValue {
  case object NullValue extends ResponseValue {
    override def toString: String = "null"
  }
  case class IntValue(value: Int) extends ResponseValue {
    override def toString: String = value.toString
  }
  case class FloatValue(value: Float) extends ResponseValue {
    override def toString: String = value.toString
  }
  case class StringValue(value: String) extends ResponseValue {
    override def toString: String = s""""$value""""
  }
  case class BooleanValue(value: Boolean) extends ResponseValue {
    override def toString: String = if (value) "true" else "false"
  }
  case class EnumValue(value: String) extends ResponseValue {
    override def toString: String = s""""$value""""
  }
  case class ListValue(values: List[ResponseValue]) extends ResponseValue {
    override def toString: String = values.mkString("[", ",", "]")
  }
  case class ObjectValue(fields: List[(String, ResponseValue)]) extends ResponseValue {
    override def toString: String =
      fields.map { case (name, value) => s""""$name":${value.toString}""" }.mkString("{", ",", "}")
  }
}
