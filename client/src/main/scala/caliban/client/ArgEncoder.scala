package caliban.client

import caliban.client.Value.{ BooleanValue, NullValue, NumberValue, StringValue }

trait ArgEncoder[-A] {
  def encode(value: A): Value
  def typeName: String
  def optional: Boolean      = false
  def formatTypeName: String = if (optional) typeName else s"$typeName!"
}

object ArgEncoder {

  implicit val int: ArgEncoder[Int] = new ArgEncoder[Int] {
    override def encode(value: Int): Value = NumberValue(value)
    override def typeName: String          = "Int"
  }

  implicit val string: ArgEncoder[String] = new ArgEncoder[String] {
    override def encode(value: String): Value = StringValue(value)
    override def typeName: String             = "String"
  }

  implicit val boolean: ArgEncoder[Boolean] = new ArgEncoder[Boolean] {
    override def encode(value: Boolean): Value = BooleanValue(value)
    override def typeName: String              = "Boolean"
  }

  implicit def option[A](implicit ev: ArgEncoder[A]): ArgEncoder[Option[A]] = new ArgEncoder[Option[A]] {
    override def encode(value: Option[A]): Value = value.fold(NullValue: Value)(ev.encode)
    override def typeName: String                = ev.typeName
    override def optional: Boolean               = true
  }

}
