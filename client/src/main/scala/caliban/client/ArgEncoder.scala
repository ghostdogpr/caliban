package caliban.client

import caliban.client.Value.{ BooleanValue, ListValue, NullValue, NumberValue, ObjectValue, StringValue }
import io.circe.Json

/**
 * Typeclass that defines how to encode an argument of type `A` into a valid [[caliban.client.Value]].
 * Every type that can be passed as an argument needs an instance of `ArgEncoder`.
 */
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

  implicit val long: ArgEncoder[Long] = new ArgEncoder[Long] {
    override def encode(value: Long): Value = NumberValue(value)
    override def typeName: String           = "Long"
  }

  implicit val bigInt: ArgEncoder[BigInt] = new ArgEncoder[BigInt] {
    override def encode(value: BigInt): Value = NumberValue(BigDecimal(value))
    override def typeName: String             = "BigInt"
  }

  implicit val double: ArgEncoder[Double] = new ArgEncoder[Double] {
    override def encode(value: Double): Value = NumberValue(value)
    override def typeName: String             = "Double"
  }

  implicit val bigDecimal: ArgEncoder[BigDecimal] = new ArgEncoder[BigDecimal] {
    override def encode(value: BigDecimal): Value = NumberValue(value)
    override def typeName: String                 = "BigDecimal"
  }

  implicit val string: ArgEncoder[String] = new ArgEncoder[String] {
    override def encode(value: String): Value = StringValue(value)
    override def typeName: String             = "String"
  }

  implicit val boolean: ArgEncoder[Boolean] = new ArgEncoder[Boolean] {
    override def encode(value: Boolean): Value = BooleanValue(value)
    override def typeName: String              = "Boolean"
  }

  implicit val unit: ArgEncoder[Unit] = new ArgEncoder[Unit] {
    override def encode(value: Unit): Value = ObjectValue(Nil)
    override def typeName: String           = "Unit"
  }

  implicit def option[A](implicit ev: ArgEncoder[A]): ArgEncoder[Option[A]] = new ArgEncoder[Option[A]] {
    override def encode(value: Option[A]): Value = value.fold(NullValue: Value)(ev.encode)
    override def typeName: String                = ev.typeName
    override def optional: Boolean               = true
  }

  implicit def list[A](implicit ev: ArgEncoder[A]): ArgEncoder[List[A]] = new ArgEncoder[List[A]] {
    override def encode(value: List[A]): Value = ListValue(value.map(ev.encode))
    override def typeName: String              = s"[${ev.typeName}]"
  }

  implicit val json: ArgEncoder[Json] = new ArgEncoder[Json] {
    override def encode(value: Json): Value = Value.valueDecoder.decodeJson(value).getOrElse(NullValue)
    override def typeName: String           = "Json"
  }
}
