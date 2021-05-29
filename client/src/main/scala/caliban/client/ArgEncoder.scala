package caliban.client

import caliban.client.__Value.{ __BooleanValue, __ListValue, __NullValue, __NumberValue, __ObjectValue, __StringValue }
import io.circe.Json

import scala.annotation.implicitNotFound

/**
 * Typeclass that defines how to encode an argument of type `A` into a valid [[caliban.client.__Value]].
 * Every type that can be passed as an argument needs an instance of `ArgEncoder`.
 */
@implicitNotFound(
  """Cannot find an ArgEncoder for type ${A}.
     
Caliban needs it to know how to encode arguments of type ${A}.
"""
)
trait ArgEncoder[-A] {
  def encode(value: A): __Value
}

object ArgEncoder {

  implicit val int: ArgEncoder[Int] = (value: Int) => __NumberValue(value)

  implicit val long: ArgEncoder[Long] = (value: Long) => __NumberValue(value)

  implicit val bigInt: ArgEncoder[BigInt] = (value: BigInt) => __NumberValue(BigDecimal(value))

  implicit val double: ArgEncoder[Double] = (value: Double) => __NumberValue(value)

  implicit val bigDecimal: ArgEncoder[BigDecimal] = (value: BigDecimal) => __NumberValue(value)

  implicit val string: ArgEncoder[String] = (value: String) => __StringValue(value)

  implicit val boolean: ArgEncoder[Boolean] = (value: Boolean) => __BooleanValue(value)

  implicit val unit: ArgEncoder[Unit] = (_: Unit) => __ObjectValue(Nil)

  implicit def option[A](implicit ev: ArgEncoder[A]): ArgEncoder[Option[A]] = (value: Option[A]) =>
    value.fold(__NullValue: __Value)(ev.encode)

  implicit def list[A](implicit ev: ArgEncoder[A]): ArgEncoder[List[A]] = (value: List[A]) =>
    __ListValue(value.map(ev.encode))

  implicit val json: ArgEncoder[Json] = (value: Json) => __Value.valueDecoder.decodeJson(value).getOrElse(__NullValue)
}
