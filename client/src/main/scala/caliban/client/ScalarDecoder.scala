package caliban.client

import scala.util.Try
import java.util.UUID
import caliban.client.ResponseValue._

trait ScalarDecoder[+A] {
  def decode(value: ResponseValue): Either[String, A]
}

object ScalarDecoder {
  implicit val int: ScalarDecoder[Int] = {
    case value: NumberValue => Try(value.value.toIntExact).toEither.left.map(_.toString)
    case other              => Left(s"Can't build an Int from input $other")
  }
  implicit val long: ScalarDecoder[Long] = {
    case value: NumberValue => Try(value.value.toLongExact).toEither.left.map(_.toString)
    case other              => Left(s"Can't build a Long from input $other")
  }
  implicit val bigInt: ScalarDecoder[BigInt] = {
    case value: NumberValue =>
      Try(value.value.toBigIntExact).toEither.left.map(_.toString).flatMap {
        case None    => Left(s"Can't build a BigInt from input ${value.value}")
        case Some(v) => Right(v)
      }
    case other => Left(s"Can't build a BigInt from input $other")
  }
  implicit val float: ScalarDecoder[Float] = {
    case value: NumberValue => Right(value.value.toFloat)
    case other              => Left(s"Can't build a Float from input $other")
  }
  implicit val double: ScalarDecoder[Double] = {
    case value: NumberValue => Right(value.value.toDouble)
    case other              => Left(s"Can't build a Double from input $other")
  }
  implicit val bigDecimal: ScalarDecoder[BigDecimal] = {
    case value: NumberValue => Right(value.value)
    case other              => Left(s"Can't build a BigDecimal from input $other")
  }
  implicit val boolean: ScalarDecoder[Boolean] = {
    case BooleanValue(value) => Right(value)
    case other               => Left(s"Can't build a Boolean from input $other")
  }
  implicit val string: ScalarDecoder[String] = {
    case StringValue(value) => Right(value)
    case other              => Left(s"Can't build a String from input $other")
  }
  implicit val uuid: ScalarDecoder[UUID] = {
    case StringValue(value) => Try(UUID.fromString(value)).toEither.left.map(_.toString)
    case other              => Left(s"Can't build a UUID from input $other")
  }
  implicit val unit: ScalarDecoder[Unit] = {
    case ObjectValue(Nil) => Right(())
    case other            => Left(s"Can't build Unit from input $other")
  }
}
