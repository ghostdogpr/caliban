package caliban.client

import scala.util.Try
import java.util.UUID
import caliban.client.CalibanClientError.DecodingError
import caliban.client.Value._

trait ScalarDecoder[+A] {
  def decode(value: Value): Either[DecodingError, A]
}

object ScalarDecoder {
  implicit val int: ScalarDecoder[Int] = {
    case NumberValue(value) =>
      Try(value.toIntExact).toEither.left.map(ex => DecodingError(s"Can't build an Int from input $value", Some(ex)))
    case other => Left(DecodingError(s"Can't build an Int from input $other"))
  }
  implicit val long: ScalarDecoder[Long] = {
    case NumberValue(value) =>
      Try(value.toLongExact).toEither.left.map(ex => DecodingError(s"Can't build a Long from input $value", Some(ex)))
    case other => Left(DecodingError(s"Can't build a Long from input $other"))
  }
  implicit val bigInt: ScalarDecoder[BigInt] = {
    case NumberValue(value) =>
      Try(value.toBigIntExact).toEither.left
        .map(ex => DecodingError(s"Can't build a BigInt from input $value", Some(ex)))
        .flatMap {
          case None    => Left(DecodingError(s"Can't build a BigInt from input $value"))
          case Some(v) => Right(v)
        }
    case other => Left(DecodingError(s"Can't build a BigInt from input $other"))
  }
  implicit val float: ScalarDecoder[Float] = {
    case NumberValue(value) => Right(value.toFloat)
    case other              => Left(DecodingError(s"Can't build a Float from input $other"))
  }
  implicit val double: ScalarDecoder[Double] = {
    case NumberValue(value) => Right(value.toDouble)
    case other              => Left(DecodingError(s"Can't build a Double from input $other"))
  }
  implicit val bigDecimal: ScalarDecoder[BigDecimal] = {
    case NumberValue(value) => Right(value)
    case other              => Left(DecodingError(s"Can't build a BigDecimal from input $other"))
  }
  implicit val boolean: ScalarDecoder[Boolean] = {
    case BooleanValue(value) => Right(value)
    case other               => Left(DecodingError(s"Can't build a Boolean from input $other"))
  }
  implicit val string: ScalarDecoder[String] = {
    case StringValue(value) => Right(value)
    case other              => Left(DecodingError(s"Can't build a String from input $other"))
  }
  implicit val uuid: ScalarDecoder[UUID] = {
    case StringValue(value) =>
      Try(UUID.fromString(value)).toEither.left
        .map(ex => DecodingError(s"Can't build a UUID from input $value", Some(ex)))
    case other => Left(DecodingError(s"Can't build a UUID from input $other"))
  }
  implicit val unit: ScalarDecoder[Unit] = {
    case ObjectValue(Nil) => Right(())
    case other            => Left(DecodingError(s"Can't build Unit from input $other"))
  }
}
