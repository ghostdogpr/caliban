package caliban.client

import caliban.ResponseValue
import caliban.Value.{ BooleanValue, IntValue, StringValue }

trait ScalarDecoder[+A] {
  def decode(value: ResponseValue): Either[String, A]
}

object ScalarDecoder {
  implicit val int: ScalarDecoder[Int] = {
    case value: IntValue => Right(value.toInt)
    case other           => Left(s"Can't build an Int from input $other")
  }
  implicit val string: ScalarDecoder[String] = {
    case StringValue(value) => Right(value)
    case other              => Left(s"Can't build a String from input $other")
  }
  implicit val boolean: ScalarDecoder[Boolean] = {
    case BooleanValue(value) => Right(value)
    case other               => Left(s"Can't build a Boolean from input $other")
  }
}
