package caliban.client

import caliban.client.ResponseValue._

trait ScalarDecoder[+A] {
  def decode(value: ResponseValue): Either[String, A]
}

object ScalarDecoder {
  implicit val int: ScalarDecoder[Int] = {
    case value: IntValue => Right(value.value)
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
