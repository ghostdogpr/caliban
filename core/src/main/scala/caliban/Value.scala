package caliban

import caliban.Value._
import caliban.interop.circe._
import zio.stream.Stream

import scala.language.higherKinds
import scala.util.Try

sealed trait InputValue
sealed trait ResponseValue
sealed trait Value extends InputValue with ResponseValue

object Value {
  case object NullValue extends Value {
    override def toString: String = "null"
  }
  sealed trait IntValue extends Value {
    def toInt: Int
    def toLong: Long
    def toBigInt: BigInt
  }
  sealed trait FloatValue extends Value {
    def toFloat: Float
    def toDouble: Double
    def toBigDecimal: BigDecimal
  }
  case class StringValue(value: String) extends Value {
    override def toString: String = s""""${value.replace("\"", "\\\"")}""""
  }
  case class BooleanValue(value: Boolean) extends Value {
    override def toString: String = if (value) "true" else "false"
  }
  case class EnumValue(value: String) extends Value {
    override def toString: String = s""""${value.replace("\"", "\\\"")}""""
  }

  object IntValue {
    def apply(v: Int): IntValue    = IntNumber(v)
    def apply(v: Long): IntValue   = LongNumber(v)
    def apply(v: BigInt): IntValue = BigIntNumber(v)
    def apply(s: String): IntValue =
      Try(IntNumber(s.toInt)).orElse(Try(LongNumber(s.toLong))).getOrElse(BigIntNumber(BigInt(s)))

    case class IntNumber(value: Int) extends IntValue {
      override def toInt: Int       = value
      override def toLong: Long     = value.toLong
      override def toBigInt: BigInt = BigInt(value)
      override def toString: String = value.toString
    }
    case class LongNumber(value: Long) extends IntValue {
      override def toInt: Int       = value.toInt
      override def toLong: Long     = value
      override def toBigInt: BigInt = BigInt(value)
      override def toString: String = value.toString
    }
    case class BigIntNumber(value: BigInt) extends IntValue {
      override def toInt: Int       = value.toInt
      override def toLong: Long     = value.toLong
      override def toBigInt: BigInt = value
      override def toString: String = value.toString
    }
  }

  object FloatValue {
    def apply(v: Float): FloatValue      = FloatNumber(v)
    def apply(v: Double): FloatValue     = DoubleNumber(v)
    def apply(v: BigDecimal): FloatValue = BigDecimalNumber(v)
    def apply(s: String): FloatValue     = BigDecimalNumber(BigDecimal(s))

    case class FloatNumber(value: Float) extends FloatValue {
      override def toFloat: Float           = value
      override def toDouble: Double         = value.toDouble
      override def toBigDecimal: BigDecimal = BigDecimal.decimal(value)
      override def toString: String         = value.toString
    }
    case class DoubleNumber(value: Double) extends FloatValue {
      override def toFloat: Float           = value.toFloat
      override def toDouble: Double         = value
      override def toBigDecimal: BigDecimal = BigDecimal(value)
      override def toString: String         = value.toString
    }
    case class BigDecimalNumber(value: BigDecimal) extends FloatValue {
      override def toFloat: Float           = value.toFloat
      override def toDouble: Double         = value.toDouble
      override def toBigDecimal: BigDecimal = value
      override def toString: String         = value.toString
    }
  }
}

object InputValue {
  case class ListValue(values: List[InputValue])          extends InputValue
  case class ObjectValue(fields: Map[String, InputValue]) extends InputValue
  case class VariableValue(name: String)                  extends InputValue

  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[InputValue] =
    ValueCirce.inputValueDecoder.asInstanceOf[F[InputValue]]
}

object ResponseValue {
  case class ListValue(values: List[ResponseValue]) extends ResponseValue {
    override def toString: String = values.mkString("[", ",", "]")
  }
  case class ObjectValue(fields: List[(String, ResponseValue)]) extends ResponseValue {
    override def toString: String =
      fields.map { case (name, value) => s""""$name":${value.toString}""" }.mkString("{", ",", "}")
  }
  case class StreamValue(stream: Stream[Throwable, ResponseValue]) extends ResponseValue {
    override def toString: String = "<stream>"
  }

  implicit def circeEncoder[F[_]](implicit ev: IsCirceEncoder[F]): F[ResponseValue] =
    ValueCirce.responseValueEncoder.asInstanceOf[F[ResponseValue]]
}

private object ValueCirce {
  import io.circe._
  private def jsonToValue(json: Json): InputValue =
    json.fold(
      NullValue,
      BooleanValue,
      number =>
        number.toBigInt.map(IntValue.apply) orElse
          number.toBigDecimal.map(FloatValue.apply) getOrElse
          FloatValue(number.toDouble),
      StringValue,
      array => InputValue.ListValue(array.toList.map(jsonToValue)),
      obj => InputValue.ObjectValue(obj.toMap.map { case (k, v) => k -> jsonToValue(v) })
    )
  val inputValueDecoder: Decoder[InputValue] = Decoder.instance(hcursor => Right(jsonToValue(hcursor.value)))
  val responseValueEncoder: Encoder[ResponseValue] = Encoder
    .instance[ResponseValue]({
      case NullValue => Json.Null
      case v: IntValue =>
        v match {
          case IntValue.IntNumber(value)    => Json.fromInt(value)
          case IntValue.LongNumber(value)   => Json.fromLong(value)
          case IntValue.BigIntNumber(value) => Json.fromBigInt(value)
        }
      case v: FloatValue =>
        v match {
          case FloatValue.FloatNumber(value)      => Json.fromFloatOrNull(value)
          case FloatValue.DoubleNumber(value)     => Json.fromDoubleOrNull(value)
          case FloatValue.BigDecimalNumber(value) => Json.fromBigDecimal(value)
        }
      case StringValue(value)              => Json.fromString(value)
      case BooleanValue(value)             => Json.fromBoolean(value)
      case EnumValue(value)                => Json.fromString(value)
      case ResponseValue.ListValue(values) => Json.arr(values.map(responseValueEncoder.apply): _*)
      case ResponseValue.ObjectValue(fields) =>
        Json.obj(fields.map { case (k, v) => k -> responseValueEncoder.apply(v) }: _*)
      case s: ResponseValue.StreamValue => Json.fromString(s.toString)
    })
}
