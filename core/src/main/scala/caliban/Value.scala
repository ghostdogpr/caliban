package caliban

import scala.util.Try
import caliban.Value._
import caliban.interop.circe._
import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }
import zio.stream.Stream

sealed trait InputValue
object InputValue {
  case class ListValue(values: List[InputValue])          extends InputValue
  case class ObjectValue(fields: Map[String, InputValue]) extends InputValue
  case class VariableValue(name: String)                  extends InputValue

  implicit def circeEncoder[F[_]: IsCirceEncoder]: F[InputValue] =
    ValueCirce.inputValueEncoder.asInstanceOf[F[InputValue]]
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[InputValue] =
    ValueCirce.inputValueDecoder.asInstanceOf[F[InputValue]]

  implicit def playJsonWrites[F[_]: IsPlayJsonWrites]: F[InputValue] =
    ValuePlayJson.inputValueWrites.asInstanceOf[F[InputValue]]
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[InputValue] =
    ValuePlayJson.inputValueReads.asInstanceOf[F[InputValue]]
}

sealed trait ResponseValue
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

  implicit def circeEncoder[F[_]: IsCirceEncoder]: F[ResponseValue] =
    ValueCirce.responseValueEncoder.asInstanceOf[F[ResponseValue]]
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[ResponseValue] =
    ValueCirce.responseValueDecoder.asInstanceOf[F[ResponseValue]]

  implicit def playJsonWrites[F[_]: IsPlayJsonWrites]: F[ResponseValue] =
    ValuePlayJson.responseValueWrites.asInstanceOf[F[ResponseValue]]
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[ResponseValue] =
    ValuePlayJson.responseValueReads.asInstanceOf[F[ResponseValue]]
}

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
      Try(IntNumber(s.toInt)) orElse
        Try(LongNumber(s.toLong)) getOrElse
        BigIntNumber(BigInt(s))

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

private object ValueCirce {
  import io.circe._
  val valueEncoder: Encoder[Value] = Encoder
    .instance[Value]({
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
      case StringValue(value)  => Json.fromString(value)
      case BooleanValue(value) => Json.fromBoolean(value)
      case EnumValue(value)    => Json.fromString(value)
    })
  private def jsonToInputValue(json: Json): InputValue =
    json.fold(
      NullValue,
      BooleanValue,
      number =>
        number.toBigInt.map(IntValue.apply) orElse
          number.toBigDecimal.map(FloatValue.apply) getOrElse
          FloatValue(number.toDouble),
      StringValue,
      array => InputValue.ListValue(array.toList.map(jsonToInputValue)),
      obj => InputValue.ObjectValue(obj.toMap.map { case (k, v) => k -> jsonToInputValue(v) })
    )
  val inputValueDecoder: Decoder[InputValue] = Decoder.instance(hcursor => Right(jsonToInputValue(hcursor.value)))
  val inputValueEncoder: Encoder[InputValue] = Encoder
    .instance[InputValue]({
      case value: Value                 => valueEncoder.apply(value)
      case InputValue.ListValue(values) => Json.arr(values.map(inputValueEncoder.apply): _*)
      case InputValue.ObjectValue(fields) =>
        Json.obj(fields.map { case (k, v) => k -> inputValueEncoder.apply(v) }.toList: _*)
      case InputValue.VariableValue(name) => Json.fromString(name)
    })
  private def jsonToResponseValue(json: Json): ResponseValue =
    json.fold(
      NullValue,
      BooleanValue,
      number =>
        number.toBigInt.map(IntValue.apply) orElse
          number.toBigDecimal.map(FloatValue.apply) getOrElse
          FloatValue(number.toDouble),
      StringValue,
      array => ResponseValue.ListValue(array.toList.map(jsonToResponseValue)),
      obj => ResponseValue.ObjectValue(obj.toList.map { case (k, v) => k -> jsonToResponseValue(v) })
    )
  val responseValueDecoder: Decoder[ResponseValue] =
    Decoder.instance(hcursor => Right(jsonToResponseValue(hcursor.value)))
  val responseValueEncoder: Encoder[ResponseValue] = Encoder
    .instance[ResponseValue]({
      case value: Value                    => valueEncoder.apply(value)
      case ResponseValue.ListValue(values) => Json.arr(values.map(responseValueEncoder.apply): _*)
      case ResponseValue.ObjectValue(fields) =>
        Json.obj(fields.map { case (k, v) => k -> responseValueEncoder.apply(v) }: _*)
      case s: ResponseValue.StreamValue => Json.fromString(s.toString)
    })
}

private object ValuePlayJson {
  import play.api.libs.json._

  val valueWrites: Writes[Value] = Writes {
    case NullValue => JsNull
    case v: IntValue =>
      v match {
        case IntValue.IntNumber(value)    => JsNumber(BigDecimal(value))
        case IntValue.LongNumber(value)   => JsNumber(BigDecimal(value))
        case IntValue.BigIntNumber(value) => JsNumber(BigDecimal(value))
      }
    case v: FloatValue =>
      v match {
        case FloatValue.FloatNumber(value)      => JsNumber(BigDecimal(value.toDouble))
        case FloatValue.DoubleNumber(value)     => JsNumber(BigDecimal(value))
        case FloatValue.BigDecimalNumber(value) => JsNumber(value)
      }
    case StringValue(value)  => JsString(value)
    case BooleanValue(value) => JsBoolean(value)
    case EnumValue(value)    => JsString(value)
  }

  private def jsonToInputValue(json: JsValue): InputValue =
    json match {
      case JsObject(fields)  => InputValue.ObjectValue(fields.map { case (k, v) => k -> jsonToInputValue(v) }.toMap)
      case JsArray(elements) => InputValue.ListValue(elements.toList.map(jsonToInputValue))
      case JsString(value)   => StringValue(value)
      case JsNumber(value) =>
        Try(value.toIntExact)
          .map(IntValue.apply)
          .getOrElse(FloatValue(value))

      case b: JsBoolean => BooleanValue(b.value)
      case JsNull       => NullValue
    }

  val inputValueReads: Reads[InputValue] = Reads(json => JsSuccess(jsonToInputValue(json)))
  val inputValueWrites: Writes[InputValue] = Writes {
    case value: Value                 => valueWrites.writes(value)
    case InputValue.ListValue(values) => JsArray(values.map(inputValueWrites.writes))
    case InputValue.ObjectValue(fields) =>
      JsObject(fields.map { case (k, v) => k -> inputValueWrites.writes(v) })
    case InputValue.VariableValue(name) => JsString(name)
  }

  private def jsonToResponseValue(json: JsValue): ResponseValue =
    json match {
      case JsObject(fields) =>
        ResponseValue.ObjectValue(fields.map { case (k, v) => k -> jsonToResponseValue(v) }.toList)
      case JsArray(elements) => ResponseValue.ListValue(elements.toList.map(jsonToResponseValue))
      case JsString(value)   => StringValue(value)
      case JsNumber(value) =>
        Try(value.toIntExact)
          .map(IntValue.apply)
          .getOrElse(FloatValue(value))

      case b: JsBoolean => BooleanValue(b.value)
      case JsNull       => NullValue
    }

  val responseValueReads: Reads[ResponseValue] =
    Reads(json => JsSuccess(jsonToResponseValue(json)))

  val responseValueWrites: Writes[ResponseValue] = Writes {
    case value: Value                    => valueWrites.writes(value)
    case ResponseValue.ListValue(values) => JsArray(values.map(responseValueWrites.writes))
    case ResponseValue.ObjectValue(fields) =>
      JsObject(fields.map { case (k, v) => k -> responseValueWrites.writes(v) })
    case s: ResponseValue.StreamValue => JsString(s.toString)
  }
}
