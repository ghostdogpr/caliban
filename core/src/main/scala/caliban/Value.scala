package caliban

import scala.util.Try
import caliban.Value._
import caliban.interop.circe._
import caliban.interop.jsoniter._
import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }
import zio.stream.Stream

sealed trait InputValue
object InputValue {
  case class ListValue(values: List[InputValue]) extends InputValue {
    override def toString: String = values.mkString("[", ",", "]")
  }
  case class ObjectValue(fields: Map[String, InputValue]) extends InputValue {
    override def toString: String =
      fields.map { case (name, value) => s""""$name":${value.toString}""" }.mkString("{", ",", "}")
  }
  case class VariableValue(name: String) extends InputValue {
    override def toString: String = s"$$$name"
  }

  implicit def circeEncoder[F[_]: IsCirceEncoder]: F[InputValue] =
    ValueCirce.inputValueEncoder.asInstanceOf[F[InputValue]]
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[InputValue] =
    ValueCirce.inputValueDecoder.asInstanceOf[F[InputValue]]

  implicit def jsoniterCodec[F[_]: IsJsoniterCodec]: F[InputValue] =
    ValueJsoniter.inputValueCodec.asInstanceOf[F[InputValue]]

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

  implicit def jsoniterCodec[F[_]: IsJsoniterCodec]: F[ResponseValue] =
    ValueJsoniter.responseValueCodec.asInstanceOf[F[ResponseValue]]

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
    override def toString: String = s""""${value.replace("\"", "\\\"").replace("\n", "\\n")}""""
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

private object ValueJsoniter {
  import com.github.plokhotnyuk.jsoniter_scala.core._
  import com.github.plokhotnyuk.jsoniter_scala.macros._
  import scala.collection.mutable

  implicit val streamValueCodec: JsonValueCodec[ResponseValue.StreamValue] =
    new JsonValueCodec[ResponseValue.StreamValue] {
      def decodeValue(in: JsonReader, default: ResponseValue.StreamValue): ResponseValue.StreamValue =
        default // TODO should I create a decoder for stream values?
      def encodeValue(x: ResponseValue.StreamValue, out: JsonWriter): Unit =
        out.writeVal(x.toString)
      def nullValue: ResponseValue.StreamValue =
        null
    }

  implicit val booleanValueCodec: JsonValueCodec[Value.BooleanValue] =
    new JsonValueCodec[Value.BooleanValue] {
      def decodeValue(in: JsonReader, default: Value.BooleanValue): Value.BooleanValue =
        Value.BooleanValue(in.readBoolean())
      def encodeValue(x: Value.BooleanValue, out: JsonWriter): Unit =
        out.writeVal(x.value)
      def nullValue: Value.BooleanValue =
        null
    }

  implicit val intNumberCodec: JsonValueCodec[Value.IntValue.IntNumber] =
    new JsonValueCodec[Value.IntValue.IntNumber] {
      def decodeValue(in: JsonReader, default: Value.IntValue.IntNumber): Value.IntValue.IntNumber =
        Value.IntValue.IntNumber(in.readInt())
      def encodeValue(x: Value.IntValue.IntNumber, out: JsonWriter): Unit =
        out.writeVal(x.value)
      def nullValue: Value.IntValue.IntNumber =
        null
    }

  implicit val bigIntNumberCodec: JsonValueCodec[Value.IntValue.BigIntNumber] =
    new JsonValueCodec[Value.IntValue.BigIntNumber] {
      def decodeValue(in: JsonReader, default: Value.IntValue.BigIntNumber): Value.IntValue.BigIntNumber =
        Value.IntValue.BigIntNumber(in.readBigInt(null))
      def encodeValue(x: Value.IntValue.BigIntNumber, out: JsonWriter): Unit =
        out.writeVal(x.value)
      def nullValue: Value.IntValue.BigIntNumber =
        null
    }

  implicit val longNumberCodec: JsonValueCodec[Value.IntValue.LongNumber] =
    new JsonValueCodec[Value.IntValue.LongNumber] {
      def decodeValue(in: JsonReader, default: Value.IntValue.LongNumber): Value.IntValue.LongNumber =
        Value.IntValue.LongNumber(in.readLong())
      def encodeValue(x: Value.IntValue.LongNumber, out: JsonWriter): Unit =
        out.writeVal(x.value)
      def nullValue: Value.IntValue.LongNumber =
        null
    }

  implicit val doubleNumberCodec: JsonValueCodec[Value.FloatValue.DoubleNumber] =
    new JsonValueCodec[Value.FloatValue.DoubleNumber] {
      def decodeValue(in: JsonReader, default: Value.FloatValue.DoubleNumber): Value.FloatValue.DoubleNumber =
        Value.FloatValue.DoubleNumber(in.readDouble())
      def encodeValue(x: Value.FloatValue.DoubleNumber, out: JsonWriter): Unit =
        out.writeVal(x.value)
      def nullValue: Value.FloatValue.DoubleNumber =
        null
    }

  implicit val floatNumberCodec: JsonValueCodec[Value.FloatValue.FloatNumber] =
    new JsonValueCodec[Value.FloatValue.FloatNumber] {
      def decodeValue(in: JsonReader, default: Value.FloatValue.FloatNumber): Value.FloatValue.FloatNumber =
        Value.FloatValue.FloatNumber(in.readFloat())
      def encodeValue(x: Value.FloatValue.FloatNumber, out: JsonWriter): Unit =
        out.writeVal(x.value)
      def nullValue: Value.FloatValue.FloatNumber =
        null
    }

  implicit val bigDecimalNumberCodec: JsonValueCodec[Value.FloatValue.BigDecimalNumber] =
    new JsonValueCodec[Value.FloatValue.BigDecimalNumber] {
      def decodeValue(in: JsonReader, default: Value.FloatValue.BigDecimalNumber): Value.FloatValue.BigDecimalNumber =
        Value.FloatValue.BigDecimalNumber(in.readBigDecimal(null))
      def encodeValue(x: Value.FloatValue.BigDecimalNumber, out: JsonWriter): Unit =
        out.writeVal(x.value)
      def nullValue: Value.FloatValue.BigDecimalNumber =
        null
    }

  implicit val stringValueCodec: JsonValueCodec[Value.StringValue] =
    new JsonValueCodec[Value.StringValue] {
      def decodeValue(in: JsonReader, default: Value.StringValue): Value.StringValue =
        Value.StringValue(in.readString(null))
      def encodeValue(x: Value.StringValue, out: JsonWriter): Unit =
        out.writeVal(x.value)
      def nullValue: Value.StringValue =
        null
    }

  implicit val enumValueCodec: JsonValueCodec[Value.EnumValue] =
    new JsonValueCodec[Value.EnumValue] {
      def decodeValue(in: JsonReader, default: Value.EnumValue): Value.EnumValue =
        Value.EnumValue(in.readString(null))
      def encodeValue(x: Value.EnumValue, out: JsonWriter): Unit =
        out.writeVal(x.value)
      def nullValue: Value.EnumValue =
        null
    }

  implicit val listValueCodec: JsonValueCodec[ResponseValue.ListValue] =
    new JsonValueCodec[ResponseValue.ListValue] {
      def decodeValue(in: JsonReader, default: ResponseValue.ListValue): ResponseValue.ListValue =
        if (in.isNextToken('[')) { // TODO test this
          val values = List.newBuilder[ResponseValue]
          if (!in.isNextToken(']')) {
            in.rollbackToken()
            while ({
              values += responseValueCodec.decodeValue(in, responseValueCodec.nullValue)
              in.isNextToken(',')
            })()
          }
          ResponseValue.ListValue(values.result)
        } else {
          in.readNullOrTokenError(default, '[')
        }
      def encodeValue(x: ResponseValue.ListValue, out: JsonWriter): Unit = {
        out.writeArrayStart()
        x.values.foreach(x1 => responseValueCodec.encodeValue(x1, out))
        out.writeArrayEnd()
      }
      def nullValue: ResponseValue.ListValue =
        null
    }

  implicit val objectValueCodec: JsonValueCodec[ResponseValue.ObjectValue] =
    new JsonValueCodec[ResponseValue.ObjectValue] {
      def decodeValue(in: JsonReader, default: ResponseValue.ObjectValue): ResponseValue.ObjectValue =
        if (in.isNextToken('{')) { // TODO test this
          val values: mutable.Buffer[(String, ResponseValue)] = mutable.ListBuffer[(String, ResponseValue)]()
          if (!in.isNextToken('}')) {
            in.rollbackToken()
            while({
              values.append((in.readKeyAsString(), responseValueCodec.decodeValue(in, responseValueCodec.nullValue)))
              in.isNextToken(',')
            })()
          }
          ResponseValue.ObjectValue(values.toList)
        } else {
          in.readNullOrTokenError(default, '{')
        }
      def encodeValue(x: ResponseValue.ObjectValue, out: JsonWriter): Unit = {
        out.writeObjectStart()
        x.fields.foreach {
          case (k, v) =>
            out.writeKey(k)
            responseValueCodec.encodeValue(v, out)
        }
        out.writeObjectEnd()
      }
      def nullValue: ResponseValue.ObjectValue =
        null
    }

  val inputValueCodec: JsonValueCodec[InputValue] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  val responseValueCodec: JsonValueCodec[ResponseValue] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
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
