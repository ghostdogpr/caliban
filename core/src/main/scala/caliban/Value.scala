package caliban

import scala.util.Try
import caliban.Value._
import caliban.interop.circe._
import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }
import zio.Chunk
import zio.stream.Stream

import scala.annotation.switch

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

  implicit def playJsonWrites[F[_]: IsPlayJsonWrites]: F[InputValue] =
    ValuePlayJson.inputValueWrites.asInstanceOf[F[InputValue]]
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[InputValue] =
    ValuePlayJson.inputValueReads.asInstanceOf[F[InputValue]]

  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[InputValue] =
    ValueZIOJson.inputValueEncoder.asInstanceOf[F[InputValue]]
  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[InputValue] =
    ValueZIOJson.inputValueDecoder.asInstanceOf[F[InputValue]]
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

  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[ResponseValue] =
    ValueZIOJson.responseValueEncoder.asInstanceOf[F[ResponseValue]]
  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[ResponseValue] =
    ValueZIOJson.responseValueDecoder.asInstanceOf[F[ResponseValue]]

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

private object ValueZIOJson {
  import zio.json._
  import zio.json.internal.{ Lexer, RetractReader, Write }

  val valueEncoder: JsonEncoder[Value] = (a: Value, indent: Option[Int], out: Write) =>
    a match {
      case Value.NullValue => Null.encoder.unsafeEncode(NullValue, indent, out)
      case v: IntValue =>
        v match {
          case IntValue.IntNumber(value)    => JsonEncoder.int.unsafeEncode(value, indent, out)
          case IntValue.LongNumber(value)   => JsonEncoder.long.unsafeEncode(value, indent, out)
          case IntValue.BigIntNumber(value) => JsonEncoder.bigInteger.unsafeEncode(value.bigInteger, indent, out)
        }
      case v: FloatValue =>
        v match {
          case FloatValue.FloatNumber(value)      => JsonEncoder.float.unsafeEncode(value, indent, out)
          case FloatValue.DoubleNumber(value)     => JsonEncoder.double.unsafeEncode(value, indent, out)
          case FloatValue.BigDecimalNumber(value) => JsonEncoder.bigDecimal.unsafeEncode(value.bigDecimal, indent, out)
        }
      case StringValue(value)  => JsonEncoder.string.unsafeEncode(value, indent, out)
      case BooleanValue(value) => JsonEncoder.boolean.unsafeEncode(value, indent, out)
      case EnumValue(value)    => JsonEncoder.string.unsafeEncode(value, indent, out)
  }

  object Null {
    private[this] val nullChars: Array[Char] = "null".toCharArray
    val encoder: JsonEncoder[NullValue.type] = (a: NullValue.type, indent: Option[Int], out: Write) => out.write("null")
    implicit val decoder: JsonDecoder[NullValue.type] =
      (trace: List[JsonDecoder.JsonError], in: RetractReader) => {
        Lexer.readChars(trace, in, nullChars, "null")
        NullValue
      }
  }

  object Bool {
    implicit val decoder: JsonDecoder[BooleanValue] = JsonDecoder.boolean.map(BooleanValue)
  }

  object Obj {
    val decoder: JsonDecoder[InputValue.ObjectValue] = {
      val objd = JsonDecoder.keyValueChunk[String, InputValue]
      (trace: List[JsonDecoder.JsonError], in: RetractReader) =>
        InputValue.ObjectValue(objd.unsafeDecode(trace, in).toMap)
    }

    val responseDecoder: JsonDecoder[ResponseValue.ObjectValue] = {
      val objd = JsonDecoder.keyValueChunk[String, ResponseValue]
      (trace: List[JsonDecoder.JsonError], in: RetractReader) =>
        ResponseValue.ObjectValue(objd.unsafeDecode(trace, in).toList)
    }

    val responseEncoder: JsonEncoder[List[(String, ResponseValue)]] = {
      val obje = JsonEncoder.keyValueChunk[String, ResponseValue]
      (a: List[(String, ResponseValue)], indent: Option[Int], out: Write) =>
        obje.unsafeEncode(Chunk.fromIterable(a), indent, out)
    }
  }

  object Arr {
    val decoder: JsonDecoder[InputValue.ListValue] = {
      val arrd = JsonDecoder.list[InputValue]
      (trace: List[JsonDecoder.JsonError], in: RetractReader) =>
        InputValue.ListValue(arrd.unsafeDecode(trace, in))
    }

    val responseDecoder: JsonDecoder[ResponseValue.ListValue] = {
      val arrd = JsonDecoder.list[ResponseValue]
      (trace: List[JsonDecoder.JsonError], in: RetractReader) =>
        ResponseValue.ListValue(arrd.unsafeDecode(trace, in))
    }

    val responseEncoder: JsonEncoder[List[ResponseValue]] =
      JsonEncoder.list[ResponseValue]

  }

  object Str {
    val decoder: JsonDecoder[StringValue] = JsonDecoder.string.map(StringValue)
  }

  object Num {
    val decoder: JsonDecoder[Value] = (trace: List[JsonDecoder.JsonError], in: RetractReader) => {
      val bd = BigDecimal(JsonDecoder.bigDecimal.unsafeDecode(trace, in))
      if (bd.isValidInt) IntValue(bd.toIntExact)
      else if (bd.isValidLong) IntValue(bd.toLongExact)
      else bd.toBigIntExact.fold[Value](FloatValue(bd))(IntValue(_))
    }
  }

  implicit val inputValueDecoder: JsonDecoder[InputValue] = (trace: List[JsonDecoder.JsonError], in: RetractReader) => {
    val c = in.nextNonWhitespace()
    in.retract()
    (c: @switch) match {
      case 'n'       => Null.decoder.unsafeDecode(trace, in)
      case 'f' | 't' => Bool.decoder.unsafeDecode(trace, in)
      case '{'       => Obj.decoder.unsafeDecode(trace, in)
      case '['       => Arr.decoder.unsafeDecode(trace, in)
      case '"'       => Str.decoder.unsafeDecode(trace, in)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        Num.decoder.unsafeDecode(trace, in)
      case c =>
        throw JsonDecoder.UnsafeJson(JsonDecoder.JsonError.Message(s"unexpected '$c'") :: trace)
    }

  }

  val inputValueEncoder: JsonEncoder[InputValue] = (a: InputValue, indent: Option[Int], out: Write) =>
    a match {
      case value: Value                 => valueEncoder.unsafeEncode(value, indent, out)
      case InputValue.ListValue(values) => JsonEncoder.list(inputValueEncoder).unsafeEncode(values, indent, out)
      case InputValue.ObjectValue(fields) =>
        JsonEncoder.map(JsonFieldEncoder.string, inputValueEncoder).unsafeEncode(fields, indent, out)
      case InputValue.VariableValue(name) => JsonEncoder.string.unsafeEncode(name, indent, out)
  }

  val responseValueDecoder: JsonDecoder[ResponseValue] = (trace: List[JsonDecoder.JsonError], in: RetractReader) => {
    val c = in.nextNonWhitespace()
    in.retract()
    (c: @switch) match {
      case 'n'       => Null.decoder.unsafeDecode(trace, in)
      case 'f' | 't' => Bool.decoder.unsafeDecode(trace, in)
      case '{'       => Obj.responseDecoder.unsafeDecode(trace, in)
      case '['       => Arr.responseDecoder.unsafeDecode(trace, in)
      case '"'       => Str.decoder.unsafeDecode(trace, in)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        Num.decoder.unsafeDecode(trace, in)
      case c =>
        throw JsonDecoder.UnsafeJson(JsonDecoder.JsonError.Message(s"unexpected '$c'") :: trace)
    }

  }

  implicit val responseValueEncoder: JsonEncoder[ResponseValue] =
    (a: ResponseValue, indent: Option[Int], out: Write) =>
      a match {
        case value: Value                      => valueEncoder.unsafeEncode(value, indent, out)
        case ResponseValue.ListValue(values)   => Arr.responseEncoder.unsafeEncode(values, indent, out)
        case ResponseValue.ObjectValue(fields) => Obj.responseEncoder.unsafeEncode(fields, indent, out)
        case s: ResponseValue.StreamValue      => JsonEncoder.string.unsafeEncode(s.toString, indent, out)
    }

}
