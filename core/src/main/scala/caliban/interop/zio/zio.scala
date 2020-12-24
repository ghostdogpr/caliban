package caliban.interop.zio

import caliban.{ CalibanError, GraphQLResponse, InputValue, ResponseValue, Value }
import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue }
import caliban.parsing.adt.LocationInfo
import zio.Chunk
import zio.json.{ JsonDecoder, JsonEncoder }

import scala.annotation.switch

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark circe dependency as optional and keep Encoders defined in the companion object.
 */
private[caliban] trait IsZIOJsonEncoder[F[_]]
private[caliban] object IsZIOJsonEncoder {
  implicit val isZIOJsonEncoder: IsZIOJsonEncoder[JsonEncoder] = null
}

private[caliban] trait IsZIOJsonDecoder[F[_]]
private[caliban] object IsZIOJsonDecoder {
  implicit val isZIOJsonDecoder: IsZIOJsonDecoder[JsonDecoder] = null
}

private[caliban] object ValueZIOJson {
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
      (trace: List[JsonDecoder.JsonError], in: RetractReader) => InputValue.ListValue(arrd.unsafeDecode(trace, in))
    }

    val responseDecoder: JsonDecoder[ResponseValue.ListValue] = {
      val arrd = JsonDecoder.list[ResponseValue]
      (trace: List[JsonDecoder.JsonError], in: RetractReader) => ResponseValue.ListValue(arrd.unsafeDecode(trace, in))
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

private[caliban] object ErrorZioJson {
  import zio.json._
  import zio.json.internal.Write

  private def locationToResponse(li: LocationInfo): ResponseValue =
    ResponseValue.ListValue(
      List(ResponseValue.ObjectValue(List("line" -> IntValue(li.line), "column" -> IntValue(li.column))))
    )

  private[caliban] def errorToResponseValue(e: CalibanError): ResponseValue =
    ResponseValue.ObjectValue(e match {
      case CalibanError.ParsingError(msg, locationInfo, _, extensions) =>
        List(
          "message"                                               -> StringValue(s"Parsing Error: $msg")
        ) ++ (extensions: Option[ResponseValue]).map("extensions" -> _) ++
          locationInfo.map(locationToResponse).map("locations"    -> _)
      case CalibanError.ValidationError(msg, _, locationInfo, extensions) =>
        List(
          "message"                                               -> StringValue(msg)
        ) ++ (extensions: Option[ResponseValue]).map("extensions" -> _) ++
          locationInfo.map(locationToResponse).map("locations"    -> _)
      case CalibanError.ExecutionError(msg, path, locationInfo, _, extensions) =>
        List(
          "message"                                               -> StringValue(msg)
        ) ++ (extensions: Option[ResponseValue]).map("extensions" -> _) ++
          locationInfo.map(locationToResponse).map("locations"    -> _) ++
          Some(path).collect {
            case p if p.nonEmpty =>
              "path" -> ResponseValue.ListValue(p.map {
                case Left(value)  => StringValue(value)
                case Right(value) => IntValue(value)
              })
          }
    })

  val errorValueEncoder: JsonEncoder[CalibanError] = (a: CalibanError, indent: Option[Int], out: Write) =>
    ValueZIOJson.responseValueEncoder.unsafeEncode(
      errorToResponseValue(a),
      indent,
      out
    )
}

private[caliban] object GraphQLResponseZioJson {
  import zio.json._
  import zio.json.internal.Write

  private def handleError(err: Any): ResponseValue =
    err match {
      case ce: CalibanError => ErrorZioJson.errorToResponseValue(ce)
      case _                => ResponseValue.ObjectValue(List("message" -> StringValue(err.toString)))
    }

  val graphQLResponseEncoder: JsonEncoder[GraphQLResponse[Any]] =
    (a: GraphQLResponse[Any], indent: Option[Int], out: Write) => {
      val responseEncoder = JsonEncoder.map[String, ResponseValue]
      a match {
        case GraphQLResponse(data, Nil, None) =>
          responseEncoder.unsafeEncode(Map("data" -> data), indent, out)
        case GraphQLResponse(data, Nil, Some(extensions)) =>
          responseEncoder.unsafeEncode(
            Map("data" -> data, "extension" -> extensions.asInstanceOf[ResponseValue]),
            indent,
            out
          )
        case GraphQLResponse(data, errors, None) =>
          responseEncoder.unsafeEncode(
            Map("data" -> data, "errors" -> ResponseValue.ListValue(errors.map(handleError))),
            indent,
            out
          )
        case GraphQLResponse(data, errors, Some(extensions)) =>
          responseEncoder.unsafeEncode(
            Map(
              "data"       -> data,
              "errors"     -> ResponseValue.ListValue(errors.map(handleError)),
              "extensions" -> extensions.asInstanceOf[ResponseValue]
            ),
            indent,
            out
          )
      }
    }
}
