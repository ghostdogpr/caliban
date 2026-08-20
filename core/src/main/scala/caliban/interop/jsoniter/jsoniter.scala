package caliban.interop.jsoniter

import caliban.Value._
import caliban._
import caliban.parsing.adt.LocationInfo
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

import scala.collection.immutable.TreeMap

/**
 *  Implementation of the custom decoders ported from the jsoniter-circe implementation:
 *
 *  https://github.com/plokhotnyuk/jsoniter-scala/blob/master/jsoniter-scala-circe/shared/src/main/scala/io/circe/JsoniterScalaCodec.scala
 *
 *  NOTE: The encoders / decoders rely on a stack-recursive implementation. To prevent stack-overflow errors,
 *  the maximum recursion depth is limited to 512. For most use cases, this should be far more than enough.
 *
 *  If your schema allows for infinite recursion and it's not possible to limit the max depth below 512 (using the
 *  `maxDepth` wrapper), prefer using one of the other codecs
 */
private[caliban] object ValueJsoniter {
  implicit val stringListCodec: JsonValueCodec[Map[String, List[String]]] = JsonCodecMaker.make

  private val emptyInputList      = InputValue.ListValue(Nil)
  private val emptyInputObject    = InputValue.ObjectValue(Map.empty)
  private val emptyResponseList   = ResponseValue.ListValue(Nil)
  private val emptyResponseObject = ResponseValue.ObjectValue(Nil)

  // NOTE: We don't encode input values as JSON objects, but we need to have this logic so that we can create the codec
  private def encodeInputValue(x: InputValue, out: JsonWriter): Unit = x match {
    case v: StringValue                 => out.writeVal(v.value)
    case v: BooleanValue                => out.writeVal(v.value)
    case v: IntValue.IntNumber          => out.writeVal(v.value)
    case NullValue                      => out.writeNull()
    case v: EnumValue                   => out.writeVal(v.value)
    case v: InputValue.ObjectValue      => writeInputObject(v.fields, out)
    case v: InputValue.ListValue        => writeInputArray(v.values, out)
    case v: IntValue.LongNumber         => out.writeVal(v.value)
    case v: IntValue.BigIntNumber       => out.writeVal(v.value)
    case v: FloatValue.FloatNumber      => out.writeVal(v.value)
    case v: FloatValue.DoubleNumber     => out.writeVal(v.value)
    case v: FloatValue.BigDecimalNumber => out.writeVal(v.value)
    case v: InputValue.VariableValue    => out.writeVal(v.name)
  }

  private def writeInputArray(l: List[InputValue], out: JsonWriter): Unit = {
    out.writeArrayStart()
    var remaining = l
    while (remaining ne Nil) {
      val head = remaining.head
      remaining = remaining.tail
      encodeInputValue(head, out)
    }
    out.writeArrayEnd()
  }

  private def writeInputObject(m: Map[String, InputValue], out: JsonWriter): Unit = {
    out.writeObjectStart()
    val iter = m.iterator
    while (iter.hasNext) {
      val kv = iter.next()
      out.writeKey(kv._1)
      encodeInputValue(kv._2, out)
    }
    out.writeObjectEnd()
  }

  /**
   * Note on performance:
   *
   * We keep the number of cases low (less than 12) for the most commonly used types and avoid using unapply methods in the pattern-matching.
   * To keep the number of cases below this value, we handle the `FloatValue` type separately.
   */
  private def encodeResponseValue(x: ResponseValue, out: JsonWriter): Unit = x match {
    case v: StringValue               => out.writeVal(v.value)
    case v: BooleanValue              => out.writeVal(v.value)
    case v: IntValue.IntNumber        => out.writeVal(v.value)
    case v: IntValue.LongNumber       => out.writeVal(v.value)
    case v: ResponseValue.ObjectValue => writeResponseObject(v.fields, out)
    case v: ResponseValue.ListValue   => writeResponseArray(v.values, out)
    case NullValue                    => out.writeNull()
    case v: EnumValue                 => out.writeVal(v.value)
    case v: FloatValue.DoubleNumber   => out.writeVal(v.value)
    case v: IntValue.BigIntNumber     => out.writeVal(v.value)
    case v: FloatValue                => encodeFloatValue(v, out)
    case v: ResponseValue.StreamValue => out.writeVal(v.toString)
  }

  private def encodeFloatValue(x: FloatValue, out: JsonWriter): Unit = x match {
    case v: FloatValue.FloatNumber      => out.writeVal(v.value)
    case v: FloatValue.BigDecimalNumber => out.writeVal(v.value)
    case v: FloatValue.DoubleNumber     => out.writeVal(v.value)
  }

  private def writeResponseArray(l: List[ResponseValue], out: JsonWriter): Unit = {
    out.writeArrayStart()
    var remaining = l
    while (remaining ne Nil) {
      val head = remaining.head
      remaining = remaining.tail
      encodeResponseValue(head, out)
    }
    out.writeArrayEnd()
  }

  private def writeResponseObject(l: List[(String, ResponseValue)], out: JsonWriter): Unit = {
    out.writeObjectStart()
    var remaining = l
    while (remaining ne Nil) {
      val kv = remaining.head
      remaining = remaining.tail
      out.writeKey(kv._1)
      encodeResponseValue(kv._2, out)
    }
    out.writeObjectEnd()
  }

  private def decodeInputValue(in: JsonReader): InputValue =
    in.nextToken() match {
      case '"'                                     =>
        in.rollbackToken()
        StringValue(in.readString(null))
      case x if x == '-' || (x >= '0' && x <= '9') =>
        in.rollbackToken()
        numberParser(in)
      case 'n'                                     =>
        in.readNullOrError(NullValue, "unexpected JSON value")
      case 'f' | 't'                               =>
        in.rollbackToken()
        BooleanValue(in.readBoolean())
      case '{'                                     =>
        if (in.isNextToken('}')) emptyInputObject
        else {
          in.rollbackToken()
          /*
            Using a TreeMap to prevent DoS exploitation of the HashMap keys in Scala 2.12. We could potentially make
            this Scala version specific, but might be unnecessary given the Input objects are most of the time very
            small (extensions and variables). More info see: https://github.com/scala/bug/issues/11203
           */
          val x = TreeMap.newBuilder[String, InputValue]
          while ({
            x += (in.readKeyAsString() -> decodeInputValue(in))
            in.isNextToken(',')
          }) ()
          if (in.isCurrentToken('}')) InputValue.ObjectValue(x.result())
          else in.objectEndOrCommaError()
        }
      case '['                                     =>
        if (in.isNextToken(']')) emptyInputList
        else {
          in.rollbackToken()
          val x = List.newBuilder[InputValue]
          while ({
            x += decodeInputValue(in)
            in.isNextToken(',')
          }) ()
          if (in.isCurrentToken(']')) InputValue.ListValue(x.result())
          else in.arrayEndOrCommaError()
        }
      case c                                       =>
        in.decodeError(s"unexpected token $c")
    }

  private def decodeResponseValue(in: JsonReader): ResponseValue =
    in.nextToken() match {
      case '"'                                     =>
        in.rollbackToken()
        StringValue(in.readString(null))
      case x if x == '-' || (x >= '0' && x <= '9') =>
        in.rollbackToken()
        numberParser(in)
      case 'n'                                     =>
        in.readNullOrError(NullValue, "unexpected JSON value")
      case 'f' | 't'                               =>
        in.rollbackToken()
        BooleanValue(in.readBoolean())
      case '{'                                     =>
        if (in.isNextToken('}')) emptyResponseObject
        else {
          in.rollbackToken()
          val x = List.newBuilder[(String, ResponseValue)]
          while ({
            x += (in.readKeyAsString() -> decodeResponseValue(in))
            in.isNextToken(',')
          }) ()
          if (in.isCurrentToken('}')) ResponseValue.ObjectValue(x.result())
          else in.objectEndOrCommaError()
        }
      case '['                                     =>
        if (in.isNextToken(']')) emptyResponseList
        else {
          in.rollbackToken()
          val x = List.newBuilder[ResponseValue]
          while ({
            x += decodeResponseValue(in)
            in.isNextToken(',')
          }) ()
          if (in.isCurrentToken(']')) ResponseValue.ListValue(x.result())
          else in.arrayEndOrCommaError()
        }
      case c                                       =>
        in.decodeError(s"unexpected token $c")
    }

  private val numberParser: JsonReader => Value = in => {
    in.setMark()
    var digits = 0
    var b      = in.nextByte()
    if (b == '-') b = in.nextByte()
    try
      while (b >= '0' && b <= '9') {
        digits += 1
        b = in.nextByte()
      }
    catch {
      case _: JsonReaderException => // ignore the end of input error for now
    }
    in.rollbackToMark()

    if ((b | 0x20) != 'e' && b != '.') {
      if (digits < 19) {
        if (digits < 10) Value.IntValue.IntNumber(in.readInt())
        else Value.IntValue.LongNumber(in.readLong())
      } else {
        val x = in.readBigInt(null)
        if (x.bitLength < 64) Value.IntValue.LongNumber(x.longValue)
        else Value.IntValue.BigIntNumber(x.bigInteger)
      }
    } else Value.FloatValue.BigDecimalNumber(in.readBigDecimal(null).bigDecimal)
  }

  val inputValueCodec: JsonValueCodec[InputValue] = new JsonValueCodec[InputValue] {
    override def decodeValue(in: JsonReader, default: InputValue): InputValue =
      try decodeInputValue(in)
      catch { case _: StackOverflowError => in.decodeError("depth limit exceeded") }
    override def encodeValue(x: InputValue, out: JsonWriter): Unit            =
      try encodeInputValue(x, out)
      catch { case _: StackOverflowError => out.encodeError("depth limit exceeded") }
    override def nullValue: InputValue                                        = NullValue
  }

  val responseValueCodec: JsonValueCodec[ResponseValue] = new JsonValueCodec[ResponseValue] {
    override def decodeValue(in: JsonReader, default: ResponseValue): ResponseValue =
      try decodeResponseValue(in)
      catch { case _: StackOverflowError => in.decodeError("depth limit exceeded") }
    override def encodeValue(x: ResponseValue, out: JsonWriter): Unit               =
      try encodeResponseValue(x, out)
      catch { case _: StackOverflowError => out.encodeError("depth limit exceeded") }
    override def nullValue: ResponseValue                                           = NullValue
  }
}

private[caliban] object ErrorJsoniter {
  val errorValueCodec: JsonValueCodec[CalibanError] = new JsonValueCodec[CalibanError] {
    override def decodeValue(in: JsonReader, default: CalibanError): CalibanError = {
      val value = ValueJsoniter.responseValueCodec.decodeValue(in, null)
      CalibanError.fromResponseValue(value).getOrElse(in.decodeError("invalid GraphQL error"))
    }
    override def encodeValue(x: CalibanError, out: JsonWriter): Unit              =
      ValueJsoniter.responseValueCodec.encodeValue(x.toResponseValue, out)
    override def nullValue: CalibanError                                          =
      null.asInstanceOf[CalibanError]
  }
}

private[caliban] object GraphQLResponseJsoniter {
  val graphQLResponseCodec: JsonValueCodec[GraphQLResponse[Any]] =
    new JsonValueCodec[GraphQLResponse[Any]] {
      override def decodeValue(
        in: JsonReader,
        default: GraphQLResponse[Any]
      ): GraphQLResponse[Any] = {
        import GraphQLResponse.ResponseField

        if (!in.isNextToken('{')) in.decodeError("expected JSON object")

        var data: ResponseField[ResponseValue]            = ResponseField.Missing
        var errors: ResponseField[List[CalibanError]]     = ResponseField.Missing
        var extensions: Option[ResponseValue.ObjectValue] = None
        var hasNext: Option[Boolean]                      = None

        if (!in.isNextToken('}')) {
          in.rollbackToken()
          while ({
            in.readKeyAsString() match {
              case "data"       => data = ResponseField.Present(ValueJsoniter.responseValueCodec.decodeValue(in, null))
              case "errors"     =>
                ValueJsoniter.responseValueCodec.decodeValue(in, null) match {
                  case ResponseValue.ListValue(values) =>
                    GraphQLResponse.decodeErrors(values) match {
                      case Some(decoded) => errors = ResponseField.Present(decoded)
                      case None          => in.decodeError("invalid GraphQL error")
                    }
                  case _                               => in.decodeError("expected JSON array")
                }
              case "extensions" =>
                ValueJsoniter.responseValueCodec.decodeValue(in, null) match {
                  case value: ResponseValue.ObjectValue => extensions = Some(value)
                  case _                                => in.decodeError("expected JSON object")
                }
              case "hasNext"    => hasNext = Some(in.readBoolean())
              case _            => in.skip()
            }
            in.isNextToken(',')
          }) ()
          if (!in.isCurrentToken('}')) in.objectEndOrCommaError()
        }

        GraphQLResponse
          .fromDecoded(data, errors, extensions, hasNext)
          .getOrElse(in.decodeError("invalid GraphQL response"))
      }
      override def encodeValue(x: GraphQLResponse[Any], out: JsonWriter): Unit =
        ValueJsoniter.responseValueCodec.encodeValue(x.toResponseValue, out)
      override def nullValue: GraphQLResponse[Any]                             =
        null.asInstanceOf[GraphQLResponse[Any]]
    }
}
