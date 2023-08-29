package caliban.interop.jsoniter

import caliban.Value._
import caliban._
import caliban.parsing.adt.LocationInfo
import com.github.plokhotnyuk.jsoniter_scala.core._

import scala.annotation.switch
import scala.collection.immutable.TreeMap

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark jsoniter dependency as optional and keep Encoders defined in the companion object.
 */
private[caliban] trait IsJsoniterCodec[F[_]]
private[caliban] object IsJsoniterCodec {
  implicit val isJsoniterCodec: IsJsoniterCodec[JsonValueCodec] = null
}

/**
 *  Implementation of the custom decoders ported from the jsoniter-circe implementation:
 *
 *  https://github.com/plokhotnyuk/jsoniter-scala/blob/master/jsoniter-scala-circe/shared/src/main/scala/io/circe/JsoniterScalaCodec.scala
 *
 *  NOTE: The encoders / decoders rely on a stack-recursive implementation. To prevent stack-overflow errors,
 *  the maximum recursion depth is limited to 512. For most usecases, this should be far more than enough.
 *
 *  If your schema allows for infinite recursion and it's not possible to limit the max depth below 512 (using the
 *  `maxDepth` wrapper), prefer using one of the other codecs
 */
private[caliban] object ValueJsoniter {

  final private val maxDepth = 512

  private val emptyInputList      = InputValue.ListValue(Nil)
  private val emptyInputObject    = InputValue.ObjectValue(Map.empty)
  private val emptyResponseList   = ResponseValue.ListValue(Nil)
  private val emptyResponseObject = ResponseValue.ObjectValue(Nil)

  private def encodeInputValue(x: InputValue, out: JsonWriter, depth: Int): Unit = x match {
    case StringValue(value)                 => out.writeVal(value)
    case BooleanValue(value)                => out.writeVal(value)
    case IntValue.IntNumber(value)          => out.writeVal(value)
    case IntValue.LongNumber(value)         => out.writeVal(value)
    case IntValue.BigIntNumber(value)       => out.writeVal(value)
    case FloatValue.FloatNumber(value)      => out.writeVal(value)
    case FloatValue.DoubleNumber(value)     => out.writeVal(value)
    case FloatValue.BigDecimalNumber(value) => out.writeVal(value)
    case NullValue                          => out.writeNull()
    case EnumValue(value)                   => out.writeVal(value)
    case InputValue.ListValue(l)            =>
      val depthM1 = depth - 1
      if (depthM1 < 0) out.encodeError("depth limit exceeded")
      out.writeArrayStart()
      l.foreach(v => encodeInputValue(v, out, depthM1))
      out.writeArrayEnd()
    case InputValue.ObjectValue(o)          =>
      val depthM1 = depth - 1
      if (depthM1 < 0) out.encodeError("depth limit exceeded")
      out.writeObjectStart()
      o.foreach { case (k, v) =>
        out.writeKey(k)
        encodeInputValue(v, out, depthM1)
      }
      out.writeObjectEnd()
    case InputValue.VariableValue(v)        => out.writeVal(v)
  }

  private def encodeResponseValue(x: ResponseValue, out: JsonWriter, depth: Int): Unit = x match {
    case StringValue(value)                 => out.writeVal(value)
    case BooleanValue(value)                => out.writeVal(value)
    case IntValue.IntNumber(value)          => out.writeVal(value)
    case IntValue.LongNumber(value)         => out.writeVal(value)
    case IntValue.BigIntNumber(value)       => out.writeVal(value)
    case FloatValue.FloatNumber(value)      => out.writeVal(value)
    case FloatValue.DoubleNumber(value)     => out.writeVal(value)
    case FloatValue.BigDecimalNumber(value) => out.writeVal(value)
    case NullValue                          => out.writeNull()
    case EnumValue(value)                   => out.writeVal(value)
    case ResponseValue.ListValue(l)         =>
      val depthM1 = depth - 1
      if (depthM1 < 0) out.encodeError("depth limit exceeded")
      out.writeArrayStart()
      l.foreach(v => encodeResponseValue(v, out, depthM1))
      out.writeArrayEnd()
    case ResponseValue.ObjectValue(o)       =>
      val depthM1 = depth - 1
      if (depthM1 < 0) out.encodeError("depth limit exceeded")
      out.writeObjectStart()
      o.foreach { case (k, v) =>
        out.writeKey(k)
        encodeResponseValue(v, out, depthM1)
      }
      out.writeObjectEnd()
    case s: ResponseValue.StreamValue       => out.writeVal(s.toString)
  }

  private def decodeInputValue(in: JsonReader, depth: Int): InputValue =
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
        val depthM1 = depth - 1
        if (depthM1 < 0) in.decodeError("depth limit exceeded")
        else if (in.isNextToken('}')) emptyInputObject
        else {
          in.rollbackToken()
          /*
            Using a TreeMap to prevent DoS explotation of the HashMap keys in Scala 2.12. We could potentially make
            this Scala version specific, but might be unnecessary given the Input objects are most of the time very
            small (extensions and variables). More info see: https://github.com/scala/bug/issues/11203
           */
          val x = TreeMap.newBuilder[String, InputValue]
          while ({
            x += (in.readKeyAsString() -> decodeInputValue(in, depthM1))
            in.isNextToken(',')
          }) ()
          if (in.isCurrentToken('}')) InputValue.ObjectValue(x.result())
          else in.objectEndOrCommaError()
        }
      case '['                                     =>
        val depthM1 = depth - 1
        if (depthM1 < 0) in.decodeError("depth limit exceeded")
        else if (in.isNextToken(']')) emptyInputList
        else {
          in.rollbackToken()
          val x = List.newBuilder[InputValue]
          while ({
            x += decodeInputValue(in, depthM1)
            in.isNextToken(',')
          }) ()
          if (in.isCurrentToken(']')) InputValue.ListValue(x.result())
          else in.arrayEndOrCommaError()
        }
      case c                                       =>
        in.decodeError(s"unexpected token $c")
    }

  private def decodeResponseValue(in: JsonReader, depth: Int): ResponseValue =
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
        val depthM1 = depth - 1
        if (depthM1 < 0) in.decodeError("depth limit exceeded")
        else if (in.isNextToken('}')) emptyResponseObject
        else {
          in.rollbackToken()
          val x = List.newBuilder[(String, ResponseValue)]
          while ({
            x += (in.readKeyAsString() -> decodeResponseValue(in, depthM1))
            in.isNextToken(',')
          }) ()
          if (in.isCurrentToken('}')) ResponseValue.ObjectValue(x.result())
          else in.objectEndOrCommaError()
        }
      case '['                                     =>
        val depthM1 = depth - 1
        if (depthM1 < 0) in.decodeError("depth limit exceeded")
        else if (in.isNextToken(']')) emptyResponseList
        else {
          in.rollbackToken()
          val x = List.newBuilder[ResponseValue]
          while ({
            x += decodeResponseValue(in, depthM1)
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
        b = in.nextByte()
        digits += 1
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

  implicit val inputValueCodec: JsonValueCodec[InputValue] = new JsonValueCodec[InputValue] {
    override def decodeValue(in: JsonReader, default: InputValue): InputValue = decodeInputValue(in, maxDepth)
    override def encodeValue(x: InputValue, out: JsonWriter): Unit            = encodeInputValue(x, out, maxDepth)
    override def nullValue: InputValue                                        = NullValue
  }

  implicit val responseValueCodec: JsonValueCodec[ResponseValue] = new JsonValueCodec[ResponseValue] {
    override def decodeValue(in: JsonReader, default: ResponseValue): ResponseValue = decodeResponseValue(in, maxDepth)
    override def encodeValue(x: ResponseValue, out: JsonWriter): Unit               = encodeResponseValue(x, out, maxDepth)
    override def nullValue: ResponseValue                                           = NullValue
  }
}

private[caliban] object ErrorJsoniter {
  import com.github.plokhotnyuk.jsoniter_scala.macros._

  private case class ErrorDTO(
    message: String,
    path: Option[List[Either[String, Int]]],
    locations: Option[List[LocationInfo]],
    extensions: Option[ResponseValue.ObjectValue]
  )

  private implicit val eitherCodec: JsonValueCodec[Either[String, Int]] = new JsonValueCodec[Either[String, Int]] {
    override def decodeValue(in: JsonReader, default: Either[String, Int]): Either[String, Int] = {
      val b = in.nextToken()
      in.rollbackToken()
      (b: @switch) match {
        case '"'                                                             => Left(in.readString(null))
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => Right(in.readInt())
        case _                                                               => in.decodeError("expected int or string")
      }
    }
    override def encodeValue(x: Either[String, Int], out: JsonWriter): Unit                     =
      x.fold(out.writeVal, out.writeVal)
    override def nullValue: Either[String, Int]                                                 =
      null.asInstanceOf[Either[String, Int]]
  }

  private implicit val objectValueCodec: JsonValueCodec[ResponseValue.ObjectValue] =
    new JsonValueCodec[ResponseValue.ObjectValue] {
      override def decodeValue(in: JsonReader, default: ResponseValue.ObjectValue): ResponseValue.ObjectValue =
        ValueJsoniter.responseValueCodec.decodeValue(in, default) match {
          case o: ResponseValue.ObjectValue => o
          case _                            => in.decodeError("expected json object")
        }
      override def encodeValue(x: ResponseValue.ObjectValue, out: JsonWriter): Unit                           =
        ValueJsoniter.responseValueCodec.encodeValue(x, out)
      override def nullValue: ResponseValue.ObjectValue                                                       =
        null.asInstanceOf[ResponseValue.ObjectValue]
    }

  implicit val errorValueCodec: JsonValueCodec[CalibanError] = new JsonValueCodec[CalibanError] {
    private val dtoCodec: JsonValueCodec[ErrorDTO] = JsonCodecMaker.make

    override def decodeValue(in: JsonReader, default: CalibanError): CalibanError = {
      val err = dtoCodec.decodeValue(in, null)
      CalibanError.ExecutionError(
        msg = err.message,
        path = err.path.getOrElse(Nil),
        locationInfo = err.locations.flatMap(_.headOption),
        innerThrowable = None,
        extensions = err.extensions
      )
    }
    override def encodeValue(x: CalibanError, out: JsonWriter): Unit              =
      ValueJsoniter.responseValueCodec.encodeValue(x.toResponseValue, out)
    override def nullValue: CalibanError                                          =
      null.asInstanceOf[CalibanError]
  }
}

private[caliban] object GraphQLResponseJsoniter {
  import com.github.plokhotnyuk.jsoniter_scala.macros._

  private case class GraphQLResponseDTO(data: ResponseValue, errors: Option[List[CalibanError]])

  implicit val graphQLResponseCodec: JsonValueCodec[GraphQLResponse[CalibanError]] =
    new JsonValueCodec[GraphQLResponse[CalibanError]] {
      private val dtoCodec: JsonValueCodec[GraphQLResponseDTO] = JsonCodecMaker.make

      override def decodeValue(
        in: JsonReader,
        default: GraphQLResponse[CalibanError]
      ): GraphQLResponse[CalibanError] = {
        val resp = dtoCodec.decodeValue(in, null)
        GraphQLResponse[CalibanError](
          data = resp.data,
          errors = resp.errors.getOrElse(Nil),
          extensions = None
        )
      }
      override def encodeValue(x: GraphQLResponse[CalibanError], out: JsonWriter): Unit =
        ValueJsoniter.responseValueCodec.encodeValue(x.toResponseValue, out)
      override def nullValue: GraphQLResponse[CalibanError]                             =
        null.asInstanceOf[GraphQLResponse[CalibanError]]
    }
}

private[caliban] object GraphQLRequestJsoniter {
  import com.github.plokhotnyuk.jsoniter_scala.macros._

  implicit val graphQLRequestCodec: JsonValueCodec[GraphQLRequest] = JsonCodecMaker.make
}

private[caliban] object GraphQLWSInputJsoniter {
  import com.github.plokhotnyuk.jsoniter_scala.macros._

  implicit val graphQLWSInputCodec: JsonValueCodec[GraphQLWSInput] = JsonCodecMaker.make
}

private[caliban] object GraphQLWSOutputJsoniter {
  import com.github.plokhotnyuk.jsoniter_scala.macros._

  implicit val graphQLWSOuputCodec: JsonValueCodec[GraphQLWSOutput] = JsonCodecMaker.make
}
