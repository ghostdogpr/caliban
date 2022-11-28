package caliban.interop.jsoniter

import caliban.Value._
import caliban._
import caliban.parsing.adt.LocationInfo
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import scala.collection.immutable.VectorBuilder
import scala.jdk.CollectionConverters._

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark jsoniter dependency as optional and keep Encoders defined in the companion object.
 */
private[caliban] trait IsJsoniterCodec[F[_]]
private[caliban] object IsJsoniterCodec {
  implicit val isJsoniterCodec: IsJsoniterCodec[JsonValueCodec] = null
}

object json {

  private def encodeValue(out: JsonWriter): Value => Unit = {
    case NullValue           => out.writeNull()
    case v: IntValue         =>
      v match {
        case IntValue.IntNumber(value)    => out.writeVal(value)
        case IntValue.LongNumber(value)   => out.writeVal(value)
        case IntValue.BigIntNumber(value) => out.writeVal(value)
      }
    case v: FloatValue       =>
      v match {
        case FloatValue.FloatNumber(value)      => out.writeVal(value)
        case FloatValue.DoubleNumber(value)     => out.writeVal(value)
        case FloatValue.BigDecimalNumber(value) => out.writeVal(value)
      }
    case StringValue(value)  => out.writeVal(value)
    case BooleanValue(value) => out.writeVal(value)
    case EnumValue(value)    => out.writeVal(value)
  }

  private def encodeInputValue(out: JsonWriter, depth: Int): InputValue => Unit = {
    case v: Value                    => encodeValue(out)(v)
    case InputValue.ListValue(l)     =>
      val depthM1 = depth - 1
      if (depthM1 < 0) out.encodeError("depth limit exceeded")
      out.writeArrayStart()
      l.foreach(v => encodeInputValue(out, depthM1)(v))
      out.writeArrayEnd()
    case InputValue.ObjectValue(o)   =>
      val depthM1 = depth - 1
      if (depthM1 < 0) out.encodeError("depth limit exceeded")
      out.writeObjectStart()
      o.foreach { case (k, v) =>
        out.writeKey(k)
        encodeInputValue(out, depthM1)(v)
      }
      out.writeObjectEnd()
    case InputValue.VariableValue(v) => out.writeVal(v)
  }

  private def encodeResponseValue(out: JsonWriter, depth: Int): ResponseValue => Unit = {
    case v: Value                     => encodeValue(out)(v)
    case ResponseValue.ListValue(l)   =>
      val depthM1 = depth - 1
      if (depthM1 < 0) out.encodeError("depth limit exceeded")
      out.writeArrayStart()
      l.foreach(v => encodeResponseValue(out, depthM1)(v))
      out.writeArrayEnd()
    case ResponseValue.ObjectValue(o) =>
      val depthM1 = depth - 1
      if (depthM1 < 0) out.encodeError("depth limit exceeded")
      out.writeObjectStart()
      o.foreach { case (k, v) =>
        out.writeKey(k)
        encodeResponseValue(out, depthM1)(v)
      }
      out.writeObjectEnd()
    case s: ResponseValue.StreamValue => out.writeVal(s.toString)
  }

  private val valueTokens: Set[Byte] = Set('"', '-', 't', 'f', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  private val emptyInputList         = InputValue.ListValue(Nil)
  private val emptyInputObject       = InputValue.ObjectValue(Map.empty)
  private val emptyResponseList      = ResponseValue.ListValue(Nil)
  private val emptyResponseObject    = ResponseValue.ObjectValue(Nil)

  private def valueDecoder(in: JsonReader): Byte => Value = { b =>
    if (b == '"') {
      in.rollbackToken()
      StringValue(in.readString(null))
    } else if (b == 'f' || b == 't') {
      in.rollbackToken()
      if (in.readBoolean()) BooleanValue(true)
      else BooleanValue(false)
    } else if (b >= '0' && b <= '9' || b == '-') {
      in.rollbackToken()
      defaultNumberParser(in)
    } else in.readNullOrError(NullValue, "expected JSON value")
  }

  private def decodeInputValue(in: JsonReader, depth: Int): InputValue = {
    val b = in.nextToken()
    if (valueTokens.contains(b)) {
      valueDecoder(in)(b)
    } else if (b == '[') {
      val depthM1 = depth - 1
      if (depthM1 < 0) in.decodeError("depth limit exceeded")
      if (in.isNextToken(']')) emptyInputList
      else {
        in.rollbackToken()
        val x = new VectorBuilder[InputValue]
        while ({
          x += decodeInputValue(in, depthM1)
          in.isNextToken(',')
        }) ()
        if (in.isCurrentToken(']')) InputValue.ListValue(x.result().toList)
        else in.arrayEndOrCommaError()
      }
    } else if (b == '{') {
      val depthM1 = depth - 1
      if (depthM1 < 0) in.decodeError("depth limit exceeded")
      if (in.isNextToken('}')) emptyInputObject
      else {
        in.rollbackToken()
        val x = new java.util.LinkedHashMap[String, InputValue](8)
        while ({
          x.put(in.readKeyAsString(), decodeInputValue(in, depthM1))
          in.isNextToken(',')
        }) ()
        if (in.isCurrentToken('}')) InputValue.ObjectValue(x.asScala.toMap)
        else in.objectEndOrCommaError()
      }
    } else in.readNullOrError(NullValue, "expected JSON value")
  }

  private def decodeResponseValue(in: JsonReader, depth: Int): ResponseValue = {
    val b = in.nextToken()
    if (valueTokens.contains(b)) {
      valueDecoder(in)(b)
    } else if (b == '[') {
      val depthM1 = depth - 1
      if (depthM1 < 0) in.decodeError("depth limit exceeded")
      if (in.isNextToken(']')) emptyResponseList
      else {
        in.rollbackToken()
        val x = new VectorBuilder[ResponseValue]
        while ({
          x += decodeResponseValue(in, depthM1)
          in.isNextToken(',')
        }) ()
        if (in.isCurrentToken(']')) ResponseValue.ListValue(x.result().toList)
        else in.arrayEndOrCommaError()
      }
    } else if (b == '{') {
      val depthM1 = depth - 1
      if (depthM1 < 0) in.decodeError("depth limit exceeded")
      if (in.isNextToken('}')) emptyResponseObject
      else {
        in.rollbackToken()
        val x = new java.util.LinkedHashMap[String, ResponseValue](8)
        while ({
          x.put(in.readKeyAsString(), decodeResponseValue(in, depthM1))
          in.isNextToken(',')
        }) ()
        if (in.isCurrentToken('}')) ResponseValue.ObjectValue(x.asScala.toList)
        else in.objectEndOrCommaError()
      }
    } else in.readNullOrError(NullValue, "expected JSON value")
  }

  private val defaultNumberParser: JsonReader => Value = in => {
    in.setMark()
    var digits = 0
    var b      = in.nextByte()
    if (b == '-') b = in.nextByte()
    try while (b >= '0' && b <= '9') {
      b = in.nextByte()
      digits += 1
    } catch {
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
    override def decodeValue(in: JsonReader, default: InputValue): InputValue = decodeInputValue(in, 1024)
    override def encodeValue(x: InputValue, out: JsonWriter): Unit            = encodeInputValue(out, 1024)(x)
    override def nullValue: InputValue                                        = NullValue
  }

  val responseValueCodec: JsonValueCodec[ResponseValue] = new JsonValueCodec[ResponseValue] {
    override def decodeValue(in: JsonReader, default: ResponseValue): ResponseValue = decodeResponseValue(in, 1024)
    override def encodeValue(x: ResponseValue, out: JsonWriter): Unit               = encodeResponseValue(out, 1024)(x)
    override def nullValue: ResponseValue                                           = NullValue
  }

  private case class DeserializedError(
    message: String,
    path: Option[List[Either[String, Int]]],
    locations: Option[List[LocationInfo]],
    extensions: Option[ResponseValue.ObjectValue]
  )

  implicit val errorValueCodec: JsonValueCodec[CalibanError] = new JsonValueCodec[CalibanError] {
    private val errorProxyCodec: JsonValueCodec[DeserializedError] = JsonCodecMaker.make

    override def decodeValue(in: JsonReader, default: CalibanError): CalibanError = {
      val err = errorProxyCodec.decodeValue(in, null)
      CalibanError.ExecutionError(
        msg = err.message,
        path = err.path.getOrElse(Nil),
        locationInfo = err.locations.flatMap(_.headOption),
        innerThrowable = None,
        extensions = err.extensions
      )
    }
    override def encodeValue(x: CalibanError, out: JsonWriter): Unit              =
      responseValueCodec.encodeValue(x.toResponseValue, out)
    override def nullValue: CalibanError                                          = null.asInstanceOf[CalibanError]
  }

  private case class GraphQLResponseProxy(data: ResponseValue, errors: Option[List[CalibanError]])

  implicit val graphQLResponseCodec: JsonValueCodec[GraphQLResponse[CalibanError]] =
    new JsonValueCodec[GraphQLResponse[CalibanError]] {
      private val proxyCodec: JsonValueCodec[GraphQLResponseProxy]                      = JsonCodecMaker.make
      override def decodeValue(
        in: JsonReader,
        default: GraphQLResponse[CalibanError]
      ): GraphQLResponse[CalibanError] = {
        val resp = proxyCodec.decodeValue(in, null)
        GraphQLResponse[CalibanError](
          data = resp.data,
          errors = resp.errors.getOrElse(Nil),
          extensions = None
        )
      }
      override def encodeValue(x: GraphQLResponse[CalibanError], out: JsonWriter): Unit =
        responseValueCodec.encodeValue(x.toResponseValue, out)
      override def nullValue: GraphQLResponse[CalibanError]                             =
        null.asInstanceOf[GraphQLResponse[CalibanError]]
    }

  implicit val graphQLRequestCodec: JsonValueCodec[GraphQLRequest]      = JsonCodecMaker.make
  implicit val graphQLWSInputCodec: JsonValueCodec[GraphQLWSInput]      = JsonCodecMaker.make
  implicit val graphQLWSOuputCodec: JsonValueCodec[GraphQLWSOutput]     = JsonCodecMaker.make
  implicit val stringMapCodec: JsonValueCodec[Map[String, Seq[String]]] = JsonCodecMaker.make
}
