package caliban.interop.circe

import caliban.Value._
import caliban._
import caliban.introspection.adt.__Type
import caliban.schema.Types.makeScalar
import caliban.schema.{ ArgBuilder, Schema, Step }
import io.circe._

object json {
  implicit val jsonSchema: Schema[Any, Json] = new Schema[Any, Json] {
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = makeScalar("Json")
    override def resolve(value: Json): Step[Any]                           = Step.fromEither(responseValueDecoder.decodeJson(value))
  }

  implicit val jsonArgBuilder: ArgBuilder[Json] = (input: InputValue) => Right(inputValueEncoder(input))

  private val inputValueEncoder: Encoder[InputValue]         = Encoder
    .instance[InputValue] {
      case NullValue                          => Json.Null
      case IntValue.IntNumber(value)          => Json.fromInt(value)
      case IntValue.LongNumber(value)         => Json.fromLong(value)
      case IntValue.BigIntNumber(value)       => Json.fromBigInt(value)
      case FloatValue.FloatNumber(value)      => Json.fromFloatOrNull(value)
      case FloatValue.DoubleNumber(value)     => Json.fromDoubleOrNull(value)
      case FloatValue.BigDecimalNumber(value) => Json.fromBigDecimal(value)
      case StringValue(value)                 => Json.fromString(value)
      case BooleanValue(value)                => Json.fromBoolean(value)
      case EnumValue(value)                   => Json.fromString(value)
      case InputValue.ListValue(values)       => Json.arr(values.map(inputValueEncoder.apply): _*)
      case InputValue.ObjectValue(fields)     =>
        Json.obj(fields.map { case (k, v) => k -> inputValueEncoder.apply(v) }.toList: _*)
      case InputValue.VariableValue(name)     => Json.fromString(name)
    }
  private def jsonToResponseValue(json: Json): ResponseValue =
    json.fold(
      NullValue,
      BooleanValue.apply,
      number =>
        number.toInt.map(IntValue.apply) orElse
          number.toBigInt.map(IntValue.apply) orElse
          number.toBigDecimal.map(FloatValue.apply) getOrElse
          FloatValue(number.toDouble),
      StringValue.apply,
      array => ResponseValue.ListValue(array.toList.map(jsonToResponseValue)),
      obj => objToResponseValue(obj)
    )

  private def objToResponseValue(obj: JsonObject) = ResponseValue.ObjectValue(obj.toList.map { case (k, v) =>
    k -> jsonToResponseValue(v)
  })

  private val responseValueDecoder: Decoder[ResponseValue] =
    Decoder.instance(hcursor => Right(jsonToResponseValue(hcursor.value)))
}

/**
 * Mix-in trait that provides implicit [[caliban.schema.Schema]] and [[caliban.schema.ArgBuilder]] instances for Circe's JSON support.
 */
trait CirceJson {
  implicit val jsonSchema: Schema[Any, Json]    = json.jsonSchema
  implicit val jsonArgBuilder: ArgBuilder[Json] = json.jsonArgBuilder
}
