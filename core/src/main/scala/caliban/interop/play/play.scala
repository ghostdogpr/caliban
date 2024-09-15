package caliban.interop.play

import caliban.Value._
import caliban._
import caliban.introspection.adt.__Type
import caliban.schema.Types.makeScalar
import caliban.schema.{ ArgBuilder, Schema, Step }
import play.api.libs.json._

import scala.util.Try

object json {
  implicit val jsonSchema: Schema[Any, JsValue] = new Schema[Any, JsValue] {
    private def parse(value: JsValue) =
      responseValueReads
        .reads(value)
        .asEither
        .left
        .map(parsingException)

    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = makeScalar("Json")
    override def resolve(value: JsValue): Step[Any]                        = Step.fromEither(parse(value))
  }

  implicit val jsonArgBuilder: ArgBuilder[JsValue] = (input: InputValue) => Right(inputValueWrites.writes(input))

  private def parsingException(
    errs: scala.collection.Seq[(JsPath, scala.collection.Seq[JsonValidationError])]
  ) =
    new Throwable(s"Couldn't decode json: $errs")

  private val valueWrites: Writes[Value] = Writes {
    case NullValue           => JsNull
    case v: IntValue         =>
      v match {
        case IntValue.IntNumber(value)    => JsNumber(BigDecimal(value))
        case IntValue.LongNumber(value)   => JsNumber(BigDecimal(value))
        case IntValue.BigIntNumber(value) => JsNumber(BigDecimal(value))
      }
    case v: FloatValue       =>
      v match {
        case FloatValue.FloatNumber(value)      => JsNumber(BigDecimal(value.toDouble))
        case FloatValue.DoubleNumber(value)     => JsNumber(BigDecimal(value))
        case FloatValue.BigDecimalNumber(value) => JsNumber(value)
      }
    case StringValue(value)  => JsString(value)
    case BooleanValue(value) => JsBoolean(value)
    case EnumValue(value)    => JsString(value)
  }

  private val inputValueWrites: Writes[InputValue] = Writes {
    case value: Value                   => valueWrites.writes(value)
    case InputValue.ListValue(values)   => JsArray(values.map(inputValueWrites.writes))
    case InputValue.ObjectValue(fields) =>
      JsObject(fields.map { case (k, v) => k -> inputValueWrites.writes(v) })
    case InputValue.VariableValue(name) => JsString(name)
  }

  private def jsonToResponseValue(json: JsValue): ResponseValue =
    json match {
      case obj: JsObject     => responseObjectValueFromFields(obj.fields.toList)
      case JsArray(elements) => ResponseValue.ListValue(elements.toList.map(jsonToResponseValue))
      case JsString(value)   => StringValue(value)
      case JsNumber(value)   =>
        Try(value.toIntExact)
          .map(IntValue.apply)
          .getOrElse(FloatValue(value))

      case b: JsBoolean => BooleanValue(b.value)
      case JsNull       => NullValue
    }

  private def responseObjectValueFromFields(fields: List[(String, JsValue)]): ResponseValue.ObjectValue =
    ResponseValue.ObjectValue(fields.map { case (k, v) =>
      k -> jsonToResponseValue(v)
    })

  private val responseValueReads: Reads[ResponseValue] =
    Reads(json => JsSuccess(jsonToResponseValue(json)))
}

/**
 * Mix-in trait that provides implicit [[caliban.schema.Schema]] and [[caliban.schema.ArgBuilder]] instances for PLay JSON support.
 */
trait PlayJson {
  implicit val jsonSchema: Schema[Any, JsValue]    = json.jsonSchema
  implicit val jsonArgBuilder: ArgBuilder[JsValue] = json.jsonArgBuilder
}
