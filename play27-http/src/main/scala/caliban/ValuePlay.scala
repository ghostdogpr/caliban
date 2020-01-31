package caliban

import caliban.Value._
import play.api.libs.json._

trait ValuePlay {

  private def jsonToValue(json: JsValue): InputValue = json match {
    case JsNull               => NullValue
    case boolean: JsBoolean   => BooleanValue(boolean.value)
    case JsNumber(value)      => value.toBigIntExact.map(IntValue.apply).getOrElse(FloatValue(value))
    case JsString(value)      => StringValue(value)
    case JsArray(value)       => InputValue.ListValue(value.toList.map(jsonToValue))
    case JsObject(underlying) => InputValue.ObjectValue(underlying.map { case (k, v) => k -> jsonToValue(v) }.toMap)
  }

  val inputValueReads: Reads[InputValue] = Reads { jsValue =>
    JsSuccess(jsonToValue(jsValue))
  }

  val responseValueWrites: Writes[ResponseValue] = Writes {
    case NullValue                       => JsNull
    case BooleanValue(v)                 => JsBoolean(v)
    case StringValue(v)                  => JsString(v)
    case EnumValue(v)                    => JsString(v)
    case IntValue.IntNumber(v)           => JsNumber(v)
    case IntValue.BigIntNumber(v)        => JsNumber(BigDecimal(v))
    case IntValue.LongNumber(v)          => JsNumber(v)
    case FloatValue.FloatNumber(v)       => JsNumber(v.toDouble)
    case FloatValue.DoubleNumber(v)      => JsNumber(v)
    case FloatValue.BigDecimalNumber(v)  => JsNumber(v)
    case ResponseValue.ListValue(values) => JsArray(values.map(responseValueWrites.writes))
    case ResponseValue.ObjectValue(fields) =>
      JsObject(fields.map {
        case (k, v) => k -> responseValueWrites.writes(v)
      })
    case ResponseValue.StreamValue(stream) => JsString(stream.toString)
  }

}
