package caliban

import spray.json.{
  DefaultJsonProtocol,
  JsArray,
  JsBoolean,
  JsNull,
  JsNumber,
  JsObject,
  JsString,
  JsValue,
  JsonFormat,
  JsonReader,
  JsonWriter
}
import Value._

import scala.util.Try

private[caliban] object ValueSprayJson extends DefaultJsonProtocol {

  val valueWriter: JsonWriter[Value] = new JsonWriter[Value] {
    def write(obj: Value): JsValue = obj match {
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

  }

  private def jsonToInputValue(json: JsValue): InputValue =
    json match {
      case JsObject(fields)  => InputValue.ObjectValue(fields.map { case (k, v) => k -> jsonToInputValue(v) })
      case JsArray(elements) => InputValue.ListValue(elements.toList.map(jsonToInputValue))
      case JsString(value)   => StringValue(value)
      case JsNumber(value) =>
        Try(value.toIntExact)
          .map(IntValue.apply)
          .getOrElse(FloatValue(value))

      case b: JsBoolean => BooleanValue(b.value)
      case JsNull       => NullValue
    }

  val inputValueReader: JsonReader[InputValue] = new JsonReader[InputValue] {
    def read(json: JsValue): InputValue = jsonToInputValue(json)
  }
  val inputValueWriter: JsonWriter[InputValue] = new JsonWriter[InputValue] {
    def write(obj: InputValue): JsValue = obj match {
      case value: Value                 => valueWriter.write(value)
      case InputValue.ListValue(values) => JsArray(values.map(inputValueWriter.write): _*)
      case InputValue.ObjectValue(fields) =>
        JsObject(fields.map { case (k, v) => k -> inputValueWriter.write(v) }.toList: _*)
      case InputValue.VariableValue(name) => JsString(name)
    }
  }

  implicit val inputValueFormat = new JsonFormat[InputValue] {
    def write(obj: InputValue): JsValue = inputValueWriter.write(obj)
    def read(json: JsValue): InputValue = inputValueReader.read(json)
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

  val responseValueReader: JsonReader[ResponseValue] =
    new JsonReader[ResponseValue] {
      def read(json: JsValue): ResponseValue = jsonToResponseValue(json)
    }

  val responseValueWriter: JsonWriter[ResponseValue] = new JsonWriter[ResponseValue] {
    def write(obj: ResponseValue): JsValue = obj match {
      case value: Value                    => valueWriter.write(value)
      case ResponseValue.ListValue(values) => JsArray(values.map(responseValueWriter.write): _*)
      case ResponseValue.ObjectValue(fields) =>
        JsObject(fields.map { case (k, v) => k -> responseValueWriter.write(v) }: _*)
      case s: ResponseValue.StreamValue => JsString(s.toString)
    }
  }

  implicit val responseValueFormat = new JsonFormat[ResponseValue] {
    def write(obj: ResponseValue): JsValue = responseValueWriter.write(obj)
    def read(json: JsValue): ResponseValue = responseValueReader.read(json)
  }

}
