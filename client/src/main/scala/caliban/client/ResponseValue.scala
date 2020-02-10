package caliban.client

import io.circe.{ Decoder, Json }

sealed trait ResponseValue

object ResponseValue {
  case object NullValue                                         extends ResponseValue
  case class NumberValue(value: BigDecimal)                     extends ResponseValue
  case class StringValue(value: String)                         extends ResponseValue
  case class BooleanValue(value: Boolean)                       extends ResponseValue
  case class ListValue(values: List[ResponseValue])             extends ResponseValue
  case class ObjectValue(fields: List[(String, ResponseValue)]) extends ResponseValue

  private def jsonToResponseValue(json: Json): ResponseValue =
    json.fold(
      NullValue,
      BooleanValue,
      number => NumberValue(number.toBigDecimal getOrElse BigDecimal(number.toDouble)),
      StringValue,
      array => ResponseValue.ListValue(array.toList.map(jsonToResponseValue)),
      obj => ResponseValue.ObjectValue(obj.toList.map { case (k, v) => k -> jsonToResponseValue(v) })
    )

  implicit val responseValueDecoder: Decoder[ResponseValue] =
    Decoder.instance(hcursor => Right(jsonToResponseValue(hcursor.value)))
}
