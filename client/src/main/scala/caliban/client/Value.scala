package caliban.client

import io.circe.{ Decoder, Encoder, Json }

sealed trait Value

object Value {
  case object NullValue extends Value {
    override def toString: String = "null"
  }
  case class NumberValue(value: BigDecimal) extends Value {
    override def toString: String = s"$value"
  }
  case class StringValue(value: String) extends Value {
    override def toString: String = s""""${value.replace("\"", "\\\"")}""""
  }
  case class BooleanValue(value: Boolean) extends Value {
    override def toString: String = value.toString
  }
  case class ListValue(values: List[Value]) extends Value {
    override def toString: String = values.map(_.toString).mkString("[", ",", "]")
  }
  case class ObjectValue(fields: List[(String, Value)]) extends Value {
    override def toString: String =
      fields.map { case (name, value) => s""""$name":${value.toString}""" }.mkString("{", ",", "}")
  }

  private def jsonToValue(json: Json): Value =
    json.fold(
      NullValue,
      BooleanValue,
      number => NumberValue(number.toBigDecimal getOrElse BigDecimal(number.toDouble)),
      StringValue,
      array => Value.ListValue(array.toList.map(jsonToValue)),
      obj => Value.ObjectValue(obj.toList.map { case (k, v) => k -> jsonToValue(v) })
    )

  private def valueToJson(a: Value): Json = a match {
    case NullValue           => Json.Null
    case NumberValue(value)  => Json.fromBigDecimal(value)
    case StringValue(value)  => Json.fromString(value)
    case BooleanValue(value) => Json.fromBoolean(value)
    case ListValue(values)   => Json.fromValues(values.map(valueToJson))
    case ObjectValue(fields) => Json.obj(fields.map { case (k, v) => k -> valueToJson(v) }: _*)
  }

  implicit val valueDecoder: Decoder[Value] = Decoder.instance(hcursor => Right(jsonToValue(hcursor.value)))

  implicit val valueEncoder: Encoder[Value] = (a: Value) => valueToJson(a)
}
