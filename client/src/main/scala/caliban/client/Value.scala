package caliban.client

import io.circe.{ Decoder, Encoder, Json }

/**
 * Value that can be returned by the server or sent as an argument.
 */
sealed trait Value

object Value {
  case object NullValue extends Value {
    override def toString: String = "null"
  }
  case class __NumberValue(value: BigDecimal) extends Value {
    override def toString: String = s"$value"
  }
  case class __EnumValue(value: String) extends Value {
    override def toString: String = value
  }
  case class __StringValue(value: String) extends Value {
    override def toString: String = s""""${value.replace("\"", "\\\"")}""""
  }
  case class __BooleanValue(value: Boolean) extends Value {
    override def toString: String = value.toString
  }
  case class __ListValue(values: List[Value]) extends Value {
    override def toString: String = values.map(_.toString).mkString("[", ",", "]")
  }
  case class ObjectValue(fields: List[(String, Value)]) extends Value {
    override def toString: String =
      fields.map { case (name, value) => s"""$name:${value.toString}""" }.mkString("{", ",", "}")
  }

  private def jsonToValue(json: Json): Value =
    json.fold(
      NullValue,
      __BooleanValue,
      number => __NumberValue(number.toBigDecimal getOrElse BigDecimal(number.toDouble)),
      __StringValue,
      array => Value.__ListValue(array.toList.map(jsonToValue)),
      obj => Value.ObjectValue(obj.toList.map { case (k, v) => k -> jsonToValue(v) })
    )

  private def valueToJson(a: Value): Json = a match {
    case NullValue             => Json.Null
    case __NumberValue(value)  => Json.fromBigDecimal(value)
    case __StringValue(value)  => Json.fromString(value)
    case __EnumValue(value)    => Json.fromString(value)
    case __BooleanValue(value) => Json.fromBoolean(value)
    case __ListValue(values)   => Json.fromValues(values.map(valueToJson))
    case ObjectValue(fields)   => Json.obj(fields.map { case (k, v) => k -> valueToJson(v) }: _*)
  }

  implicit val valueDecoder: Decoder[Value] = Decoder.instance(hcursor => Right(jsonToValue(hcursor.value)))

  implicit val valueEncoder: Encoder[Value] = (a: Value) => valueToJson(a)
}
