package caliban.client

import io.circe.{ Decoder, Encoder, Json }

/**
 * Value that can be returned by the server or sent as an argument.
 */
sealed trait __Value

object __Value {
  case object __NullValue                                   extends __Value {
    override def toString: String = "null"
  }
  case class __NumberValue(value: BigDecimal)               extends __Value {
    override def toString: String = s"$value"
  }
  case class __EnumValue(value: String)                     extends __Value {
    override def toString: String = value
  }
  case class __StringValue(value: String)                   extends __Value {
    override def toString: String = s""""${value.replace("\"", "\\\"")}""""
  }
  case class __BooleanValue(value: Boolean)                 extends __Value {
    override def toString: String = value.toString
  }
  case class __ListValue(values: List[__Value])             extends __Value {
    override def toString: String = values.map(_.toString).mkString("[", ",", "]")
  }
  case class __ObjectValue(fields: List[(String, __Value)]) extends __Value {
    override def toString: String =
      fields.map { case (name, value) => s"""$name:${value.toString}""" }.mkString("{", ",", "}")
  }

  private def jsonToValue(json: Json): __Value =
    json.fold(
      __NullValue,
      __BooleanValue,
      number => __NumberValue(number.toBigDecimal getOrElse BigDecimal(number.toDouble)),
      __StringValue,
      array => __Value.__ListValue(array.toList.map(jsonToValue)),
      obj => __Value.__ObjectValue(obj.toList.map { case (k, v) => k -> jsonToValue(v) })
    )

  private def valueToJson(a: __Value): Json = a match {
    case `__NullValue`         => Json.Null
    case __NumberValue(value)  => Json.fromBigDecimal(value)
    case __StringValue(value)  => Json.fromString(value)
    case __EnumValue(value)    => Json.fromString(value)
    case __BooleanValue(value) => Json.fromBoolean(value)
    case __ListValue(values)   => Json.fromValues(values.map(valueToJson))
    case __ObjectValue(fields) => Json.obj(fields.map { case (k, v) => k -> valueToJson(v) }: _*)
  }

  implicit val valueDecoder: Decoder[__Value] = Decoder.instance(hcursor => Right(jsonToValue(hcursor.value)))

  implicit val valueEncoder: Encoder[__Value] = (a: __Value) => valueToJson(a)
}
