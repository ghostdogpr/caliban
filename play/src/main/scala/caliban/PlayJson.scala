package caliban

import caliban.Value._
import caliban.parsing.adt.LocationInfo
import play.api.http.Writeable
import play.api.libs.json.Reads._
import play.api.libs.json.Writes._
import play.api.libs.json._

import scala.util.Try

object PlayJson {

  //// JSON Reads
  private def jsonToInputValue(json: JsValue): InputValue = json match {
    case JsNull             => NullValue
    case boolean: JsBoolean => BooleanValue(boolean.value)
    case JsNumber(value) =>
      Try(value.toIntExact).fold(_ => FloatValue(value), IntValue(_))
    case JsString(value) => StringValue(value)
    case JsArray(array) =>
      InputValue.ListValue(array.toList.map(jsonToInputValue))
    case JsObject(obj) =>
      val builder = Predef.Map.newBuilder[String, InputValue]
      builder.sizeHint(obj)
      obj.foreach { case (k, v) => builder += (k -> jsonToInputValue(v)) }
      InputValue.ObjectValue(builder.result())
  }

  implicit val readsInputValue: Reads[InputValue] =
    json => JsSuccess(jsonToInputValue(json))

  implicit val readsGraphQLRequest: Reads[GraphQLRequest] =
    for {
      query         <- (__ \ "query").read[String]
      operationName <- (__ \ "operationName").readNullable[String]
      variables     <- (__ \ "variables").readNullable[Map[String, InputValue]]
    } yield GraphQLRequest(query, operationName, variables)

  //// JSON Writes

  // ResponseValue
  private def responseValueToJson(response: ResponseValue): JsValue = response match {
    case v: Value                          => valueToJson(v)
    case ResponseValue.ListValue(values)   => JsArray(values.map(responseValueToJson))
    case ResponseValue.ObjectValue(fields) => JsObject(fields.map { case (k, v) => k -> responseValueToJson(v) })
    case s: ResponseValue.StreamValue      => JsString(s.toString)
  }

  private def valueToJson(value: Value): JsValue = value match {
    case Value.NullValue     => JsNull
    case value: IntValue     => JsNumber(BigDecimal(value.toBigInt.bigInteger))
    case value: FloatValue   => JsNumber(value.toBigDecimal)
    case StringValue(value)  => JsString(value)
    case BooleanValue(value) => JsBoolean(value)
    case EnumValue(value)    => JsString(value)
  }

  implicit val writesResponseValue: Writes[ResponseValue] = responseValueToJson

  private def handleError(err: Any): JsValue = err match {
    case ce: CalibanError => errorValueEncoder.writes(ce)
    case _                => Json.obj("message" -> JsString(err.toString))
  }

  // CalibanError
  private def locationToJson(li: LocationInfo): JsValue =
    JsObject(List("line" -> li.line.asJson, "column" -> li.column.asJson))

  val errorValueEncoder: Writes[CalibanError] = {
    case CalibanError.ParsingError(msg, locationInfo, _) =>
      JsObject(
        List("message" -> s"Parsing Error: $msg".asJson) ++
          locationInfo.map(li => "locations" -> Json.arr(locationToJson(li)))
      )
    case CalibanError.ValidationError(msg, _, locationInfo) =>
      JsObject(
        List("message" -> msg.asJson) ++
          locationInfo.map(li => "locations" -> Json.arr(locationToJson(li)))
      )
    case CalibanError.ExecutionError(msg, path, locationInfo, _) =>
      JsObject(
        List("message" -> msg.asJson) ++
          locationInfo.map(li => "locations" -> Json.arr(locationToJson(li))) ++
          (path match {
            case p if p.nonEmpty => Some("path" -> JsArray(p.map(_.fold(_.asJson, _.asJson))))
            case _               => None
          })
      )
  }

  // GraphQLResponse
  implicit def writesGraphQLResponse[E]: Writes[GraphQLResponse[E]] = {
    case GraphQLResponse(data, Nil, None) => Json.obj("data" -> responseValueToJson(data))
    case GraphQLResponse(data, Nil, Some(extensions)) =>
      Json.obj(
        "data"       -> responseValueToJson(data),
        "extensions" -> responseValueToJson(extensions.asInstanceOf[ResponseValue])
      )
    case GraphQLResponse(data, errors, None) =>
      Json.obj("data" -> responseValueToJson(data), "errors" -> JsArray(errors.map(handleError)))
    case GraphQLResponse(data, errors, Some(extensions)) =>
      Json.obj(
        "data"       -> responseValueToJson(data),
        "errors"     -> JsArray(errors.map(handleError)),
        "extensions" -> responseValueToJson(extensions.asInstanceOf[ResponseValue])
      )
  }

  //// HTTP writable
  implicit def writableGraphQLResponse[E]: Writeable[GraphQLResponse[E]] =
    Writeable.writeableOf_JsValue.map(writesGraphQLResponse.writes)

  //// Simple helper syntax
  implicit private class IntAsJsonOps(val value: Int) extends AnyVal {
    def asJson: JsValue = JsNumber(value)
  }

  implicit private class StringAsJsonOps(val value: String) extends AnyVal {
    def asJson: JsValue = JsString(value)
  }

}
