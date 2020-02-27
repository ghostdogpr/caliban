package caliban

import caliban.Value._
import caliban.parsing.adt.LocationInfo
import play.api.http.Writeable
import play.api.libs.json.Writes._
import play.api.libs.json._

object PlayJson {

  //// JSON Reads
  private def jsonToInputValue(json: JsValue): InputValue = json match {
    case JsNull             => NullValue
    case boolean: JsBoolean => BooleanValue(boolean.value)
    case JsNumber(value) =>
      if (value.isValidInt) IntValue(value.toInt)
      else if (value.isValidLong) IntValue(value.toLong)
      else if (value.isWhole) IntValue(value.toBigInt)
      else FloatValue(value)
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
    Json.reads

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
  private implicit val locationToJson: Writes[LocationInfo] =
    li => Json.obj("locations" -> Json.arr(Json.obj("line" -> li.line, "column" -> li.column)))

  private def encodeLocationInfoAndMessage(li: Option[LocationInfo], message: String) =
    li.fold(Json.obj())(li => Json.obj("locations" -> li)) ++ Json.obj("message" -> message)

  val errorValueEncoder: Writes[CalibanError] = {
    case CalibanError.ParsingError(msg, locationInfo, _) =>
      encodeLocationInfoAndMessage(locationInfo, s"Parsing Error: $msg")

    case CalibanError.ValidationError(msg, _, locationInfo) =>
      encodeLocationInfoAndMessage(locationInfo, msg)

    case CalibanError.ExecutionError(msg, path, locationInfo, _) =>
      val paths =
        if (path.isEmpty) Nil
        else
          List(
            "path" -> Json
              .arr(path.map(_.fold(Json.toJsFieldJsValueWrapper[String], Json.toJsFieldJsValueWrapper[Int])): _*)
          )

      encodeLocationInfoAndMessage(locationInfo, msg) ++ JsObject(paths)
  }

  // GraphQLResponse
  implicit def writesGraphQLResponse[E]: Writes[GraphQLResponse[E]] = {
    case GraphQLResponse(data, errors, maybeExtensions) =>
      Json.obj(
        "data" -> responseValueToJson(data),
        "errors" -> (errors match {
          case Nil => Option.empty[JsValue]
          case _   => Some(JsArray(errors.map(handleError)))
        }),
        "extensions" -> maybeExtensions.map(responseValueToJson)
      )
  }

  //// HTTP writable
  implicit def writableGraphQLResponse[E]: Writeable[GraphQLResponse[E]] =
    Writeable.writeableOf_JsValue.map(writesGraphQLResponse.writes)

}
