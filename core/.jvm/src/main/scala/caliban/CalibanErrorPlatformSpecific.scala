package caliban

import caliban.ValueSprayJson.responseValueFormat
import caliban.interop.spray.IsSprayJsonWriter
import caliban.parsing.adt.LocationInfo
import spray.json.{ DefaultJsonProtocol, JsArray, JsNumber, JsObject, JsString, JsValue, JsonWriter }

private[caliban] trait CalibanErrorPlatformSpecific {
  implicit def sprayJsonWriter[F[_]: IsSprayJsonWriter]: F[CalibanError] =
    CalibanErrorSprayJson.calibanErrorJsonWriter.asInstanceOf[F[CalibanError]]
}

private[caliban] object CalibanErrorSprayJson extends DefaultJsonProtocol {

  val calibanErrorJsonWriter: JsonWriter[CalibanError] = new JsonWriter[CalibanError] {
    def write(obj: CalibanError): JsValue = obj match {
      case CalibanError.ParsingError(msg, locationInfo, _, extensions) =>
        val strictFields = Map(
          "message"    -> JsString(s"Parsing Error: $msg"),
          "extensions" -> implicitly[JsonWriter[Option[ResponseValue]]].write(extensions)
        )
        // excluding to avoid nulls
        val optionalFields = locationToJson(locationInfo)

        JsObject(strictFields ++ optionalFields)
      case CalibanError.ValidationError(msg, _, locationInfo, extensions) =>
        val strictFields = Map(
          "message"    -> JsString(msg),
          "extensions" -> implicitly[JsonWriter[Option[ResponseValue]]].write(extensions)
        )
        // excluding to avoid nulls
        val optionalFields = locationToJson(locationInfo)

        JsObject(strictFields ++ optionalFields)
      case CalibanError.ExecutionError(msg, path, locationInfo, _, extensions) =>
        val strictFields = Map(
          "message"    -> JsString(msg),
          "extensions" -> implicitly[JsonWriter[Option[ResponseValue]]].write(extensions)
        )
        // excluding to avoid nulls
        val optionalFields = locationToJson(locationInfo) ++
          Some(path).collect {
            case p if p.nonEmpty =>
              JsArray(p.map {
                case Left(value)  => JsString(value)
                case Right(value) => JsNumber(value)
              }: _*)
          }.fold(Map.empty[String, JsValue])(v => Map("path" -> v))

        JsObject(strictFields ++ optionalFields)
    }
  }

  private def locationToJson(locInfo: Option[LocationInfo]): Map[String, JsValue] =
    Some(locInfo).collect {
      case Some(li) => JsArray(JsObject("line" -> JsNumber(li.line), "column" -> JsNumber(li.column)))
    }.fold(Map.empty[String, JsValue])(v => Map("locations" -> v))

}
