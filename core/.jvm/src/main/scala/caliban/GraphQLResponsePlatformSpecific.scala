package caliban

import caliban.interop.spray.IsSprayJsonWriter
import spray.json.{ DefaultJsonProtocol, JsArray, JsObject, JsString, JsValue, JsonWriter }

private[caliban] trait GraphQLResponsePlatformSpecific {
  implicit def sprayJsonWriter[F[_]: IsSprayJsonWriter, E]: F[GraphQLResponse[E]] =
    GraphQLResponseSprayJson.graphQLResponseJsonWriter.asInstanceOf[F[GraphQLResponse[E]]]
}

private[caliban] object GraphQLResponseSprayJson extends DefaultJsonProtocol {

  def asJson(r: ResponseValue): JsValue = ValueSprayJson.responseValueWriter.write(r)
  val graphQLResponseJsonWriter: JsonWriter[GraphQLResponse[Any]] = new JsonWriter[GraphQLResponse[Any]] {
    def write(obj: GraphQLResponse[Any]): JsValue = obj match {
      case GraphQLResponse(data, Nil, None) => JsObject("data" -> ValueSprayJson.responseValueWriter.write(data))
      case GraphQLResponse(data, Nil, Some(extensions)) =>
        JsObject(
          "data" -> asJson(data),
          "extensions" ->
            asJson(extensions.asInstanceOf[ResponseValue])
        )
      case GraphQLResponse(data, errors, None) =>
        JsObject("data" -> asJson(data), "errors" -> JsArray(errors.map(handleError): _*))
      case GraphQLResponse(data, errors, Some(extensions)) =>
        JsObject(
          "data"       -> asJson(data),
          "errors"     -> JsArray(errors.map(handleError): _*),
          "extensions" -> asJson(extensions.asInstanceOf[ResponseValue])
        )
    }

    private def handleError(err: Any): JsValue =
      err match {
        case ce: CalibanError => CalibanErrorSprayJson.calibanErrorJsonWriter.write(ce)
        case _                => JsObject("message" -> JsString(err.toString))
      }
  }

}
