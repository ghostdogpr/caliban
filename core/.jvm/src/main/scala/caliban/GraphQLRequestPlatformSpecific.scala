package caliban

import caliban.interop.spray.IsSprayJsonReader
import spray.json.{ DefaultJsonProtocol, JsValue, JsonReader }
import ValueSprayJson.inputValueFormat

private[caliban] trait GraphQLRequestPlatformSpecific {
  implicit def sprayJsonReader[F[_]: IsSprayJsonReader]: F[GraphQLRequest] =
    GraphQLRequestSprayJson.graphQLRequestJsonReader.asInstanceOf[F[GraphQLRequest]]
}

private[caliban] object GraphQLRequestSprayJson extends DefaultJsonProtocol {

  val graphQLRequestJsonReader: JsonReader[GraphQLRequest] = new JsonReader[GraphQLRequest] {
    def read(json: JsValue): GraphQLRequest = {
      val obj = json.asJsObject
      GraphQLRequest(
        obj.fields("query").convertTo[String],
        obj.fields("operationName").convertTo[Option[String]],
        obj.fields("variables").convertTo[Option[Map[String, InputValue]]]
      )
    }
  }

}
