package caliban

import play.api.libs.json.JsObject

final case class PlayWSMessage(id: String, messageType: String, payload: Option[JsObject]) {
  lazy val request: Option[GraphQLRequest] = payload.flatMap(_.validate[GraphQLRequest].asOpt)
}
