package caliban.interop.play

import caliban._
import play.api.libs.json.JsObject

private[caliban] final case class PlayWSMessage(id: String, messageType: String, payload: Option[JsObject])
    extends WSMessage {
  lazy val request: Option[GraphQLRequest] = payload.flatMap(_.validate[GraphQLRequest].asOpt)
}
