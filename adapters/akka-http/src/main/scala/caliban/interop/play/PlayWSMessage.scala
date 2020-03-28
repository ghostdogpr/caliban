package caliban.interop.play

import caliban._
import play.api.libs.json.JsObject

private[caliban] final case class PlayWSMessage(id: String, messageType: String, payload: Option[JsObject])
    extends WSMessage {
  lazy val operationName: Option[String] =
    payload.flatMap(p => (p \ "operationName").toOption).flatMap(_.validate[String].asOpt)
  lazy val query: Option[String] = payload.flatMap(p => (p \ "query").toOption).flatMap(_.validate[String].asOpt)
}
