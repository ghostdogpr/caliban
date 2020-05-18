package caliban

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.WebSocket.MessageFlowTransformer

case class PlayWSMessage(messageType: String, id: Option[String] = None, payload: Option[JsValue] = None) {
  lazy val request: Option[GraphQLRequest] = payload.flatMap(_.asOpt[GraphQLRequest])
}

object PlayWSMessage {
  def apply[T](messageType: String, id: Option[String], payload: T)(implicit wr: Writes[T]): PlayWSMessage =
    PlayWSMessage(messageType, id, Some(wr.writes(payload)))

  implicit val playWSMessageFormat: Format[PlayWSMessage] =
    ((__ \ "type").format[String] and
      (__ \ "id").formatNullable[String]
      and (__ \ "payload").formatNullable[JsValue])(PlayWSMessage.apply, unlift(PlayWSMessage.unapply))

  implicit val plaWSMessageMessageFlowTransformer: MessageFlowTransformer[PlayWSMessage, PlayWSMessage] =
    MessageFlowTransformer.jsonMessageFlowTransformer[PlayWSMessage, PlayWSMessage]
}
