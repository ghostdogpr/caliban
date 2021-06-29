package caliban.interop.circe

import caliban._
import io.circe.ACursor

private[caliban] final case class CirceWSMessage(id: String, messageType: String, payload: ACursor) extends WSMessage {
  lazy val request: Option[GraphQLRequest] = payload.as[GraphQLRequest].fold(_ => None, Some(_))
}
