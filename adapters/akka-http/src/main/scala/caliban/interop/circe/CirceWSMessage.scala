package caliban.interop.circe

import caliban._
import io.circe.ACursor

private[caliban] final case class CirceWSMessage(id: String, messageType: String, payload: ACursor) extends WSMessage {
  lazy val operationName: Option[String] = payload.downField("operationName").success.flatMap(_.value.asString)
  lazy val query: Option[String]         = payload.downField("query").success.flatMap(_.value.asString)
}
