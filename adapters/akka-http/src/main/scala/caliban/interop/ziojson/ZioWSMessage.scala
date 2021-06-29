package caliban.interop.ziojson

import caliban._
import zio.json._

private[caliban] final case class ZioWSMessage(id: String, messageType: String, payload: Option[String])
    extends WSMessage {
  lazy val request: Option[GraphQLRequest] = payload.flatMap(_.fromJson[GraphQLRequest].toOption)
}

object ZioWSMessage {
  implicit val decoder = DeriveJsonDecoder.gen[ZioWSMessage]
}
