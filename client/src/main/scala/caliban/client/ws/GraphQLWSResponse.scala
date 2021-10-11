package caliban.client.ws

import io.circe.{ Decoder, HCursor, Json }

final case class GraphQLWSResponse(`type`: String, id: Option[String], payload: Option[Json])

object GraphQLWSResponse {
  implicit val graphQLWSResponseEncoder: Decoder[GraphQLWSResponse] = (c: HCursor) =>
    for {
      t       <- c.downField("type").as[String]
      id      <- c.downField("id").as[Option[String]]
      payload <- c.downField("payload").as[Option[Json]]
    } yield GraphQLWSResponse(t, id, payload)
}
