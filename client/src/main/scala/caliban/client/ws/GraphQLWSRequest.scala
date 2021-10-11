package caliban.client.ws

import caliban.client.GraphQLRequest
import io.circe.syntax._
import io.circe.{ Encoder, Json }

final case class GraphQLWSRequest(`type`: String, id: Option[String], payload: Option[GraphQLRequest])

object GraphQLWSRequest {
  implicit val graphQLWSRequestEncoder: Encoder[GraphQLWSRequest] = (req: GraphQLWSRequest) =>
    Json.obj(
      "type"    -> Json.fromString(req.`type`),
      "id"      -> req.id.fold(Json.Null)(Json.fromString),
      "payload" -> req.payload.fold(Json.Null)(_.asJson)
    )
}
