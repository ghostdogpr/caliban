package caliban.client

import io.circe.Encoder
import io.circe.derivation.deriveEncoder

case class GraphQLRequest(query: String)

object GraphQLRequest {

  implicit val encoder: Encoder[GraphQLRequest] = deriveEncoder[GraphQLRequest]

}
