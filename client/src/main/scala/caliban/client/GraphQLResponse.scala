package caliban.client

import caliban.client.Value.ObjectValue
import io.circe.derivation.deriveDecoder
import io.circe.Decoder

case class GraphQLResponse(
  data: Value,
  errors: List[GraphQLResponseError] = Nil,
  extensions: Option[ObjectValue] = None
)

object GraphQLResponse {

  implicit val objectValueDecoder: Decoder[ObjectValue] = Decoder[Value].emap {
    case o @ ObjectValue(_) => Right(o)
    case _                  => Left("Invalid value, should be an object.")
  }

  implicit val decoder: Decoder[GraphQLResponse] = deriveDecoder[GraphQLResponse]

}
