package caliban.client

import caliban.client.ResponseValue.ObjectValue
import io.circe.derivation.deriveDecoder
import io.circe.Decoder

case class GraphQLResponse(
  data: ResponseValue,
  errors: List[GraphQLResponseError] = Nil,
  extensions: Option[ObjectValue] = None
)

object GraphQLResponse {

  implicit val objectValueDecoder: Decoder[ObjectValue] = Decoder[ResponseValue].emap {
    case o @ ObjectValue(_) => Right(o)
    case _                  => Left("Invalid ResponseValue, should be an ObjectValue.")
  }

  implicit val decoder: Decoder[GraphQLResponse] = deriveDecoder[GraphQLResponse]

}
