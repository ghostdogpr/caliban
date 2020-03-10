package caliban.client

import caliban.client.Value.ObjectValue
import io.circe.{ Decoder, HCursor }

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse(data: Option[Value], errors: List[GraphQLResponseError] = Nil)

object GraphQLResponse {

  implicit val objectValueDecoder: Decoder[ObjectValue] = Decoder[Value].emap {
    case o @ ObjectValue(_) => Right(o)
    case _                  => Left("Invalid value, should be an object.")
  }

  implicit val decoder: Decoder[GraphQLResponse] = (c: HCursor) =>
    for {
      data   <- c.downField("data").as[Option[Value]]
      errors <- c.downField("errors").as[Option[List[GraphQLResponseError]]]
    } yield GraphQLResponse(data, errors.getOrElse(Nil))

}
