package caliban.client

import caliban.client.__Value.__ObjectValue
import io.circe.{ Decoder, HCursor, Json }

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse(
  data: Option[__Value],
  errors: List[GraphQLResponseError] = Nil,
  extensions: Option[Json] = None
)

object GraphQLResponse {

  implicit val objectValueDecoder: Decoder[__ObjectValue] = Decoder[__Value].emap {
    case o @ __ObjectValue(_) => Right(o)
    case _                    => Left("Invalid value, should be an object.")
  }

  implicit val decoder: Decoder[GraphQLResponse] = (c: HCursor) =>
    for {
      data       <- c.downField("data").as[Option[__Value]]
      errors     <- c.downField("errors").as[Option[List[GraphQLResponseError]]]
      extensions <- c.downField("extensions").as[Option[Json]]
    } yield GraphQLResponse(data, errors.getOrElse(Nil), extensions)

}
