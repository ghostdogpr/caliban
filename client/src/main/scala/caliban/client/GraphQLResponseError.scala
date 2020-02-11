package caliban.client

import caliban.client.GraphQLResponseError.Location
import io.circe.{ Decoder, DecodingFailure, HCursor }
import io.circe.derivation.deriveDecoder

case class GraphQLResponseError(
  message: String,
  locations: Option[List[Location]],
  path: Option[List[Either[String, Int]]]
)

object GraphQLResponseError {

  case class Location(line: Int, column: Int)

  implicit val decoderEither: Decoder[Either[String, Int]] = (c: HCursor) =>
    c.value.asNumber.flatMap(_.toInt).map(v => Right(Right(v))) orElse c.value.asString
      .map(v => Right(Left(v))) getOrElse Left(DecodingFailure("Value is not an Either[String, Int]", c.history))

  implicit val locationDecoder: Decoder[Location]     = deriveDecoder[Location]
  implicit val decoder: Decoder[GraphQLResponseError] = deriveDecoder[GraphQLResponseError]

}
