package caliban.client

import io.circe.{ Decoder, DecodingFailure, HCursor }
import io.circe.derivation.deriveDecoder

case class GraphQLResponseError(
  message: String,
  locations: Option[List[String]],
  path: Option[List[Either[String, Int]]]
)

object GraphQLResponseError {

  implicit val decoderEither: Decoder[Either[String, Int]] = (c: HCursor) =>
    c.value.asNumber.flatMap(_.toInt).map(v => Right(Right(v))) orElse c.value.asString
      .map(v => Right(Left(v))) getOrElse Left(DecodingFailure("Value is not an Either[String, Int]", c.history))

  implicit val decoder: Decoder[GraphQLResponseError] = deriveDecoder[GraphQLResponseError]

}
