package caliban

import caliban.interop.circe._

case class GraphQLResponseError(
  message: String,
  locations: Option[List[String]],
  path: Option[List[Either[String, Int]]]
)

object GraphQLResponseError {
  implicit def circeDecoder[F[_]: IsCirceDecoder, E]: F[GraphQLResponseError] =
    GraphQLResponseErrorCirce.graphQLResponseErrorDecoder.asInstanceOf[F[GraphQLResponseError]]
}

private object GraphQLResponseErrorCirce {
  import io.circe._
  import io.circe.derivation._

  implicit val decoderEither: Decoder[Either[String, Int]] = (c: HCursor) =>
    c.value.asNumber.flatMap(_.toInt).map(v => Right(Right(v))) orElse c.value.asString
      .map(v => Right(Left(v))) getOrElse Left(DecodingFailure("Value is not an Either[String, Int]", c.history))

  val graphQLResponseErrorDecoder: Decoder[GraphQLResponseError] = deriveDecoder[GraphQLResponseError]
}
