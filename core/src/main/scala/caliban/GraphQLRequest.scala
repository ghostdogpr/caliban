package caliban

import caliban.interop.circe._

/**
 * Represents a GraphQL request, containing a query, an operation name and a map of variables.
 */
case class GraphQLRequest(
  query: String,
  operationName: Option[String],
  variables: Option[Map[String, InputValue]]
)

object GraphQLRequest {
  implicit def circeEncoder[F[_]: IsCirceEncoder]: F[GraphQLRequest] =
    GraphQLRequestCirce.graphQLRequestEncoder.asInstanceOf[F[GraphQLRequest]]
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[GraphQLRequest] =
    GraphQLRequestCirce.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
}

private object GraphQLRequestCirce {
  import io.circe._
  import io.circe.derivation._
  val graphQLRequestEncoder: Encoder[GraphQLRequest] = deriveEncoder[GraphQLRequest]
  val graphQLRequestDecoder: Decoder[GraphQLRequest] = deriveDecoder[GraphQLRequest]
}
