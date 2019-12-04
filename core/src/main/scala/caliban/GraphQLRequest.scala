package caliban

import caliban.interop.circe._

import scala.language.higherKinds

/**
 * Represents a GraphQL request, containing a query, an operation name and a map of variables.
 */
case class GraphQLRequest(
  query: String,
  operationName: Option[String],
  variables: Option[Map[String, InputValue]]
)

object GraphQLRequest {
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[GraphQLRequest] =
    GraphQLRequestCirce.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
}

private object GraphQLRequestCirce {
  import io.circe._
  import io.circe.derivation._
  val graphQLRequestDecoder: Decoder[GraphQLRequest] = deriveDecoder[GraphQLRequest]
}
