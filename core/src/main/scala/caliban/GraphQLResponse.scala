package caliban

import caliban.ResponseValue.ObjectValue
import caliban.interop.circe._

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E], extensions: Option[ObjectValue] = None)

object GraphQLResponse extends GraphQLResponseJsonCompat {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]] =
    caliban.interop.circe.json.GraphQLResponseCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def circeDecoder[F[_]: IsCirceDecoder, E]: F[GraphQLResponse[E]] =
    caliban.interop.circe.json.GraphQLResponseCirce.graphQLResponseDecoder.asInstanceOf[F[GraphQLResponse[E]]]
}
