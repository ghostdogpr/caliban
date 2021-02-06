package caliban

import caliban.ResponseValue.ObjectValue
import caliban.interop.circe._
import caliban.interop.play._
import caliban.interop.zio.IsZIOJsonEncoder

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E], extensions: Option[ObjectValue] = None)

object GraphQLResponse {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]]     =
    caliban.interop.circe.json.GraphQLResponseCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites, E]: F[GraphQLResponse[E]] =
    caliban.interop.play.json.GraphQLResponsePlayJson.graphQLResponseWrites.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder, E]: F[GraphQLResponse[E]] =
    caliban.interop.zio.GraphQLResponseZioJson.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
}
