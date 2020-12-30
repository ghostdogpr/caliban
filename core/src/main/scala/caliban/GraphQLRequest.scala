package caliban

import caliban.interop.circe.IsCirceDecoder
import caliban.interop.play.IsPlayJsonReads
import caliban.interop.zio.IsZIOJsonDecoder

/**
 * Represents a GraphQL request, containing a query, an operation name and a map of variables.
 */
case class GraphQLRequest(
  query: Option[String] = None,
  operationName: Option[String] = None,
  variables: Option[Map[String, InputValue]] = None,
  extensions: Option[Map[String, InputValue]] = None
)

object GraphQLRequest {
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[GraphQLRequest] =
    caliban.interop.circe.json.GraphQLRequestCirce.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLRequest] =
    caliban.interop.play.json.GraphQLRequestPlayJson.graphQLRequestReads.asInstanceOf[F[GraphQLRequest]]
  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[GraphQLRequest] =
    caliban.interop.zio.GraphQLRequestZioJson.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
}
