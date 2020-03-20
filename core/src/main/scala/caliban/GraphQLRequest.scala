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

object GraphQLRequest extends GraphQLRequestPlatformSpecific {
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[GraphQLRequest] =
    GraphQLRequestCirce.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
}

private object GraphQLRequestCirce {
  import io.circe._
  val graphQLRequestDecoder: Decoder[GraphQLRequest] = (c: HCursor) =>
    for {
      query         <- c.downField("query").as[String]
      operationName <- c.downField("operationName").as[Option[String]]
      variables     <- c.downField("variables").as[Option[Map[String, InputValue]]]
    } yield GraphQLRequest(query, operationName, variables)

}
