package caliban.client

import io.circe.Encoder
import io.circe.derivation.deriveEncoder

/**
 * Represents a GraphQL request, containing a query and a map of variables.
 */
case class GraphQLRequest(query: String, variables: Map[String, Value])

object GraphQLRequest {

  implicit val encoder: Encoder[GraphQLRequest] = deriveEncoder[GraphQLRequest]

}
