package caliban.client

import io.circe.syntax._
import io.circe.{ Encoder, Json }

/**
 * Represents a GraphQL request, containing a query and a map of variables.
 */
case class GraphQLRequest(query: String, variables: Map[String, __Value])

object GraphQLRequest {

  implicit val encoder: Encoder[GraphQLRequest] = (req: GraphQLRequest) =>
    Json.obj(
      "query"     -> Json.fromString(req.query),
      "variables" -> Json.obj(req.variables.map { case (k, v) =>
        k -> v.asJson
      }.toList: _*)
    )

}
