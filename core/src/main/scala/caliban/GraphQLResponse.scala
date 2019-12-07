package caliban

import caliban.interop.circe._

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E])

object GraphQLResponse {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]] =
    GraphQLResponceCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
}

private object GraphQLResponceCirce {
  import io.circe._
  import io.circe.syntax._
  val graphQLResponseEncoder: Encoder[GraphQLResponse[CalibanError]] = Encoder
    .instance[GraphQLResponse[CalibanError]](
      response =>
        Json.obj(
          "data"   -> response.data.asJson,
          "errors" -> Json.fromValues(response.errors.map(err => Json.fromString(err.toString)))
        )
    )
}
