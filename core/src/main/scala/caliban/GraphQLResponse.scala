package caliban

import caliban.interop.circe._

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E])

object GraphQLResponse {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]] =
    GraphQLResponseCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
}

private object GraphQLResponseCirce {
  import io.circe._
  import io.circe.syntax._
  val graphQLResponseEncoder: Encoder[GraphQLResponse[Any]] = Encoder
    .instance[GraphQLResponse[Any]] {
      case GraphQLResponse(data, Nil) => Json.obj("data" -> data.asJson)
      case GraphQLResponse(data, errors) =>
        Json.obj(
          "data"   -> data.asJson,
          "errors" -> Json.fromValues(errors.map(err => Json.obj("message" -> Json.fromString(err.toString))))
        )
    }
}
