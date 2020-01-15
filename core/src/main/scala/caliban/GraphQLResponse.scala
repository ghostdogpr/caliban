package caliban

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.ObjectValue
import caliban.interop.circe._

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E], extensions: Option[ObjectValue] = None)

object GraphQLResponse {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]] =
    GraphQLResponseCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
}

private object GraphQLResponseCirce {
  import io.circe._
  import io.circe.syntax._
  val graphQLResponseEncoder: Encoder[GraphQLResponse[Any]] = Encoder
    .instance[GraphQLResponse[Any]] {
      case GraphQLResponse(data, Nil, None) => Json.obj("data" -> data.asJson)
      case GraphQLResponse(data, Nil, Some(extensions)) =>
        Json.obj("data" -> data.asJson, "extensions" -> extensions.asInstanceOf[ResponseValue].asJson)
      case GraphQLResponse(data, errors, None) =>
        Json.obj("data" -> data.asJson, "errors" -> Json.fromValues(errors.map(handleError)))
      case GraphQLResponse(data, errors, Some(extensions)) =>
        Json.obj(
          "data"       -> data.asJson,
          "errors"     -> Json.fromValues(errors.map(handleError)),
          "extensions" -> extensions.asInstanceOf[ResponseValue].asJson
        )
    }

  private def handleError(err: Any): Json =
    err match {
      case ExecutionError(_, path, location, _) if path.nonEmpty =>
        val locationJson =
          location.fold(Json.obj())(
            l => Json.obj("location" -> Json.obj("column" -> l.column.asJson, "line" -> l.line.asJson))
          )
        Json.obj(
          "message" -> Json.fromString(err.toString),
          "path" -> Json
            .fromValues(path.map {
              case Left(value)  => Json.fromString(value)
              case Right(value) => Json.fromInt(value)
            })
            .deepMerge(locationJson)
        )
      case _ => Json.obj("message" -> Json.fromString(err.toString))
    }

}
