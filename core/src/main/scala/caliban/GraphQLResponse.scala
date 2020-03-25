package caliban

import caliban.ResponseValue.ObjectValue
import caliban.interop.circe._
import caliban.interop.play._

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E], extensions: Option[ObjectValue] = None)

object GraphQLResponse {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]] =
    GraphQLResponseCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites, E]: F[GraphQLResponse[E]] =
    GraphQLResponsePlayJson.graphQLResponseWrites.asInstanceOf[F[GraphQLResponse[E]]]
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
      case ce: CalibanError => ce.asJson
      case _                => Json.obj("message" -> Json.fromString(err.toString))
    }

}

private object GraphQLResponsePlayJson {
  import play.api.libs.json._
  import play.api.libs.json.Json.toJson

  val graphQLResponseWrites: Writes[GraphQLResponse[Any]] = Writes {
    case GraphQLResponse(data, Nil, None) => Json.obj("data" -> data)
    case GraphQLResponse(data, Nil, Some(extensions)) =>
      Json.obj("data" -> data, "extensions" -> extensions.asInstanceOf[ResponseValue])
    case GraphQLResponse(data, errors, None) =>
      Json.obj("data" -> data, "errors" -> JsArray(errors.map(handleError)))
    case GraphQLResponse(data, errors, Some(extensions)) =>
      Json.obj(
        "data"       -> data,
        "errors"     -> JsArray(errors.map(handleError)),
        "extensions" -> extensions.asInstanceOf[ResponseValue]
      )
  }

  private def handleError(err: Any): JsValue =
    err match {
      case ce: CalibanError => toJson(ce)
      case _                => Json.obj("message" -> err.toString)
    }

}
