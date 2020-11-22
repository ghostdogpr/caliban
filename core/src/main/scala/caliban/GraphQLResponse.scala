package caliban

import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import caliban.interop.circe._
import caliban.interop.play._
import caliban.interop.zio.IsZIOJsonEncoder

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E], extensions: Option[ObjectValue] = None)

object GraphQLResponse {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]] =
    GraphQLResponseCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def playJsonWrites[F[_]: IsPlayJsonWrites, E]: F[GraphQLResponse[E]] =
    GraphQLResponsePlayJson.graphQLResponseWrites.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder, E]: F[GraphQLResponse[E]] =
    GraphQLResponseZioJson.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
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

private object GraphQLResponseZioJson {
  import zio.json._
  import zio.json.internal.Write

  private def handleError(err: Any): ResponseValue =
    err match {
      case ce: CalibanError => ErrorZioJson.errorToResponseValue(ce)
      case _                => ResponseValue.ObjectValue(List("message" -> StringValue(err.toString)))
    }

  val graphQLResponseEncoder: JsonEncoder[GraphQLResponse[Any]] =
    (a: GraphQLResponse[Any], indent: Option[Int], out: Write) => {
      val responseEncoder = JsonEncoder.map[String, ResponseValue]
      a match {
        case GraphQLResponse(data, Nil, None) =>
          responseEncoder.unsafeEncode(Map("data" -> data), indent, out)
        case GraphQLResponse(data, Nil, Some(extensions)) =>
          responseEncoder.unsafeEncode(
            Map("data" -> data, "extension" -> extensions.asInstanceOf[ResponseValue]),
            indent,
            out
          )
        case GraphQLResponse(data, errors, None) =>
          responseEncoder.unsafeEncode(
            Map("data" -> data, "errors" -> ResponseValue.ListValue(errors.map(handleError))),
            indent,
            out
          )
        case GraphQLResponse(data, errors, Some(extensions)) =>
          responseEncoder.unsafeEncode(
            Map(
              "data"       -> data,
              "errors"     -> ResponseValue.ListValue(errors.map(handleError)),
              "extensions" -> extensions.asInstanceOf[ResponseValue]
            ),
            indent,
            out
          )
      }
    }
}
