package caliban

import caliban.ResponseValue.ObjectValue
import caliban.interop.circe._
import caliban.interop.jsoniter._
import caliban.interop.play._

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E], extensions: Option[ObjectValue] = None)

object GraphQLResponse {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]] =
    GraphQLResponseCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def jsoniterEncoder[F[_]: IsJsoniterCodec, E]: F[GraphQLResponse[E]] =
    GraphQLResponseJsoniterJson.graphQLResponseCodec.asInstanceOf[F[GraphQLResponse[E]]]
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

private object GraphQLResponseJsoniterJson {
  import com.github.plokhotnyuk.jsoniter_scala.core._
  val graphQLResponseCodec: JsonValueEncoder[GraphQLResponse[Any]] = new JsonValueEncoder[GraphQLResponse[Any]] {
    private def handleError(out: JsonWriter)(err: Any): Unit =
      err match {
        case ce: CalibanError =>
          ErrorJsoniter.errorValueEncoder.encodeValue(ce, out)
        case _ =>
          out.writeObjectStart()
          out.writeKey("message")
          out.writeVal(err.toString)
          out.writeObjectEnd()
      }
    def encodeValue(x: GraphQLResponse[Any], out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeKey("data")
      ResponseValue.jsoniterCodec.encodeValue(x.data, out)
      x.extensions.foreach(x1 => ResponseValue.jsoniterCodec.encodeValue(x1.asInstanceOf[ResponseValue], out))
      Some(x.errors).collect {
        case e if e.nonEmpty => e
      }.foreach { x1 =>
        out.writeKey("errors")
        out.writeArrayStart()
        x1.foreach(handleError(out))
        out.writeArrayEnd()
      }
      out.writeObjectEnd()
    }
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
