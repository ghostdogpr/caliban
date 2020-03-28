package caliban.interop.play

import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import caliban._
import caliban.interop.play.json.parsingException
import de.heikoseeberger.akkahttpplayjson.PlayJsonSupport
import play.api.libs.json.{ JsObject, JsString, JsValue, Json }

import scala.util.Try

/**
 * Play-json backend for akka-http routes.
 * <br/>
 * Requires `"de.heikoseeberger" %% "akka-http-play-json"` to be on the classpath (checked at compile-time).
 *
 * @see [[AkkaHttpAdapter]] for usage example.
 */
final class PlayJsonBackend extends JsonBackend with PlayJsonSupport {

  private def parseJson(s: String): Try[JsValue] =
    Try(Json.parse(s))

  def parseHttpRequest(query: String, op: Option[String], vars: Option[String]): Either[Throwable, GraphQLRequest] = {
    val variablesJs = vars.flatMap(parseJson(_).toOption)
    val fields = List("query" -> JsString(query)) ++
      op.map(o => "operationName"       -> JsString(o)) ++
      variablesJs.map(js => "variables" -> js)
    JsObject(fields)
      .validate[GraphQLRequest]
      .asEither
      .left
      .map(parsingException)
  }

  def encodeGraphQLResponse(r: GraphQLResponse[Any]): String = Json.toJson(r).toString()

  def parseWSMessage(text: String): Either[Throwable, WSMessage] =
    parseJson(text).toEither.map { json =>
      PlayWSMessage(
        (json \ "id").validate[String].getOrElse(""),
        (json \ "type").validate[String].getOrElse(""),
        (json \ "payload").validate[JsObject].asOpt
      )
    }

  def encodeWSResponse[E](id: String, data: ResponseValue, errors: List[E]): String =
    Json.stringify(
      Json
        .obj(
          "id"      -> id,
          "type"    -> "data",
          "payload" -> GraphQLResponse(data, errors)
        )
    )

  def encodeWSError(id: String, error: Throwable): String =
    Json.stringify(
      Json
        .obj(
          "id"      -> id,
          "type"    -> "complete",
          "payload" -> error.toString
        )
    )

  def reqUnmarshaller: FromEntityUnmarshaller[GraphQLRequest] = implicitly
}
