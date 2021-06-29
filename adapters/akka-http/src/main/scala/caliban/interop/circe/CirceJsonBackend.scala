package caliban.interop.circe

import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import caliban._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.parser._
import io.circe.syntax._

/**
 * Circe json backend for akka-http routes.
 * <br/>
 * Requires `"de.heikoseeberger" %% "akka-http-circe"` to be on the classpath (checked at compile-time).
 *
 * @see [[AkkaHttpAdapter]] for usage example.
 */
final class CirceJsonBackend extends JsonBackend with FailFastCirceSupport {
  def parseHttpRequest(
    query: Option[String],
    op: Option[String],
    vars: Option[String],
    exts: Option[String]
  ): Either[Throwable, GraphQLRequest] = {
    val variablesJs  = vars.flatMap(parse(_).toOption)
    val extensionsJs = exts.flatMap(parse(_).toOption)
    val fields       = query.map(js => "query" -> Json.fromString(js)) ++
      op.map(o => "operationName" -> Json.fromString(o)) ++
      variablesJs.map(js => "variables" -> js) ++
      extensionsJs.map(js => "extensions" -> js)
    Json
      .fromFields(fields)
      .as[GraphQLRequest]
  }

  def encodeGraphQLResponse(r: GraphQLResponse[Any]): String = r.asJson.toString()

  def parseWSMessage(text: String): Either[Throwable, WSMessage] =
    decode[Json](text).map(json =>
      CirceWSMessage(
        json.hcursor.downField("id").success.flatMap(_.value.asString).getOrElse(""),
        json.hcursor.downField("type").success.flatMap(_.value.asString).getOrElse(""),
        json.hcursor.downField("payload")
      )
    )

  def encodeWSResponse[E](id: String, data: ResponseValue, errors: List[E]): String =
    Json
      .obj(
        "id"      -> Json.fromString(id),
        "type"    -> Json.fromString("data"),
        "payload" -> GraphQLResponse(data, errors).asJson
      )
      .noSpaces

  def encodeWSError(id: String, error: String): String =
    Json
      .obj(
        "id"      -> Json.fromString(id),
        "type"    -> Json.fromString("error"),
        "payload" -> Json.obj("message" -> Json.fromString(error))
      )
      .noSpaces

  def reqUnmarshaller: FromEntityUnmarshaller[GraphQLRequest] = implicitly
}
