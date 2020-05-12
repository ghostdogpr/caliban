package caliban

import caliban.interop.play.json.parsingException
import play.api.libs.json.{ JsObject, JsString, JsValue, Json }
import play.api.http.Writeable
import play.api.libs.json.Writes

import scala.util.Try

trait PlayJson {

  implicit def writableGraphQLResponse[E](implicit wr: Writes[GraphQLResponse[E]]): Writeable[GraphQLResponse[E]] =
    Writeable.writeableOf_JsValue.map(wr.writes)

  private def parseJson(s: String): Try[JsValue] =
    Try(Json.parse(s))

  def parseHttpRequest(
    query: String,
    op: Option[String],
    vars: Option[String],
    exts: Option[String]
  ): Either[Throwable, GraphQLRequest] = {
    val variablesJs  = vars.flatMap(parseJson(_).toOption)
    val extensionsJs = exts.flatMap(parseJson(_).toOption)
    val fields = List("query" -> JsString(query)) ++
      op.map(o => "operationName"         -> JsString(o)) ++
      variablesJs.map(js => "variables"   -> js) ++
      extensionsJs.map(js => "extensions" -> js)
    JsObject(fields)
      .validate[GraphQLRequest]
      .asEither
      .left
      .map(parsingException)
  }

  def encodeGraphQLResponse(r: GraphQLResponse[Any]): String = Json.toJson(r).toString()

  def parseWSMessage(text: String): Either[Throwable, PlayWSMessage] =
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
}
