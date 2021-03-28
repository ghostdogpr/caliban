package caliban.interop.ziojson

import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import caliban._
import de.heikoseeberger.akkahttpziojson.ZioJsonSupport
import zio.Chunk
import zio.json._
import zio.json.ast.Json

/**
 * zio-json backend for akka-http routes.
 * <br/>
 * Requires `"de.heikoseeberger" %% "akka-http-zio-json"` to be on the classpath (checked at compile-time).
 *
 * @see [[AkkaHttpAdapter]] for usage example.
 */
final class ZioJsonBackend extends JsonBackend with ZioJsonSupport {

  def parseHttpRequest(
    query: Option[String],
    op: Option[String],
    vars: Option[String],
    exts: Option[String]
  ): Either[Throwable, GraphQLRequest] = {
    val variablesJs  = vars.fold[Either[String, Option[Json]]](Right(None))(_.fromJson[Json].map(Some(_)))
    val extensionsJs = exts.fold[Either[String, Option[Json]]](Right(None))(_.fromJson[Json].map(Some(_)))

    val req = for {
      varJs  <- variablesJs
      extJs  <- extensionsJs
      fields  = query.map(js => "query" -> Json.Str(js)) ++
                  op.map(o => "operationName" -> Json.Str(o)) ++
                  varJs.map(js => "variables" -> js) ++
                  extJs.map(js => "extensions" -> js)
      result <- JsonDecoder[GraphQLRequest].fromJsonAST(Json.Obj(Chunk.fromIterable(fields)))
    } yield result

    req.left.map(new RuntimeException(_))
  }

  def encodeGraphQLResponse(r: GraphQLResponse[Any]): String = r.toJson

  def parseWSMessage(text: String): Either[Throwable, WSMessage] =
    text.fromJson[ZioWSMessage].left.map(new RuntimeException(_))

  def encodeWSResponse[E](id: String, data: ResponseValue, errors: List[E]): String =
    Json
      .Obj(
        "id"      -> Json.Str(id),
        "type"    -> Json.Str("data"),
        "payload" -> GraphQLResponse(data, errors).toJsonAST.getOrElse(Json.Obj())
      )
      .toJson

  def encodeWSError(id: String, error: String): String =
    Json
      .Obj(
        "id"      -> Json.Str(id),
        "type"    -> Json.Str("error"),
        "payload" -> Json.Obj("message" -> Json.Str(error))
      )
      .toJson

  def reqUnmarshaller: FromEntityUnmarshaller[GraphQLRequest] = implicitly
}
