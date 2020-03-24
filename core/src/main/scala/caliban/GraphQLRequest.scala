package caliban

import caliban.interop.circe._
import caliban.interop.play.IsPlayJsonReads

/**
 * Represents a GraphQL request, containing a query, an operation name and a map of variables.
 */
case class GraphQLRequest(
  query: String,
  operationName: Option[String],
  variables: Option[Map[String, InputValue]]
)

object GraphQLRequest {
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[GraphQLRequest] =
    GraphQLRequestCirce.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLRequest] =
    GraphQLRequestPlayJson.graphQLRequestReads.asInstanceOf[F[GraphQLRequest]]
}

private object GraphQLRequestCirce {
  import io.circe._
  val graphQLRequestDecoder: Decoder[GraphQLRequest] = (c: HCursor) =>
    for {
      query         <- c.downField("query").as[String]
      operationName <- c.downField("operationName").as[Option[String]]
      variables     <- c.downField("variables").as[Option[Map[String, InputValue]]]
    } yield GraphQLRequest(query, operationName, variables)

}

private object GraphQLRequestPlayJson {
  import play.api.libs.json._
  import play.api.libs.json.Reads._
  import play.api.libs.functional.syntax._

  val graphQLRequestReads: Reads[GraphQLRequest] =
    (
      (JsPath \ "query").read[String] and
        (JsPath \ "operationName").readNullable[String] and
        (JsPath \ "variables").readNullable[Map[String, InputValue]]
    )(GraphQLRequest.apply _)
}
