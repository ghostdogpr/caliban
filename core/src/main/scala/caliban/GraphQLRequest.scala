package caliban

import caliban.interop.circe._
import caliban.interop.jsoniter._
import caliban.interop.play.IsPlayJsonReads

/**
 * Represents a GraphQL request, containing a query, an operation name and a map of variables.
 */
case class GraphQLRequest(
  query: Option[String] = None,
  operationName: Option[String] = None,
  variables: Option[Map[String, InputValue]] = None,
  extensions: Option[Map[String, InputValue]] = None
)

object GraphQLRequest {
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[GraphQLRequest] =
    GraphQLRequestCirce.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
  implicit def jsoniterCodec[F[_]: IsJsoniterCodec]: F[GraphQLRequest] =
    GraphQLRequestJsoniter.graphQLRequestCodec.asInstanceOf[F[GraphQLRequest]]
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLRequest] =
    GraphQLRequestPlayJson.graphQLRequestReads.asInstanceOf[F[GraphQLRequest]]
}

private object GraphQLRequestCirce {
  import io.circe._
  val graphQLRequestDecoder: Decoder[GraphQLRequest] = (c: HCursor) =>
    for {
      query         <- c.downField("query").as[Option[String]]
      operationName <- c.downField("operationName").as[Option[String]]
      variables     <- c.downField("variables").as[Option[Map[String, InputValue]]]
      extensions    <- c.downField("extensions").as[Option[Map[String, InputValue]]]
    } yield GraphQLRequest(query, operationName, variables, extensions)

}

private object GraphQLRequestJsoniter {
  import com.github.plokhotnyuk.jsoniter_scala.core._
  import com.github.plokhotnyuk.jsoniter_scala.macros._
  val graphQLRequestCodec: JsonValueCodec[GraphQLRequest] = JsonCodecMaker.make
}

private object GraphQLRequestPlayJson {
  import play.api.libs.json._

  val graphQLRequestReads: Reads[GraphQLRequest] = Json.reads[GraphQLRequest]
}
