package caliban

import caliban.GraphQLRequest.{ `apollo-federation-include-trace`, ftv1 }
import caliban.Value.StringValue
import caliban.interop.circe.IsCirceDecoder
import caliban.interop.play.IsPlayJsonReads
import caliban.interop.zio.IsZIOJsonDecoder

/**
 * Represents a GraphQL request, containing a query, an operation name and a map of variables.
 */
case class GraphQLRequest(
  query: Option[String] = None,
  operationName: Option[String] = None,
  variables: Option[Map[String, InputValue]] = None,
  extensions: Option[Map[String, InputValue]] = None
) {

  def withExtension(key: String, value: InputValue): GraphQLRequest =
    copy(extensions = Some(extensions.foldLeft(Map(key -> value))(_ ++ _)))

  def withFederatedTracing                                          =
    withExtension(`apollo-federation-include-trace`, StringValue(ftv1))

}

object GraphQLRequest {
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[GraphQLRequest]     =
    caliban.interop.circe.json.GraphQLRequestCirce.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
  implicit def playJsonReads[F[_]: IsPlayJsonReads]: F[GraphQLRequest]   =
    caliban.interop.play.json.GraphQLRequestPlayJson.graphQLRequestReads.asInstanceOf[F[GraphQLRequest]]
  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[GraphQLRequest] =
    caliban.interop.zio.GraphQLRequestZioJson.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]

  private[caliban] val ftv1                              = "ftv1"
  private[caliban] val `apollo-federation-include-trace` = "apollo-federation-include-trace"
}
