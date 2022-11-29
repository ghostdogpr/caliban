package caliban

import caliban.GraphQLRequest.{ `apollo-federation-include-trace`, ftv1 }
import caliban.Value.StringValue
import caliban.interop.circe.{ IsCirceDecoder, IsCirceEncoder }
import caliban.interop.jsoniter.IsJsoniterCodec
import caliban.interop.tapir.IsTapirSchema
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }

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

  def withFederatedTracing: GraphQLRequest =
    withExtension(`apollo-federation-include-trace`, StringValue(ftv1))

}

object GraphQLRequest extends GraphQLRequestJsonCompat {
  implicit def circeDecoder[F[_]: IsCirceDecoder]: F[GraphQLRequest]     =
    caliban.interop.circe.json.GraphQLRequestCirce.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
  implicit def circeEncoder[F[_]: IsCirceEncoder]: F[GraphQLRequest]     =
    caliban.interop.circe.json.GraphQLRequestCirce.graphQLRequestEncoder.asInstanceOf[F[GraphQLRequest]]
  implicit def zioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[GraphQLRequest] =
    caliban.interop.zio.GraphQLRequestZioJson.graphQLRequestDecoder.asInstanceOf[F[GraphQLRequest]]
  implicit def zioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[GraphQLRequest] =
    caliban.interop.zio.GraphQLRequestZioJson.graphQLRequestEncoder.asInstanceOf[F[GraphQLRequest]]
  implicit def tapirSchema[F[_]: IsTapirSchema]: F[GraphQLRequest]       =
    caliban.interop.tapir.schema.requestSchema.asInstanceOf[F[GraphQLRequest]]
  implicit def jsoniterCodec[F[_]: IsJsoniterCodec]: F[GraphQLRequest]   =
    caliban.interop.jsoniter.GraphQLRequestJsoniter.graphQLRequestCodec.asInstanceOf[F[GraphQLRequest]]

  private[caliban] val ftv1                              = "ftv1"
  private[caliban] val `apollo-federation-include-trace` = "apollo-federation-include-trace"
}
