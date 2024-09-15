package caliban

import caliban.GraphQLRequest.{ `apollo-federation-include-trace`, ftv1 }
import caliban.Value.StringValue
import caliban.interop.tapir.IsTapirSchema
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

/**
 * Represents a GraphQL request, containing a query, an operation name and a map of variables.
 */
case class GraphQLRequest(
  query: Option[String] = None,
  operationName: Option[String] = None,
  variables: Option[Map[String, InputValue]] = None,
  extensions: Option[Map[String, InputValue]] = None,
  @transient isHttpGetRequest: Boolean = false
) { self =>

  def withExtension(key: String, value: InputValue): GraphQLRequest =
    copy(extensions = Some(extensions.foldLeft(Map(key -> value))(_ ++ _)))

  def withFederatedTracing: GraphQLRequest =
    withExtension(`apollo-federation-include-trace`, StringValue(ftv1))

  private[caliban] def asHttpGetRequest: GraphQLRequest =
    new GraphQLRequest(query, operationName, variables, extensions, isHttpGetRequest = true)

  private[caliban] def isEmpty: Boolean =
    operationName.isEmpty && query.isEmpty && extensions.isEmpty
}

object GraphQLRequest {
  private[caliban] implicit val jsoniterCodec: JsonValueCodec[GraphQLRequest] = JsonCodecMaker.make

  implicit def tapirSchema[F[_]: IsTapirSchema]: F[GraphQLRequest] =
    caliban.interop.tapir.schema.requestSchema.asInstanceOf[F[GraphQLRequest]]

  private[caliban] final val ftv1                              = "ftv1"
  private[caliban] final val `apollo-federation-include-trace` = "apollo-federation-include-trace"
}
