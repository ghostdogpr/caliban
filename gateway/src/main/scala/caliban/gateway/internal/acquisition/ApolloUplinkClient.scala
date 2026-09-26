package caliban.gateway.internal.acquisition

import caliban.gateway.TypenameField
import caliban.{ GraphQLRequest, InputValue }
import caliban.gateway.{ SupergraphAcquisitionError, SupergraphUplinkConfig }
import caliban.gateway.SchemaAcquisitionError.InvalidResponse
import caliban.gateway.SupergraphAcquisitionError.UplinkFetchFailed
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.acquisition.RemoteSchemaAcquisition._
import caliban.ResponseValue.ObjectValue
import caliban.Value.{ NullValue, StringValue }
import zio.{ IO, Trace, ZIO }
import zio.http.URL

private[acquisition] object ApolloUplinkClient {

  def fetch(
    endpoint: URL,
    config: SupergraphUplinkConfig,
    ifAfterId: Option[String],
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SupergraphAcquisitionError, UplinkResponse] = {
    val request = uplinkRequest(config.apiKey.stringValue, config.graphRef, ifAfterId)

    // Any non-success status is an unexpected response, so that the loader fails over to the next endpoint.
    // Remote error text must not reach diagnostics, including JSON decoding exceptions.
    fetchData[SupergraphAcquisitionError](endpoint, request, config.acquisition, http)(
      _.isSuccess,
      _ => InvalidResponse("$.errors")
    ).flatMap(data => ZIO.fromEither(decode(data)))
  }

  sealed trait UplinkResponse

  object UplinkResponse {
    final case class Updated(id: String, supergraphSDL: String) extends UplinkResponse
    case object Unchanged                                       extends UplinkResponse
  }

  private def uplinkRequest(apiKey: String, ref: String, ifAfterId: Option[String]): GraphQLRequest =
    GraphQLRequest(
      query = Some(Query),
      operationName = Some(OperationName),
      variables = Some(
        Map(
          "apiKey"    -> StringValue(apiKey),
          "ref"       -> StringValue(ref),
          "ifAfterId" -> ifAfterId.fold[InputValue](NullValue)(StringValue(_))
        )
      )
    )

  private def decode(data: ObjectValue): Either[SupergraphAcquisitionError, UplinkResponse] = {
    val path = "$.data.routerConfig"
    objectField(data, "routerConfig", "$.data").flatMap { routerConfig =>
      string(routerConfig, TypenameField, path).flatMap {
        case "RouterConfigResult" =>
          for {
            id  <- string(routerConfig, "id", path)
            sdl <- string(routerConfig, "supergraphSDL", path)
          } yield UplinkResponse.Updated(id, sdl)
        // The code is a fixed enum and safe to render; the message beside it is remote free text.
        case "FetchError"         => string(routerConfig, "code", path).flatMap(code => Left(UplinkFetchFailed(code)))
        case "Unchanged"          => Right(UplinkResponse.Unchanged)
        case _                    => Left(InvalidResponse(s"$path.$TypenameField"))
      }
    }
  }

  private final val OperationName = "SupergraphSdl"

  private val Query: String =
    s"""query $OperationName($$apiKey: String!, $$ref: String!, $$ifAfterId: ID) {
       |  routerConfig(apiKey: $$apiKey, ref: $$ref, ifAfterId: $$ifAfterId) {
       |    __typename
       |    ... on RouterConfigResult {
       |      id
       |      supergraphSDL
       |    }
       |    ... on FetchError {
       |      code
       |    }
       |  }
       |}""".stripMargin
}
