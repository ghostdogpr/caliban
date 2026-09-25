package caliban.gateway.internal.acquisition

import caliban.GraphQLRequest
import caliban.gateway.{ RemoteGraphQLConfig, ServiceField, SubgraphAcquisitionError }
import caliban.gateway.SubgraphAcquisitionError.FederationErrors
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.acquisition.RemoteSchemaAcquisition._
import caliban.parsing.adt.Document
import caliban.parsing.Parser
import zio.{ IO, Trace, ZIO }
import zio.http.URL

private[acquisition] object FederationClient {

  def fetch(
    endpoint: URL,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SubgraphAcquisitionError, Document] =
    fetchData[SubgraphAcquisitionError](endpoint, Request, config, http)(
      isGraphQLResponse,
      FederationErrors(_)
    ).flatMap { data =>
      ZIO.fromEither(for {
        service  <- objectField(data, ServiceField, "$.data")
        sdl      <- string(service, "sdl", s"$$.data.$ServiceField")
        document <- parseWithinDepth(sdl, config.maxParsingDepth)(Parser.parseQuery)
      } yield document)
    }

  private final val OperationName = "__CalibanGatewayServiceSchema"
  private val Query               = s"query $OperationName { _service { sdl } }"
  private val Request             = GraphQLRequest(query = Some(Query), operationName = Some(OperationName))
}
