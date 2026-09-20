package caliban.gateway

import caliban.gateway.GatewayBuildError.{ SupergraphAcquisitionFailed, SupergraphDecompositionFailed }
import caliban.gateway.internal.SchemaFingerprint
import caliban.gateway.internal.acquisition.SupergraphAcquisition
import caliban.gateway.internal.composition.SupergraphDecomposition
import caliban.parsing.adt.Document
import zio.{ IO, Trace, ZIO }
import zio.http._
import zio.Config.Secret

import java.nio.file.Path

final class Supergraph[-R] private[gateway] (
  private[gateway] val source: Supergraph.Source,
  private[gateway] val config: String => RemoteGraphQLConfig[R],
  private[gateway] val endpoints: String => Option[URL]
) {

  /**
   * Sets the remote GraphQL configuration of each subgraph, looked up by the subgraph name declared in
   * `@join__graph`. Without it, every subgraph uses `RemoteGraphQLConfig.default`. Acquisition settings are
   * unused, since the subgraph schemas come from the supergraph.
   */
  def withSubgraphConfig[R1 <: R](value: String => RemoteGraphQLConfig[R1]): Supergraph[R1] =
    new Supergraph(source, value, endpoints)

  /**
   * Overrides the routing url of each subgraph, looked up by the subgraph name declared in `@join__graph`. A
   * subgraph for which the function returns `None` keeps the url from the supergraph.
   */
  def withSubgraphEndpoint(value: String => Option[URL]): Supergraph[R] = new Supergraph(source, config, value)

  private[gateway] def load(
    loader: SupergraphAcquisition.Loader
  )(implicit trace: Trace): IO[GatewayBuildError, (List[Subgraph[R]], List[String])] =
    for {
      document    <- loader.load.mapError(SupergraphAcquisitionFailed(_))
      projections <- ZIO
                       .fromEither(SupergraphDecomposition.decompose(document))
                       .mapError(SupergraphDecompositionFailed(_))
      subgraphs    = projections.map(projection =>
                       Subgraph.federation(
                         name = projection.graph.name,
                         endpoint = endpoints(projection.graph.name).getOrElse(projection.graph.url),
                         schema = projection.document,
                         config = config(projection.graph.name)
                       )
                     )
    } yield subgraphs -> List(SchemaFingerprint(document))
}

object Supergraph {
  private val HiveCdn: URL = url"https://cdn.graphql-hive.com"

  /**
   * Describes a supergraph from SDL. It is parsed once and never reloaded.
   */
  def sdl(value: String): Supergraph[Any] = fromSource(Source.Sdl(value))

  /**
   * Describes a supergraph from an already parsed schema document. It is never reloaded.
   */
  def parsed(value: Document): Supergraph[Any] = fromSource(Source.Parsed(value))

  /**
   * Describes a supergraph read from a file. A reloadable gateway reads the file again on every poll.
   */
  def file(path: Path): Supergraph[Any] = fromSource(Source.File(path))

  /**
   * Describes a supergraph fetched with a GET request. It follows no redirects unless `config` allows them, and the
   * configured headers go to `endpoint` only, never to a redirect target. Polls send the last `ETag` in
   * `If-None-Match`.
   */
  def http(
    endpoint: URL,
    config: RemoteGraphQLConfig.Acquisition = RemoteGraphQLConfig.Acquisition.default
  ): Supergraph[Any] =
    fromSource(Source.Http(endpoint, config))

  /**
   * Describes a supergraph polled from the Apollo GraphOS Uplink for a graph ref such as `my-graph@production`,
   * using Apollo's default uplink endpoints. A reloadable gateway must poll it no more often than every ten
   * seconds.
   */
  def uplink(graphRef: String, apiKey: Secret): Supergraph[Any] = uplink(SupergraphUplinkConfig(graphRef, apiKey))

  /**
   * Describes a supergraph polled from the Apollo GraphOS Uplink with custom endpoints or acquisition settings.
   */
  def uplink(config: SupergraphUplinkConfig): Supergraph[Any] = fromSource(Source.Uplink(config))

  /**
   * Describes a supergraph fetched from the Hive CDN for a target, authenticated with a CDN access key. It
   * follows at most two redirects.
   */
  def hive(targetId: String, cdnKey: Secret, cdn: URL = HiveCdn): Supergraph[Any] =
    http(
      cdn / "artifacts" / "v1" / targetId / "supergraph",
      RemoteGraphQLConfig.Acquisition.default
        .withHeaders(Header.Custom("X-Hive-CDN-Key", cdnKey.stringValue))
        .withMaxRedirects(2)
    )

  private def fromSource(source: Source): Supergraph[Any] =
    new Supergraph(source, _ => RemoteGraphQLConfig.default, _ => None)

  private[gateway] sealed trait Source {
    def refreshable: Boolean =
      this match {
        case _: Source.Sdl | _: Source.Parsed                   => false
        case _: Source.File | _: Source.Http | _: Source.Uplink => true
      }
  }

  private[gateway] object Source {
    final case class Sdl(value: String)                                           extends Source
    final case class Parsed(value: Document)                                      extends Source
    final case class File(path: Path)                                             extends Source
    final case class Http(endpoint: URL, config: RemoteGraphQLConfig.Acquisition) extends Source
    final case class Uplink(config: SupergraphUplinkConfig)                       extends Source
  }

}
