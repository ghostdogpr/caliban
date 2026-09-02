package caliban.gateway

import caliban.parsing.adt.Document
import sttp.client4.UriContext
import sttp.model.{ Header, Uri }
import zio.Config.Secret

import java.nio.file.Path

final class Supergraph[-R] private[gateway] (
  private[gateway] val source: Supergraph.Source,
  private[gateway] val config: String => RemoteGraphQLConfig[R],
  private[gateway] val endpoints: String => Option[Uri]
) {
  def withSubgraphConfig[R1 <: R](value: String => RemoteGraphQLConfig[R1]): Supergraph[R1] =
    new Supergraph(source, value, endpoints)

  def withSubgraphEndpoint(value: String => Option[Uri]): Supergraph[R] = new Supergraph(
    source,
    config,
    value
  )
}

object Supergraph {
  def sdl(value: String): Supergraph[Any]      =
    new Supergraph(Source.Sdl(value), _ => RemoteGraphQLConfig.default, _ => None)
  def parsed(value: Document): Supergraph[Any] =
    new Supergraph(Source.Parsed(value), _ => RemoteGraphQLConfig.default, _ => None)
  def file(path: Path): Supergraph[Any]        =
    new Supergraph(Source.File(path), _ => RemoteGraphQLConfig.default, _ => None)
  def http(
    endpoint: Uri,
    config: RemoteGraphQLConfig.Acquisition = RemoteGraphQLConfig.Acquisition.default
  ): Supergraph[Any] =
    new Supergraph(Source.Http(endpoint, config), _ => RemoteGraphQLConfig.default, _ => None)
  def uplink(
    graphRef: String,
    apiKey: Secret
  ): Supergraph[Any] = uplink(SupergraphUplinkConfig(graphRef, apiKey))
  def uplink(
    config: SupergraphUplinkConfig
  ): Supergraph[Any] =
    new Supergraph(Source.Uplink(config), _ => RemoteGraphQLConfig.default, _ => None)

  def hive(
    targetId: String,
    cdnKey: Secret,
    cdn: Uri = uri"https://cdn.graphql-hive.com"
  ): Supergraph[Any] =
    new Supergraph(
      Source.Http(
        cdn.addPath("artifacts", "v1", targetId, "supergraph"),
        RemoteGraphQLConfig.Acquisition.default
          .withHeaders(Header("X-Hive-CDN-Key", cdnKey.stringValue))
          .withMaxRedirects(2)
      ),
      _ => RemoteGraphQLConfig.default,
      _ => None
    )

  private[gateway] sealed trait Source { def refreshable: Boolean }
  private[gateway] object Source       {
    final case class Sdl(value: String)                     extends Source { override val refreshable: Boolean = false }
    final case class Parsed(value: Document)                extends Source { override val refreshable: Boolean = false }
    final case class File(path: Path)                       extends Source { override val refreshable: Boolean = true  }
    final case class Http(
      endpoint: Uri,
      config: RemoteGraphQLConfig.Acquisition = RemoteGraphQLConfig.Acquisition.default
    ) extends Source {
      override val refreshable: Boolean = true
    }
    final case class Uplink(config: SupergraphUplinkConfig) extends Source { override val refreshable: Boolean = true  }
  }

}
