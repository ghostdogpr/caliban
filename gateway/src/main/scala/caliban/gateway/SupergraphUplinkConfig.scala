package caliban.gateway

import zio.http._
import zio.Config.Secret

/**
 * Describes the Apollo GraphOS Uplink a supergraph is polled from.
 *
 * The uplink can answer each poll with a `minDelaySeconds` the client is asked to wait. The gateway
 * does not request it: [[Gateway#reloadable]] enforces Apollo's published ten-second floor
 * statically instead, against the fastest jittered interval [[GatewayConfig]] permits, so nothing here
 * throttles a poll dynamically.
 */
final class SupergraphUplinkConfig private (
  val graphRef: String,
  val apiKey: Secret,
  val endpoints: List[URL],
  val acquisition: RemoteGraphQLConfig.Acquisition
) {

  /**
   * Replaces the uplink endpoints, tried in order. Mirrors `withHeaders`: the argument is the whole list.
   */
  def withEndpoints(endpoints: URL*): SupergraphUplinkConfig =
    copy(endpoints = endpoints.toList)

  /**
   * Transforms the settings used to fetch the supergraph from the uplink.
   */
  def withAcquisition(
    configure: RemoteGraphQLConfig.Acquisition => RemoteGraphQLConfig.Acquisition
  ): SupergraphUplinkConfig =
    copy(acquisition = configure(acquisition))

  override def toString: String = s"SupergraphUplinkConfig($graphRef, $apiKey, $endpoints)"

  private[gateway] def diagnostics: List[String] =
    acquisition.diagnostics :::
      check(graphRef.nonEmpty, "Supergraph uplink graph ref must not be empty.") :::
      check(apiKey.value.nonEmpty, "Supergraph uplink apikey must not be empty.") :::
      check(endpoints.nonEmpty, "Supergraph uplink must have at least one endpoint.")

  private def copy(
    endpoints: List[URL] = endpoints,
    acquisition: RemoteGraphQLConfig.Acquisition = acquisition
  ): SupergraphUplinkConfig =
    new SupergraphUplinkConfig(graphRef, apiKey, endpoints, acquisition)
}

object SupergraphUplinkConfig {

  /**
   * Apollo's public uplink endpoints, in the order they are tried.
   */
  val DefaultEndpoints: List[URL] = List(
    url"https://uplink.api.apollographql.com/",
    url"https://aws.uplink.api.apollographql.com/"
  )

  /**
   * Polls the default endpoints for `graphRef`, such as `my-graph@production`, authenticated with a GraphOS API key.
   */
  def apply(graphRef: String, apiKey: Secret): SupergraphUplinkConfig =
    new SupergraphUplinkConfig(graphRef, apiKey, DefaultEndpoints, RemoteGraphQLConfig.Acquisition.default)
}
