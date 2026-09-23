package caliban.gateway

import zio.http._
import zio.Config.Secret

/**
 * Describes the Apollo GraphOS Uplink a supergraph is polled from.
 *
 * The uplink answers each poll with a `minDelaySeconds` the client is asked to wait. It is selected
 * and deliberately ignored: [[Gateway.reloadable]] enforces Apollo's published ten-second floor
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

  def withAcquisition(acquisition: RemoteGraphQLConfig.Acquisition): SupergraphUplinkConfig =
    copy(acquisition = acquisition)

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
  val DefaultEndpoints: List[URL] = List(
    url"https://uplink.api.apollographql.com/",
    url"https://aws.uplink.api.apollographql.com/"
  )

  def apply(graphRef: String, apiKey: Secret): SupergraphUplinkConfig =
    new SupergraphUplinkConfig(graphRef, apiKey, DefaultEndpoints, RemoteGraphQLConfig.Acquisition.default)
}
