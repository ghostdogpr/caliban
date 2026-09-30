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
final case class SupergraphUplinkConfig private (
  graphRef: String,
  apiKey: Secret,
  endpoints: ::[URL],
  acquisition: RemoteGraphQLConfig.Acquisition
) {

  /**
   * Replaces the uplink endpoints, tried in order. Mirrors `withHeaders`: the argument is the whole list.
   */
  def withEndpoints(endpoint: URL, fallbacks: URL*): SupergraphUplinkConfig =
    copy(endpoints = ::(endpoint, fallbacks.toList))

  /**
   * Transforms the settings used to fetch the supergraph from the uplink.
   */
  def withAcquisition(
    configure: RemoteGraphQLConfig.Acquisition => RemoteGraphQLConfig.Acquisition
  ): SupergraphUplinkConfig =
    copy(acquisition = configure(acquisition))

  private[gateway] def diagnostics: List[String] =
    acquisition.diagnostics :::
      check(graphRef.nonEmpty, "Supergraph uplink graph ref must not be empty.") :::
      check(apiKey.value.nonEmpty, "Supergraph uplink apikey must not be empty.")
}

object SupergraphUplinkConfig {

  /**
   * Apollo's public uplink endpoints, in the order they are tried.
   */
  val DefaultEndpoints: ::[URL] = ::(
    url"https://uplink.api.apollographql.com/",
    url"https://aws.uplink.api.apollographql.com/" :: Nil
  )

  /**
   * Polls the default endpoints for `graphRef`, such as `my-graph@production`, authenticated with a GraphOS API key.
   */
  def apply(graphRef: String, apiKey: Secret): SupergraphUplinkConfig =
    new SupergraphUplinkConfig(graphRef, apiKey, DefaultEndpoints, RemoteGraphQLConfig.Acquisition.default)
}
