package caliban.gateway

import sttp.client4.UriContext
import sttp.model.Uri
import zio.Config.Secret

/**
 * Describes the Apollo GraphOS Uplink a supergraph is polled from.
 *
 * The uplink answers each poll with a `minDelaySeconds` the client is asked to wait. It is selected
 * and deliberately ignored: [[Gateway.reloadable]] enforces Apollo's published ten-second floor
 * statically instead, against the fastest jittered interval [[GatewayConfig]] permits, so nothing here
 * throttles a poll dynamically.
 */
final case class SupergraphUplinkConfig private (
  graphRef: String,
  apiKey: Secret,
  endpoints: List[Uri],
  acquisition: RemoteGraphQLConfig.Acquisition
) {

  /** Replaces the uplink endpoints, tried in order. Mirrors `withHeaders`: the argument is the whole list. */
  def withEndpoints(endpoints: Uri*): SupergraphUplinkConfig =
    copy(endpoints = endpoints.toList)

  def withAcquisition(acquisition: RemoteGraphQLConfig.Acquisition): SupergraphUplinkConfig =
    copy(acquisition = acquisition)

  private[gateway] def diagnostics: List[String] =
    acquisition.diagnostics ::: (if (graphRef.nonEmpty) Nil
                                 else List("Supergraph uplink graph ref must not be empty.")) :::
      (if (apiKey.value.nonEmpty) Nil else List("Supergraph uplink apikey must not be empty.")) :::
      (if (endpoints.nonEmpty) Nil else List("Supergraph uplink must have at least one endpoint."))

}

object SupergraphUplinkConfig {
  val DefaultEndpoints: List[Uri] = List(
    uri"https://uplink.api.apollographql.com/",
    uri"https://aws.uplink.api.apollographql.com/"
  )

  def apply(graphRef: String, apiKey: Secret): SupergraphUplinkConfig =
    SupergraphUplinkConfig(graphRef, apiKey, DefaultEndpoints, RemoteGraphQLConfig.Acquisition.default)
}
