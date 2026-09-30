package example.gateway.federation

import caliban.gateway.{ Gateway, RemoteGraphQLConfig, Supergraph, SupergraphUplinkConfig }
import example.gateway.ExampleApp
import zio._

object ManagedGatewayApp extends ExampleApp("Managed Federation gateway", 8090) {
  private val uplinkConfig: Config[SupergraphUplinkConfig] =
    (Config.string("GRAPH_REF") zipWith Config.secret("KEY"))(SupergraphUplinkConfig(_, _)).nested("APOLLO")

  private val remoteConfig =
    RemoteGraphQLConfig.default.withExecution(_.forwardIncomingHeaders("Authorization"))

  def interpreter =
    ZIO.config(uplinkConfig).flatMap { uplink =>
      Gateway.fromSupergraph(Supergraph.uplink(uplink).withSubgraphConfig(_ => remoteConfig)).interpreter
    }
}
