package example.gateway.federation

import caliban.QuickAdapter
import caliban.gateway.{ Gateway, RemoteGraphQLConfig, Supergraph }
import zio.Config.Secret
import zio._

object ManagedGatewayApp extends ZIOAppDefault {
  private final case class ApolloConfig(graphRef: String, key: Secret)

  private val apolloConfig: Config[ApolloConfig] =
    (Config.string("GRAPH_REF") zipWith Config.secret("KEY"))(ApolloConfig.apply).nested("APOLLO")

  private val remoteConfig =
    RemoteGraphQLConfig.default.withExecution(_.forwardIncomingHeaders("Authorization"))

  def run =
    for {
      apollo      <- ZIO.config(apolloConfig)
      gateway      = Gateway.fromSupergraph(
                       Supergraph.uplink(apollo.graphRef, apollo.key).withSubgraphConfig(_ => remoteConfig)
                     )
      interpreter <- gateway.interpreter
      _           <- Console.printLine("Managed Federation gateway: http://localhost:8090/graphiql")
      _           <- QuickAdapter(interpreter).runServer(8090, "/graphql", graphiqlPath = Some("/graphiql"))
    } yield ()
}
