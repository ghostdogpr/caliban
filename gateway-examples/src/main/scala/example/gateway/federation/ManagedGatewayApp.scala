package example.gateway.federation

import caliban.QuickAdapter
import caliban.gateway.{ Gateway, Supergraph }
import zio.Config.Secret
import zio.{ Console, ZIO, ZIOAppDefault }

object ManagedGatewayApp extends ZIOAppDefault {
  private case class Config(
    graphRef: String,
    key: Secret
  )

  private val config: zio.Config[Config] =
    (zio.Config.string("GRAPH_REF") zipWith zio.Config.secret("KEY"))(Config.apply)
      .nested("APOLLO")

  def run =
    for {
      cfg         <- ZIO.config(config)
      gateway      = Gateway.fromSupergraph(Supergraph.uplink(cfg.graphRef, cfg.key))
      interpreter <- gateway.interpreter
      _           <- Console.printLine("Managed Federation gateway: http://localhost:8090/graphiql")
      _           <- QuickAdapter(interpreter).runServer(8090, "/graphql", graphiqlPath = Some("/graphiql"))
    } yield ()
}
