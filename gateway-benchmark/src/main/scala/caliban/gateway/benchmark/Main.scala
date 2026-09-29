package caliban.gateway.benchmark

import caliban.QuickAdapter
import caliban.gateway.{ Gateway, RemoteGraphQLConfig, Subgraph }
import zio._
import zio.http._

object Main extends ZIOAppDefault {

  private val Port = 5220

  override def run: RIO[Scope, Nothing] =
    Gateway.compose(subgraphs.head, subgraphs.tail: _*).interpreter.flatMap { interpreter =>
      ZIO.logInfo(s"Serving the gateway benchmark on port $Port.") *> QuickAdapter(interpreter)
        .runServer(Port, "/graphql")
    }

  private[benchmark] val subgraphs: ::[Subgraph[Any]] = {
    val config                            = RemoteGraphQLConfig.default.withExecution(_.forwardIncomingHeaders("Authorization"))
    def subgraph(name: String, port: Int) = Subgraph.federation(name, url"http://127.0.0.1/graphql".port(port), config)
    ::(
      subgraph("accounts", 5221),
      List(subgraph("inventory", 5222), subgraph("products", 5223), subgraph("reviews", 5224))
    )
  }
}
