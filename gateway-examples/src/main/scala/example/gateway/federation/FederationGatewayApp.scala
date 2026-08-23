package example.gateway.federation

import caliban.gateway.{ Gateway, Subgraph }
import caliban.QuickAdapter
import sttp.model.Uri
import zio._

object FederationGatewayApp extends ZIOAppDefault {
  private val gateway = Gateway.compose(
    Subgraph.federation("products", Uri.unsafeParse("http://localhost:8088/graphql")),
    Subgraph.federation("reviews", Uri.unsafeParse("http://localhost:8089/graphql"))
  )

  def run: ZIO[Any, Throwable, Unit] =
    ZIO.scoped {
      for {
        runtime <- gateway.build
        _       <- Console.printLine("Federation gateway: http://localhost:8090/graphiql")
        _       <- QuickAdapter(runtime).runServer(8090, "/graphql", graphiqlPath = Some("/graphiql"))
      } yield ()
    }
}
