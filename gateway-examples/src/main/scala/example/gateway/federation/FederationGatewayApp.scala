package example.gateway.federation

import caliban.gateway.{ Gateway, Subgraph }
import caliban.QuickAdapter
import zio._
import zio.http._

object FederationGatewayApp extends ZIOAppDefault {
  private val gateway = Gateway.compose(
    Subgraph.federation("products", url"http://localhost:8088/graphql"),
    Subgraph.federation("reviews", url"http://localhost:8089/graphql")
  )

  def run =
    for {
      interpreter <- gateway.interpreter
      _           <- Console.printLine("Federation gateway: http://localhost:8090/graphiql")
      _           <- QuickAdapter(interpreter).runServer(8090, "/graphql", graphiqlPath = Some("/graphiql"))
    } yield ()
}
