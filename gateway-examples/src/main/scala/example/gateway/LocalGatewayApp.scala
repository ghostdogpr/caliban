package example.gateway

import caliban.gateway.{ Gateway, Subgraph }
import caliban.schema.GenericSchema
import caliban.{ graphQL, QuickAdapter, RootResolver }
import zio._

object LocalGatewayApp extends ZIOAppDefault with GenericSchema[Any] {
  import auto._

  final case class Query(greeting: String)

  private val localApi = graphQL(RootResolver(Query("Hello from a local subgraph")))
  private val gateway  = Gateway.compose(Subgraph.local("local", localApi))

  def run: ZIO[Any, Throwable, Unit] =
    ZIO.scoped {
      for {
        runtime <- gateway.build
        _       <- Console.printLine("Local gateway: http://localhost:8080/graphiql")
        _       <- QuickAdapter(runtime).runServer(8080, "/graphql", graphiqlPath = Some("/graphiql"))
      } yield ()
    }
}
