package example.gateway

import caliban.gateway.{ Gateway, GatewayMetrics, Subgraph }
import caliban.schema.GenericSchema
import caliban.{ graphQL, QuickAdapter, RootResolver }
import sttp.model.Uri
import zio._

object GatewayApp extends ZIOAppDefault {
  private val products = Subgraph.graphql(
    "products",
    Uri.unsafeParse("http://localhost:8081/graphql"),
    ProductsApi.schema
  )

  private val reviews = Subgraph.graphql(
    "reviews",
    Uri.unsafeParse("http://localhost:8082/graphql")
  )

  private val gateway = Gateway.compose(
    products,
    reviews,
    Subgraph.local("gateway", LocalApi.api)
  ) @@ GatewayMetrics.wrapper

  def run: ZIO[Any, Throwable, Unit] =
    ZIO.scoped {
      for {
        runtime <- gateway.build
        _       <- Console.printLine("Gateway: http://localhost:8080/graphiql")
        _       <- QuickAdapter(runtime).runServer(8080, "/graphql", graphiqlPath = Some("/graphiql"))
      } yield ()
    }

  private object LocalApi extends GenericSchema[Any] {
    import auto._

    final case class Query(gatewayMessage: String)

    val api = graphQL(RootResolver(Query("Served by an in-process Caliban subgraph")))
  }
}
