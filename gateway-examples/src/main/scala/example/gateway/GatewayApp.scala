package example.gateway

import caliban.gateway.{ Gateway, GatewayMetrics, Subgraph }
import caliban.schema.GenericSchema
import caliban.{ graphQL, QuickAdapter, RootResolver }
import sttp.client4.UriContext
import zio._

object GatewayApp extends ZIOAppDefault {
  private val products = Subgraph.graphql(
    "products",
    uri"http://localhost:8081/graphql",
    ProductsApi.schema
  )

  private val reviews = Subgraph.graphql(
    "reviews",
    uri"http://localhost:8082/graphql"
  )

  private val gateway = Gateway.compose(
    products,
    reviews,
    Subgraph.local("gateway", LocalApi.api)
  ) @@ GatewayMetrics.wrapper

  def run =
    for {
      interpreter <- gateway.interpreter
      _           <- Console.printLine("Gateway: http://localhost:8080/graphiql")
      _           <- QuickAdapter(interpreter).runServer(8080, "/graphql", graphiqlPath = Some("/graphiql"))
    } yield ()

  private object LocalApi extends GenericSchema[Any] {
    import auto._

    final case class Query(gatewayMessage: String)

    val api = graphQL(RootResolver(Query("Served by an in-process Caliban subgraph")))
  }
}
