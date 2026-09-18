package example.gateway

import caliban.gateway.{ Gateway, GatewayMetrics, Subgraph }
import caliban.schema.GenericSchema
import caliban.{ graphQL, QuickAdapter, RootResolver }
import zio._
import zio.http._

object GatewayApp extends ZIOAppDefault with GenericSchema[Any] {
  import auto._

  final case class Query(gatewayMessage: String)

  private val localApi = graphQL(RootResolver(Query("Served by an in-process Caliban subgraph")))

  private val productsSchema =
    """
      |type Query {
      |  product(id: String!): Product
      |  products: [Product!]!
      |}
      |
      |type Product {
      |  id: String!
      |  name: String!
      |  price: Int!
      |}
      |""".stripMargin

  private val gateway = Gateway.compose(
    Subgraph.graphql("products", url"http://localhost:8081/graphql", productsSchema),
    Subgraph.graphql("reviews", url"http://localhost:8082/graphql"),
    Subgraph.local("gateway", localApi)
  ) @@ GatewayMetrics.hooks

  def run =
    for {
      interpreter <- gateway.interpreter
      _           <- Console.printLine("Gateway: http://localhost:8080/graphiql")
      _           <- QuickAdapter(interpreter).runServer(8080, "/graphql", graphiqlPath = Some("/graphiql"))
    } yield ()
}
