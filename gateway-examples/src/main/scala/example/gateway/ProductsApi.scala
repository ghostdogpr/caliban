package example.gateway

import caliban.quick._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.GenericSchema
import caliban.{ graphQL, RootResolver }
import zio._

object ProductsApi extends ZIOAppDefault with GenericSchema[Any] {
  import auto._

  final case class Product(id: String, name: String, price: Int)
  final case class ProductArgs(id: String)
  final case class Query(product: ProductArgs => Option[Product], products: List[Product])

  val schema: String =
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

  private val products = List(
    Product("caliban", "Caliban", 0),
    Product("zio", "ZIO", 0)
  )

  private val api = graphQL(
    RootResolver(
      Query(args => products.find(_.id == args.id), products)
    )
  )

  def run: Task[Unit] =
    Console.printLine("Products API: http://localhost:8081/graphiql") *>
      api.runServer(8081, "/graphql", graphiqlPath = Some("/graphiql"))
}
