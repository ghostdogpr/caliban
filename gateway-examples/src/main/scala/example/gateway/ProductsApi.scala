package example.gateway

import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import caliban.{ graphQL, RootResolver }

object ProductsApi extends ExampleApp("Products API", 8081) {
  final case class Product(id: String, name: String, price: Int)
  final case class ProductArgs(id: String)
  final case class Query(product: ProductArgs => Option[Product], products: List[Product])

  private val products = List(Product("caliban", "Caliban", 0), Product("zio", "ZIO", 0))

  val api = graphQL(RootResolver(Query(args => products.find(_.id == args.id), products)))

  def interpreter = api.interpreter
}
