package example.gateway2

import caliban._
import caliban.federation.EntityResolver
import caliban.federation.v2_5._
import caliban.quick.GraphqlServerOps
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema
import zio._
import zio.query.ZQuery

import java.util.UUID

object ProductsApi extends ZIOAppDefault {
  case class ProductArgs(id: UUID)
  @GQLKey("id")
  case class Product(id: UUID, name: String, price: Int)

  implicit val productSchema: Schema[Any, Product] = Schema.gen

  val resolver = RootResolver(Option.empty[Unit], Option.empty[Unit], Option.empty[Unit])
  val api      = graphQL(resolver).withAdditionalTypes(List(productSchema.toType_())) @@ federated(
    EntityResolver.from[ProductArgs](args => ZQuery.some(Product(args.id, "Product", 100)))
  )

  def run: Task[Unit] =
    api.runServer(8088, "/api/graphql")
}
