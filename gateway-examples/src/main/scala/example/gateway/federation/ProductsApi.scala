package example.gateway.federation

import caliban.federation.EntityResolver
import caliban.federation.v2_5._
import caliban.quick._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema
import caliban.{ graphQL, RootResolver }
import zio._
import zio.query.ZQuery

import java.util.UUID

object ProductsApi extends ZIOAppDefault {
  final case class ProductArgs(id: UUID)

  @GQLKey("id")
  final case class Product(id: UUID, name: String, price: Int)

  implicit val productSchema: Schema[Any, Product] = Schema.gen

  private val api =
    graphQL(RootResolver(Option.empty[Unit], Option.empty[Unit], Option.empty[Unit]))
      .withAdditionalTypes(List(productSchema.toType_())) @@
      federated(
        EntityResolver.from[ProductArgs](args => ZQuery.some(Product(args.id, "Caliban", 0)))
      )

  def run: Task[Unit] =
    Console.printLine("Federation products API: http://localhost:8088/graphiql") *>
      api.runServer(8088, "/graphql", graphiqlPath = Some("/graphiql"))
}
