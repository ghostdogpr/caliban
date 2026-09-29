package example.gateway.federation

import caliban.federation.EntityResolver
import caliban.federation.v2_5._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import caliban.{ graphQL, RootResolver }
import example.gateway.ExampleApp
import zio.query.ZQuery

import java.util.UUID

object ProductsApi extends ExampleApp("Federation products API", 8088) {
  final case class ProductArgs(id: UUID)

  @GQLKey("id")
  final case class Product(id: UUID, name: String, price: Int)

  def interpreter =
    (graphQL(RootResolver(Option.empty[Unit], Option.empty[Unit], Option.empty[Unit])) @@
      federated(EntityResolver.from[ProductArgs](args => ZQuery.some(Product(args.id, "Caliban", 0))))).interpreter
}
