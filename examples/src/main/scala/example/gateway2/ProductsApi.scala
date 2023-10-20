package example.gateway2

import caliban._
import caliban.federation.EntityResolver
import caliban.federation.v2_5._
import caliban.interop.tapir.HttpInterpreter
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema
import sttp.tapir.json.circe._
import zio._
import zio.http._
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
    api.interpreter
      .flatMap(interpreter =>
        Server.serve(
          Http.collectHttp[Request] { case _ -> Root / "api" / "graphql" =>
            ZHttpAdapter.makeHttpService(HttpInterpreter(interpreter))
          }
        )
      )
      .provide(ZLayer.succeed(Server.Config.default.port(8088)), Server.live)
}
