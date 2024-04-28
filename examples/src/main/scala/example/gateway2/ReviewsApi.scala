package example.gateway2

import caliban._
import caliban.federation.v2_5._
import caliban.interop.tapir.HttpInterpreter
import caliban.quick.GraphqlServerOps
import caliban.schema.Schema
import sttp.tapir.json.circe._
import zio._
import zio.http._

import java.util.UUID

object ReviewsApi extends ZIOAppDefault {
  @GQLKey("id", false)
  case class Product(id: UUID)
  case class Review(score: Int, description: String, product: Product)
  case class Query(latestReviews: List[Review])

  implicit val productSchema: Schema[Any, Product] = Schema.gen
  implicit val reviewSchema: Schema[Any, Review]   = Schema.gen
  implicit val querySchema: Schema[Any, Query]     = Schema.gen

  val resolver = RootResolver(Query(List(Review(5, "Great", Product(UUID.randomUUID())))))
  val api      = graphQL(resolver) @@ federated

  def run: Task[Unit] =
    api.runServer(8089, "/api/graphql")
}
