package example.gateway.federation

import caliban.federation.v2_5._
import caliban.quick._
import caliban.schema.Schema
import caliban.{ graphQL, RootResolver }
import zio._

import java.util.UUID

object ReviewsApi extends ZIOAppDefault {
  @GQLKey("id", false)
  final case class Product(id: UUID)
  final case class Review(score: Int, body: String, product: Product)
  final case class Query(latestReviews: List[Review])

  implicit val productSchema: Schema[Any, Product] = Schema.gen
  implicit val reviewSchema: Schema[Any, Review]   = Schema.gen
  implicit val querySchema: Schema[Any, Query]     = Schema.gen

  private val productId = UUID.fromString("00000000-0000-0000-0000-000000000001")
  private val api       = graphQL(RootResolver(Query(List(Review(5, "Excellent", Product(productId)))))) @@ federated

  def run: Task[Unit] =
    Console.printLine("Federation reviews API: http://localhost:8089/graphiql") *>
      api.runServer(8089, "/graphql", graphiqlPath = Some("/graphiql"))
}
