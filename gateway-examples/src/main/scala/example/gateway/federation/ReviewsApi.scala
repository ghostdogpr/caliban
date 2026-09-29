package example.gateway.federation

import caliban.federation.v2_5._
import caliban.schema.Schema.auto._
import caliban.{ graphQL, RootResolver }
import example.gateway.ExampleApp

import java.util.UUID

object ReviewsApi extends ExampleApp("Federation reviews API", 8089) {
  @GQLKey("id", false)
  final case class Product(id: UUID)
  final case class Review(score: Int, body: String, product: Product)
  final case class Query(latestReviews: List[Review])

  private val productId = UUID.fromString("00000000-0000-0000-0000-000000000001")

  def interpreter =
    (graphQL(RootResolver(Query(List(Review(5, "Excellent", Product(productId)))))) @@ federated).interpreter
}
