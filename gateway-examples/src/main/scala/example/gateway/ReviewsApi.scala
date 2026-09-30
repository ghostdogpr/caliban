package example.gateway

import caliban.schema.Schema.auto._
import caliban.{ graphQL, RootResolver }

object ReviewsApi extends ExampleApp("Reviews API", 8082) {
  final case class Review(id: String, body: String, productId: String)
  final case class Query(latestReviews: List[Review])

  def interpreter =
    graphQL(
      RootResolver(
        Query(
          List(
            Review("1", "Composable and type-safe", "caliban"),
            Review("2", "Structured concurrency all the way down", "zio")
          )
        )
      )
    ).interpreter
}
