package example.gateway

import caliban.quick._
import caliban.schema.GenericSchema
import caliban.{ graphQL, RootResolver }
import zio._

object ReviewsApi extends ZIOAppDefault with GenericSchema[Any] {
  import auto._

  final case class Review(id: String, body: String, productId: String)
  final case class Query(latestReviews: List[Review])

  private val api = graphQL(
    RootResolver(
      Query(
        List(
          Review("1", "Composable and type-safe", "caliban"),
          Review("2", "Structured concurrency all the way down", "zio")
        )
      )
    )
  )

  def run: Task[Unit] =
    Console.printLine("Reviews API: http://localhost:8082/graphiql") *>
      api.runServer(8082, "/graphql", graphiqlPath = Some("/graphiql"))
}
