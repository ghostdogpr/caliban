package example.quick

import caliban._
import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import zio._
import caliban.quick._

object ExampleApp extends ZIOAppDefault {

  // Only as a convenience of shutting down the server via CLI, don't add to production code!
  private val cliShutdownSignal =
    Console.printLine("Server online at http://localhost:8090/") *>
      Console.printLine("Press RETURN to stop...") *>
      Console.readLine.unit

  private val serve =
    ZIO
      .serviceWithZIO[GraphQL[Any]] {
        _.runServer(
          port = 8090,
          apiPath = "/api/graphql",
          graphiqlPath = Some("/graphiql")
        )
      }
      .provide(
        ExampleService.make(sampleCharacters),
        ExampleApi.layer
      )

  override def run: ZIO[Any, Throwable, Unit] = serve race cliShutdownSignal

}
