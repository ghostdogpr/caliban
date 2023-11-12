package example.quick

import caliban._
import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import zio._
import zio.http._

object ExampleApp extends ZIOAppDefault {

  // Only as a convenience of shutting down the server via CLI, don't add to production code!
  private val cliShutdownSignal =
    Console.printLine("Server online at http://localhost:8090/") *>
      Console.printLine("Press RETURN to stop...") *>
      Console.readLine.unit

  private val serve =
    QuickAdapter
      .runServer[Any](
        port = 8090,
        api = Root / "api" / "graphql",
        graphiql = Some(Root / "graphiql")
      )
      .provide(
        ExampleService.make(sampleCharacters),
        ExampleApi.layer
      )

  override def run: ZIO[Any, Throwable, Unit] = serve race cliShutdownSignal

}
