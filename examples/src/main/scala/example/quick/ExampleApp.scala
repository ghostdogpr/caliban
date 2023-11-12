package example.quick

import caliban._
import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import zio._
import zio.http._

object ExampleApp extends ZIOAppDefault {

  override def run: ZIO[Any, Throwable, Unit] =
    (for {
      app <- QuickAdapter.app[Any](
               api = Root / "api" / "graphql",
               graphiql = Some(Root / "graphiql")
             )
      _   <-
        Server.serve(app).race {
          Console.printLine("Server online at http://localhost:8090/") *>
            Console.printLine("Press RETURN to stop...") *>
            Console.readLine
        }
    } yield ()).provide(
      ExampleService.make(sampleCharacters),
      ExampleApi.layer,
      Server.defaultWithPort(8090),
      QuickAdapter.default[Any]
    )
}
