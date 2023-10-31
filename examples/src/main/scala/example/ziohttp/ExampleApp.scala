package example.ziohttp

import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import caliban.{ GraphQL, ZHttpAdapter }
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import zio._
import zio.http._

object ExampleApp extends ZIOAppDefault {
  import sttp.tapir.json.circe._

  private val graphiql = Handler.fromResource("graphiql.html").sandbox

  override def run: ZIO[Any, Throwable, Unit] =
    (for {
      interpreter <- ZIO.serviceWithZIO[GraphQL[Any]](_.interpreter)
      _           <-
        Server
          .serve(
            Routes(
              Method.ANY / "api" / "graphql" -> ZHttpAdapter.makeHttpService(HttpInterpreter(interpreter)).toHandler,
              Method.ANY / "ws" / "graphql"  ->
                ZHttpAdapter.makeWebSocketService(WebSocketInterpreter(interpreter)).toHandler,
              Method.ANY / "graphiql"        -> graphiql
            ).toHttpApp
          )
      _           <- Console.printLine("Server online at http://localhost:8088/")
      _           <- Console.printLine("Press RETURN to stop...") *> Console.readLine
    } yield ())
      .provide(
        ExampleService.make(sampleCharacters),
        ExampleApi.layer,
        ZLayer.succeed(
          Server.Config.default
            .port(8088)
            .webSocketConfig(ZHttpAdapter.defaultWebSocketConfig)
        ),
        Server.live
      )
}
