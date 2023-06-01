package example.ziohttp

import caliban.interop.tapir.ws.Protocol
import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import caliban.{ GraphQL, ZHttpAdapter }
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import zio._
import zio.stream._
import zio.http._

object ExampleApp extends ZIOAppDefault {
  import sttp.tapir.json.circe._

  private val graphiql = Handler.fromStream(ZStream.fromResource("graphiql.html")).toHttp.withDefaultErrorResponse

  override def run: ZIO[Any, Throwable, Unit] =
    (for {
      interpreter <- ZIO.serviceWithZIO[GraphQL[Any]](_.interpreter)
      _           <-
        Server
          .serve(
            Http
              .collectHttp[Request] {
                case _ -> Root / "api" / "graphql" => ZHttpAdapter.makeHttpService(HttpInterpreter(interpreter))
                case _ -> Root / "ws" / "graphql"  =>
                  ZHttpAdapter.makeWebSocketService(WebSocketInterpreter(interpreter))
                case _ -> Root / "graphiql"        => graphiql
              }
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
            .withWebSocketConfig(ZHttpAdapter.defaultWebSocketConfig)
        ),
        Server.live
      )
}
