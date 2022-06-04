package example.ziohttp

import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import caliban.ZHttpAdapter
import zhttp.http.Middleware.cors
import zio._
import zio.stream._
import zio.http._
import zio.Console

object ExampleApp extends ZIOAppDefault {
  import sttp.tapir.json.circe._

  private val graphiql = Handler.fromStream(ZStream.fromResource("graphiql.html")).toHttp

  override def run =
    (for {
      interpreter <- ExampleApi.api.interpreter
      _           <- Server
                       .serve(
                         Http
                           .collectRoute[Request] {
                             case _ -> !! / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
                             case _ -> !! / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
                             case _ -> !! / "graphiql"        => graphiql
                           }
                           .withDefaultErrorResponse
                       )
      _           <- Console.printLine("Server online at http://localhost:8088/")
      _           <- Console.printLine("Press RETURN to stop...") *> Console.readLine
    } yield ())
      .provide(ExampleService.make(sampleCharacters), Server.default)
      .exitCode
}
