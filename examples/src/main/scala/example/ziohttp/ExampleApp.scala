package example.ziohttp

import example.ExampleData._
import example.{ ExampleApi, ExampleService }

import caliban.ZHttpAdapter
import zio._
import zio.stream._
import zio.http._

object ExampleApp extends ZIOAppDefault {
  import sttp.tapir.json.circe._

  private val graphiql = Handler.fromStream(ZStream.fromResource("graphiql.html")).toHttp

  override def run =
    (for {
      interpreter <- ExampleApi.api.interpreter
      _           <- Server
                       .serve(
                         Http
                           .collectHttp[Request] {
                             case _ -> !! / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
                             case _ -> !! / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
                             case _ -> !! / "graphiql"        => graphiql
                           }
                           .withDefaultErrorResponse
                       )
    } yield ())
      .provide(ExampleService.make(sampleCharacters), Server.default)
      .exitCode
}
