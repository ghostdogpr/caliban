package example.ziohttp

import example.ExampleData._
import example.{ ExampleApi, ExampleService }

import caliban.ZHttpAdapter
import zio._
import zio.stream._
import zhttp.http._
import zhttp.service.Server

object ExampleApp extends ZIOAppDefault {
  import sttp.tapir.json.circe._

  private val graphiql = Http.fromStream(ZStream.fromResource("graphiql.html"))

  override def run =
    (for {
      interpreter <- ExampleApi.api.interpreter
      _           <- Server
                       .start(
                         8088,
                         Http.collectHttp[Request] {
                           case _ -> !! / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
                           case _ -> !! / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
                           case _ -> !! / "graphiql"        => graphiql
                         }
                       )
                       .forever
    } yield ())
      .provideLayer(ExampleService.make(sampleCharacters))
      .exitCode
}
