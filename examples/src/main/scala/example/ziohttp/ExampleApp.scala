package example.ziohttp

import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import caliban.ZHttpAdapter
import zhttp.http.Middleware.cors
import zio._
import zio.stream._
import zhttp.http._
import zhttp.http.middleware.Cors.CorsConfig
import zhttp.service.Server
import zio.console._

object ExampleApp extends App {
  private val graphiql = Http.fromStream(ZStream.fromResource("graphiql.html"))

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      interpreter <- ExampleApi.api.interpreter
      _           <- Server
                       .start(
                         8088,
                         Http.route[Request] {
                           case _ -> !! / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
                           case _ -> !! / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
                           case _ -> !! / "graphiql"        => graphiql
                         } @@ cors(CorsConfig())
                       )
                       .forever
                       .fork
      _           <- getStrLn
    } yield ())
      .provideCustomLayer(ExampleService.make(sampleCharacters))
      .exitCode
}
