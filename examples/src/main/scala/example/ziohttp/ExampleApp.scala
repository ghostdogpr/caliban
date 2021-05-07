package example.zhttp

import example.ExampleData._
import example.{ ExampleApi, ExampleService }

import zio._
import zhttp.http._
import zhttp.service.Server
import caliban.ZHttpAdapter

object ExampleApp extends App {
  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      interpreter <- ExampleApi.api.interpreter
      _           <- Server
                       .start(
                         8088,
                         Http.route {
                           case _ -> Root / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
                           case _ -> Root / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
                         }
                       )
                       .forever
    } yield ())
      .provideCustomLayer(ExampleService.make(sampleCharacters))
      .exitCode
}
