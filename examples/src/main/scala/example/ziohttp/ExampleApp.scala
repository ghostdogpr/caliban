package example.zhttp

import example.ExampleData._
import example.ExampleService.ExampleService
import example.{ ExampleApi, ExampleService }

import zio._
import zhttp._
import zhttp.http._
import caliban.ZHTTPAdapter

object ExampleApp extends App {
  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    val routes = Http.route {
      case "/api/graphql" => ZHttpAdapter.makeHttpService(interpreter)
      case "/ws/graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
    }

    (for {
      interpreter <- ExampleApi.api.interpreter
      _           <- Server.start(8088, routes).forever
    } yield ())
      .provideCustomLayer(ExampleService.make(sampleCharacters))
      .exitCode
  }
}
