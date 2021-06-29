package example.play

import example.{ ExampleApi, ExampleService }
import example.ExampleData.sampleCharacters
import example.ExampleService.ExampleService

import caliban.PlayRouter
import play.api.Mode
import play.api.mvc.DefaultControllerComponents
import play.core.server.{ AkkaHttpServer, ServerConfig }
import zio.clock.Clock
import zio.console.Console
import zio.internal.Platform
import zio.Runtime
import scala.io.StdIn.readLine

import zio.blocking.Blocking
import zio.random.Random

object ExampleApp extends App {

  implicit val runtime: Runtime[ExampleService with Console with Clock with Blocking with Random] =
    Runtime.unsafeFromLayer(ExampleService.make(sampleCharacters) ++ Console.live ++ Clock.live ++ Random.live ++ Blocking.live, Platform.default)

  val interpreter = runtime.unsafeRun(ExampleApi.api.interpreter)

  val server = AkkaHttpServer.fromRouterWithComponents(
    ServerConfig(
      mode = Mode.Dev,
      port = Some(8088),
      address = "127.0.0.1"
    )
  ) { components =>
    PlayRouter(
      interpreter,
      DefaultControllerComponents(
        components.defaultActionBuilder,
        components.playBodyParsers,
        components.messagesApi,
        components.langs,
        components.fileMimeTypes,
        components.executionContext
      )
    )(runtime, components.materializer).routes
  }

  println("Server online at http://localhost:8088/\nPress RETURN to stop...")
  readLine()
  server.stop()

}
