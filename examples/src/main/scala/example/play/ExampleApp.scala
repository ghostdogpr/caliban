package example.play

import akka.actor.ActorSystem
import akka.stream.Materializer
import example.{ ExampleApi, ExampleService }
import example.ExampleData.sampleCharacters
import example.ExampleService.ExampleService
import caliban.PlayAdapter
import play.api.Mode
import play.api.routing._
import play.api.routing.sird._
import play.core.server.{ AkkaHttpServer, ServerConfig }
import sttp.tapir.json.play._
import zio.clock.Clock
import zio.console.{ getStrLn, putStrLn, Console }
import zio.internal.Platform
import zio.{ ExitCode, Runtime, Task, URIO, ZIO, ZManaged }

import scala.io.StdIn.readLine
import zio.blocking.Blocking
import zio.random.Random

import scala.concurrent.ExecutionContextExecutor

object ExampleApp extends zio.App {

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] =
    (for {
      runtime     <- ZManaged.runtime[zio.ZEnv with ExampleService]
      system      <- ZManaged.make(Task.effectTotal(ActorSystem()))(sys => ZIO.fromFuture(_ => sys.terminate()).ignore)
      interpreter <- ExampleApi.api.interpreter.toManaged_
      _           <- ZManaged.makeEffect(
                       AkkaHttpServer.fromRouterWithComponents(
                         ServerConfig(
                           mode = Mode.Dev,
                           port = Some(8088),
                           address = "127.0.0.1"
                         )
                       ) { _ =>
                         implicit val ec: ExecutionContextExecutor               = system.dispatcher
                         implicit val mat: Materializer                          = Materializer(system)
                         implicit val rts: Runtime[zio.ZEnv with ExampleService] = runtime
                         Router.from {
                           case req @ POST(p"/api/graphql") => PlayAdapter.makeHttpService(interpreter).apply(req)
                           case req @ GET(p"/ws/graphql")   => PlayAdapter.makeWebSocketService(interpreter).apply(req)
                         }.routes
                       }
                     )(server => server.stop())
    } yield ())
      .use_(
        putStrLn("Server online at http://localhost:8088/\nPress RETURN to stop...") *> getStrLn
      )
      .exitCode
      .provideCustomLayer(ExampleService.make(sampleCharacters))
}
