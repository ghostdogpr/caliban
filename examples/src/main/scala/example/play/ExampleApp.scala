package example.play

import akka.actor.ActorSystem
import akka.stream.Materializer
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import caliban.{ GraphQL, PlayAdapter }
import example.ExampleData.sampleCharacters
import example.{ ExampleApi, ExampleService }
import play.api.Mode
import play.api.routing._
import play.api.routing.sird._
import play.core.server.{ AkkaHttpServer, ServerConfig }
import zio.{ Runtime, Scope, ZIO, ZIOAppDefault }

object ExampleApp extends ZIOAppDefault {
  import sttp.tapir.json.play._

  implicit val zioRuntime: Runtime[Any] = Runtime.default

  override def run: ZIO[Scope, Throwable, Unit] =
    (for {
      system      <- ZIO.succeed(ActorSystem()).withFinalizer(sys => ZIO.fromFuture(_ => sys.terminate()).ignore)
      interpreter <- ZIO.serviceWithZIO[GraphQL[Any]](_.interpreter)
      _           <- ZIO.acquireRelease(
                       ZIO.attempt(
                         AkkaHttpServer.fromRouterWithComponents(
                           ServerConfig(
                             mode = Mode.Dev,
                             port = Some(8088),
                             address = "127.0.0.1"
                           )
                         ) { _ =>
                           implicit val mat: Materializer = Materializer(system)
                           Router.from {
                             case req @ POST(p"/api/graphql") =>
                               PlayAdapter.makeHttpService(HttpInterpreter(interpreter)).apply(req)
                             case req @ GET(p"/ws/graphql")   =>
                               PlayAdapter.makeWebSocketService(WebSocketInterpreter(interpreter)).apply(req)
                           }.routes
                         }
                       )
                     )(server => ZIO.attempt(server.stop()).ignore)
      _           <- zio.Console.printLine(
                       "Server online at http://localhost:8088/\nPress RETURN to stop..."
                     ) *> zio.Console.readLine
    } yield ()).provideSome[Scope](ExampleService.make(sampleCharacters), ExampleApi.layer)
}
