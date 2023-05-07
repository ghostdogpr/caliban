package example.http4s

import caliban.{ GraphQL, Http4sAdapter }
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import cats.data.Kleisli
import com.comcast.ip4s._
import example.ExampleData._
import example.{ ExampleApi, ExampleService }
import org.http4s.StaticFile
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import zio._
import zio.interop.catz._

object ExampleApp extends ZIOAppDefault {
  import sttp.tapir.json.circe._

  override def run: ZIO[Scope, Throwable, Unit] =
    (for {
      interpreter <- ZIO.serviceWithZIO[GraphQL[Any]](_.interpreter)
      _           <- EmberServerBuilder
                       .default[Task]
                       .withHost(host"localhost")
                       .withPort(port"8088")
                       .withHttpWebSocketApp(wsBuilder =>
                         Router[Task](
                           "/api/graphql" -> CORS.policy(Http4sAdapter.makeHttpService(HttpInterpreter(interpreter))),
                           "/ws/graphql"  -> CORS.policy(
                             Http4sAdapter.makeWebSocketService(wsBuilder, WebSocketInterpreter(interpreter))
                           ),
                           "/graphiql"    -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", None))
                         ).orNotFound
                       )
                       .build
                       .toScopedZIO
      _           <- Console.printLine("Server online at http://localhost:8088/\nPress RETURN to stop...")
      _           <- Console.readLine
    } yield ())
      .provideSome[Scope](ExampleService.make(sampleCharacters), ExampleApi.layer)
}
