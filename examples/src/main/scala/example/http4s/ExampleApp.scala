package example.http4s

import caliban.Http4sAdapter
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import cats.data.Kleisli
import com.comcast.ip4s._
import example.ExampleData._
import example.ExampleService.ExampleService
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

  type ExampleTask[A] = RIO[ExampleService, A]

  override def run =
    ZIO
      .runtime[ExampleService]
      .flatMap(implicit runtime =>
        for {
          interpreter <- ExampleApi.api.interpreter
          _           <- EmberServerBuilder
                           .default[ExampleTask]
                           .withHost(host"localhost")
                           .withPort(port"8088")
                           .withHttpWebSocketApp(wsBuilder =>
                             Router[ExampleTask](
                               "/api/graphql" -> CORS.policy(Http4sAdapter.makeHttpService(HttpInterpreter(interpreter))),
                               "/ws/graphql"  -> CORS.policy(
                                 Http4sAdapter.makeWebSocketService(wsBuilder, WebSocketInterpreter(interpreter))
                               ),
                               "/graphiql"    -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", None))
                             ).orNotFound
                           )
                           .build
                           .toScopedZIO *> ZIO.never
        } yield ()
      )
      .provideSomeLayer[Scope](ExampleService.make(sampleCharacters))
}
