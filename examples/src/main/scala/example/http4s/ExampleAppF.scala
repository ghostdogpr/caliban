package example.http4s

import caliban.interop.cats.implicits._
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import caliban.{ CalibanError, Http4sAdapter }
import cats.data.Kleisli
import cats.effect.std.Dispatcher
import cats.effect.{ ExitCode, IO, IOApp }
import com.comcast.ip4s._
import example.ExampleData.sampleCharacters
import example.{ ExampleApi, ExampleService }
import org.http4s.StaticFile
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import zio.{ Runtime, Unsafe, ZIO }

object ExampleAppF extends IOApp {
  import sttp.tapir.json.circe._

  implicit val zioRuntime: Runtime[ExampleService] =
    Unsafe.unsafe(implicit u => Runtime.unsafe.fromLayer(ExampleService.make(sampleCharacters)))

  val exampleService: ExampleService =
    Unsafe.unsafe(implicit u => zioRuntime.unsafe.run(zioRuntime.run(ZIO.service[ExampleService])).getOrThrow())

  override def run(args: List[String]): IO[ExitCode] =
    Dispatcher.parallel[IO].use { implicit dispatcher =>
      for {
        interpreter <- ExampleApi.makeApi(exampleService).interpreterAsync[IO]
        _           <- EmberServerBuilder
                         .default[IO]
                         .withHost(host"localhost")
                         .withPort(port"8088")
                         .withHttpWebSocketApp(wsBuilder =>
                           Router[IO](
                             "/api/graphql" ->
                               CORS.policy(Http4sAdapter.makeHttpServiceF[IO, Any, CalibanError](HttpInterpreter(interpreter))),
                             "/ws/graphql"  ->
                               CORS.policy(
                                 Http4sAdapter
                                   .makeWebSocketServiceF[IO, Any, CalibanError](wsBuilder, WebSocketInterpreter(interpreter))
                               ),
                             "/graphiql"    ->
                               Kleisli.liftF(StaticFile.fromResource("/graphiql.html", None))
                           ).orNotFound
                         )
                         .build
                         .useForever
      } yield ExitCode.Success
    }
}
