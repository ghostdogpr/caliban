package example.http4s

import caliban.interop.cats.implicits._
import caliban.{ CalibanError, Http4sAdapter }
import cats.data.Kleisli
import cats.effect.std.Dispatcher
import cats.effect.{ ExitCode, IO, IOApp }
import example.ExampleData.sampleCharacters
import example.ExampleService.ExampleService
import example.{ ExampleApi, ExampleService }
import org.http4s.StaticFile
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import zio.{ Runtime, Unsafe }

object ExampleAppF extends IOApp {

  type MyEnv = ExampleService

  implicit val zioRuntime: Runtime[MyEnv] =
    Unsafe.unsafe(implicit u => Runtime.unsafe.fromLayer(ExampleService.make(sampleCharacters)))

  override def run(args: List[String]): IO[ExitCode] =
    Dispatcher.parallel[IO].use { implicit dispatcher =>
      for {
        interpreter <- ExampleApi.api.interpreterAsync[IO]
        _           <- BlazeServerBuilder[IO]
                         .bindHttp(8088, "localhost")
                         .withHttpWebSocketApp(wsBuilder =>
                           Router[IO](
                             "/api/graphql" ->
                               CORS.policy(Http4sAdapter.makeHttpServiceF[IO, MyEnv, CalibanError](interpreter)),
                             "/ws/graphql"  ->
                               CORS.policy(Http4sAdapter.makeWebSocketServiceF[IO, MyEnv, CalibanError](wsBuilder, interpreter)),
                             "/graphiql"    ->
                               Kleisli.liftF(StaticFile.fromResource("/graphiql.html", None))
                           ).orNotFound
                         )
                         .serve
                         .compile
                         .drain
      } yield ExitCode.Success
    }
}
