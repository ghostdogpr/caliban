package example.http4s

import caliban.Http4sAdapter
import cats.data.Kleisli
import example.ExampleData._
import example.ExampleService.ExampleService
import example.{ ExampleApi, ExampleService }
import org.http4s.StaticFile
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import zio._
import zio.interop.catz._

object ExampleApp extends App {

  type ExampleTask[A] = RIO[ZEnv with ExampleService, A]

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    ZIO
      .runtime[ZEnv with ExampleService]
      .flatMap(implicit runtime =>
        for {
          interpreter <- ExampleApi.api.interpreter
          _           <- BlazeServerBuilder[ExampleTask]
                           .bindHttp(8088, "localhost")
                           .withHttpWebSocketApp(wsBuilder =>
                             Router[ExampleTask](
                               "/api/graphql" -> CORS.policy(Http4sAdapter.makeHttpService(interpreter)),
                               "/ws/graphql"  -> CORS.policy(Http4sAdapter.makeWebSocketService(wsBuilder, interpreter)),
                               "/graphiql"    -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", None))
                             ).orNotFound
                           )
                           .resource
                           .toManagedZIO
                           .useForever
        } yield ()
      )
      .provideCustomLayer(ExampleService.make(sampleCharacters))
      .exitCode
}
