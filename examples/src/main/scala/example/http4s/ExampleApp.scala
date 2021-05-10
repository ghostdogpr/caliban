package example.http4s

import example.ExampleData._
import example.ExampleService.ExampleService
import example.{ ExampleApi, ExampleService }

import caliban.Http4sAdapter

import cats.data.Kleisli
import cats.effect.Blocker
import org.http4s.StaticFile
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio._
import zio.blocking.Blocking
import zio.interop.catz._

import scala.concurrent.ExecutionContext

object ExampleApp extends App {

  type ExampleTask[A] = RIO[ZEnv with ExampleService, A]

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    ZIO
      .runtime[ZEnv with ExampleService]
      .flatMap(implicit runtime =>
        for {
          blocker     <- ZIO.access[Blocking](_.get.blockingExecutor.asEC).map(Blocker.liftExecutionContext)
          interpreter <- ExampleApi.api.interpreter
          _ <- BlazeServerBuilder[ExampleTask](ExecutionContext.global)
                .bindHttp(8088, "localhost")
                .withHttpApp(
                  Router[ExampleTask](
                    "/api/graphql" -> CORS(Http4sAdapter.makeHttpService(interpreter)),
                    "/ws/graphql"  -> CORS(Http4sAdapter.makeWebSocketService(interpreter)),
                    "/graphiql"    -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", blocker, None))
                  ).orNotFound
                )
                .resource
                .toManaged
                .useForever
        } yield ()
      )
      .provideCustomLayer(ExampleService.make(sampleCharacters))
      .exitCode
}
