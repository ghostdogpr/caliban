package example.federation

import example.federation.FederationData.characters.sampleCharacters
import example.federation.FederationData.episodes.sampleEpisodes

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

object FederatedApp extends CatsApp {
  type ExampleTask[A] = RIO[ZEnv, A]

  val service1 = CharacterService
    .make(sampleCharacters)
    .memoize
    .use(layer =>
      for {
        blocker     <- ZIO.access[Blocking](_.get.blockingExecutor.asEC).map(Blocker.liftExecutionContext)
        interpreter <- FederatedApi.Characters.api.interpreter.map(_.provideCustomLayer(layer))
        _ <- BlazeServerBuilder[ExampleTask](ExecutionContext.global)
              .bindHttp(8089, "localhost")
              .withHttpApp(
                Router[ExampleTask](
                  "/api/graphql" -> CORS(Http4sAdapter.makeHttpService(interpreter)),
                  "/graphiql"    -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", blocker, None))
                ).orNotFound
              )
              .resource
              .toManaged
              .useForever
      } yield ()
    )

  val service2 = EpisodeService
    .make(sampleEpisodes)
    .memoize
    .use(layer =>
      for {
        blocker     <- ZIO.access[Blocking](_.get.blockingExecutor.asEC).map(Blocker.liftExecutionContext)
        interpreter <- FederatedApi.Episodes.api.interpreter.map(_.provideCustomLayer(layer))
        _ <- BlazeServerBuilder[ExampleTask](ExecutionContext.global)
              .bindHttp(8088, "localhost")
              .withHttpApp(
                Router[ExampleTask](
                  "/api/graphql" -> CORS(Http4sAdapter.makeHttpService(interpreter)),
                  "/graphiql"    -> Kleisli.liftF(StaticFile.fromResource("/graphiql.html", blocker, None))
                ).orNotFound
              )
              .resource
              .toManaged
              .useForever
      } yield ()
    )

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (service1 race service2).exitCode
}
