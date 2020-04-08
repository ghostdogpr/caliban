package caliban.federation

import caliban.ExampleData.sampleCharacters
import caliban.{ ExampleService, Http4sAdapter }
import cats.data.Kleisli
import cats.effect.Blocker
import org.http4s.StaticFile
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import zio._
import zio.blocking.Blocking
import zio.console.putStrLn
import zio.interop.catz._

object ExampleApp1 extends CatsApp {
  type ExampleTask[A] = RIO[ZEnv, A]

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    ExampleService
      .make(sampleCharacters)
      .memoize
      .use(
        layer =>
          for {
            blocker     <- ZIO.access[Blocking](_.get.blockingExecutor.asEC).map(Blocker.liftExecutionContext)
            interpreter <- FederatedApi.Characters.api.interpreter.map(_.provideCustomLayer(layer))
            _ <- BlazeServerBuilder[ExampleTask]
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
          } yield 0
      )
      .catchAll(err => putStrLn(err.toString).as(1))
}
