package poc.caliban.app

import caliban.Http4sAdapter
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.Router
import org.http4s.{ HttpRoutes, StaticFile }
import poc.caliban.posts.PostService
import zio.magic._
import zio.{ ExitCode, Has, RIO, URIO, ZEnv, ZManaged }

object Main extends zio.App {
  import org.http4s.implicits._
  import zio.interop.catz._

  type R            = ZEnv with Has[PostService]
  type LocalTask[A] = RIO[R, A]

  val server: ZManaged[ZEnv with Has[PostService], Throwable, Unit] =
    for {
      interpreter <- ZManaged.fromEffect(poc.caliban.posts.GraphQLApi.api.interpreter)
      _           <- BlazeServerBuilder[LocalTask]
                       .bindHttp(8080, "0.0.0.0")
                       .withHttpWebSocketApp(builder =>
                         Router[LocalTask](
                           "/api/graphql" -> Http4sAdapter.makeHttpService(interpreter),
                           "/ws/graphql"  -> Http4sAdapter.makeWebSocketService(builder, interpreter),
                           "/graphiql"    -> HttpRoutes.liftF(StaticFile.fromResource("/graphiql.html", None))
                         ).asInstanceOf[HttpRoutes[LocalTask]]
                           .orNotFound // Scala 2.12 is too dumb to infer the type correctly se we need to `asInstanceOf`.
                       )
                       .resource
                       .toManagedZIO
    } yield ()

  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
    server.useForever.exitCode.injectSome(PostService.layer)

}
