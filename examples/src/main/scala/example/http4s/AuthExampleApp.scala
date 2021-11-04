package example.http4s

import caliban.GraphQL._
import caliban.schema.GenericSchema
import caliban.{ Http4sAdapter, RootResolver }
import caliban.interop.tapir.TapirAdapter._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.{ Router, ServiceErrorHandler }
import org.typelevel.ci.CIString
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.interop.catz._

object AuthExampleApp extends CatsApp {

  // Simple service that returns the token coming from the request
  type Auth = Has[Auth.Service]
  object Auth {
    trait Service {
      def token: String
    }
  }
  type AuthTask[A] = RIO[Auth with Clock with Blocking, A]
  type MyTask[A] = RIO[Clock with Blocking, A]

  case class MissingToken() extends Throwable

  // http4s middleware that extracts a token from the request and eliminate the Auth layer dependency
  object AuthMiddleware {
    def apply(route: HttpRoutes[AuthTask]): HttpRoutes[MyTask] =
      Http4sAdapter.provideSomeLayerFromRequest[Clock with Blocking, Auth](
        route,
        _.headers.get(CIString("token")) match {
          case Some(value) => ZLayer.succeed(new Auth.Service { override def token: String = value.head.value })
          case None        => ZLayer.fail(MissingToken())
        }
      )
  }

  // http4s error handler to customize the response for our throwable
  object dsl extends Http4sDsl[MyTask]
  import dsl._
  val errorHandler: ServiceErrorHandler[MyTask] = _ => { case MissingToken() => Forbidden() }

  // our GraphQL API
  val schema: GenericSchema[Auth] = new GenericSchema[Auth] {}
  import schema._
  case class Query(token: RIO[Auth, String])
  private val resolver            = RootResolver(Query(ZIO.access[Auth](_.get[Auth.Service].token)))
  private val api                 = graphQL(resolver)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      interpreter <- api.interpreter
      _           <- BlazeServerBuilder[MyTask]
                       .withServiceErrorHandler(errorHandler)
                       .bindHttp(8088, "localhost")
                       .withHttpApp(
                         Router[MyTask](
                           "/api/graphql" -> AuthMiddleware(Http4sAdapter.makeHttpService(interpreter)),
                           "/ws/graphql"  -> AuthMiddleware(Http4sAdapter.makeWebSocketService(interpreter))
                         ).orNotFound
                       )
                       .resource
                       .toManagedZIO
                       .useForever
    } yield ()).exitCode
}
