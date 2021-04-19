package example.http4s

import caliban.GraphQL._
import caliban.schema.GenericSchema
import caliban.{ Http4sAdapter, RootResolver }

import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.{ Router, ServiceErrorHandler }
import org.http4s.util.CaseInsensitiveString
import zio._
import zio.interop.catz._
import zio.interop.catz.implicits._

import scala.concurrent.ExecutionContext

object AuthExampleApp extends CatsApp {

  // Simple service that returns the token coming from the request
  type Auth = Has[Auth.Service]
  object Auth {
    trait Service {
      def token: String
    }
  }
  type AuthTask[A] = RIO[Auth, A]

  case class MissingToken() extends Throwable

  // http4s middleware that extracts a token from the request and eliminate the Auth layer dependency
  object AuthMiddleware {
    def apply(route: HttpRoutes[AuthTask]): HttpRoutes[Task] =
      Http4sAdapter.provideLayerFromRequest(
        route,
        _.headers.get(CaseInsensitiveString("token")) match {
          case Some(value) => ZLayer.succeed(new Auth.Service { override def token: String = value.value })
          case None        => ZLayer.fail(MissingToken())
        }
      )
  }

  // http4s error handler to customize the response for our throwable
  object dsl extends Http4sDsl[Task]
  import dsl._
  val errorHandler: ServiceErrorHandler[Task] = _ => { case MissingToken() => Forbidden() }

  // our GraphQL API
  val schema: GenericSchema[Auth] = new GenericSchema[Auth] {}
  import schema._
  case class Query(token: RIO[Auth, String])
  private val resolver = RootResolver(Query(ZIO.access[Auth](_.get[Auth.Service].token)))
  private val api      = graphQL(resolver)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      interpreter <- api.interpreter
      route       = AuthMiddleware(Http4sAdapter.makeHttpService(interpreter))
      _ <- BlazeServerBuilder[Task](ExecutionContext.global)
            .withServiceErrorHandler(errorHandler)
            .bindHttp(8088, "localhost")
            .withHttpApp(Router[Task]("/api/graphql" -> route).orNotFound)
            .resource
            .toManaged
            .useForever
    } yield ()).exitCode
}
