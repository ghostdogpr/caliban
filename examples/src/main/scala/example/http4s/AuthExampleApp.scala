package example.http4s

import caliban.GraphQL._
import caliban.schema.GenericSchema
import caliban.{ Http4sAdapter, RootResolver }
import com.comcast.ip4s._
import org.http4s.{ HttpRoutes, Response }
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.typelevel.ci.CIString
import zio._
import zio.interop.catz._

object AuthExampleApp extends CatsApp {
  import sttp.tapir.json.circe._

  // Simple service that returns the token coming from the request
  trait Auth {
    def token: String
  }

  type AuthTask[A] = RIO[Auth, A]
  type MyTask[A]   = RIO[Any, A]

  case class MissingToken() extends Throwable

  // http4s middleware that extracts a token from the request and eliminate the Auth layer dependency
  object AuthMiddleware {
    def apply(route: HttpRoutes[AuthTask]): HttpRoutes[MyTask] =
      Http4sAdapter.provideSomeLayerFromRequest[Any, Auth](
        route,
        _.headers.get(CIString("token")) match {
          case Some(value) => ZLayer.succeed(new Auth { override def token: String = value.head.value })
          case None        => ZLayer.fail(MissingToken())
        }
      )
  }

  // http4s error handler to customize the response for our throwable
  object dsl extends Http4sDsl[MyTask]
  import dsl._
  val errorHandler: PartialFunction[Throwable, MyTask[Response[MyTask]]] = { case MissingToken() => Forbidden() }

  // our GraphQL API
  val schema: GenericSchema[Auth] = new GenericSchema[Auth] {}
  import schema.auto._
  case class Query(token: RIO[Auth, String])
  private val resolver            = RootResolver(Query(ZIO.serviceWith[Auth](_.token)))
  private val api                 = graphQL(resolver)

  override def run =
    for {
      interpreter <- api.interpreter
      _           <- EmberServerBuilder
                       .default[MyTask]
                       .withErrorHandler(errorHandler)
                       .withHost(host"localhost")
                       .withPort(port"8088")
                       .withHttpWebSocketApp(wsBuilder =>
                         Router[MyTask](
                           "/api/graphql" -> AuthMiddleware(Http4sAdapter.makeHttpService(interpreter)),
                           "/ws/graphql"  -> AuthMiddleware(Http4sAdapter.makeWebSocketService(wsBuilder, interpreter))
                         ).orNotFound
                       )
                       .build
                       .toScopedZIO *> ZIO.never
    } yield ()
}
