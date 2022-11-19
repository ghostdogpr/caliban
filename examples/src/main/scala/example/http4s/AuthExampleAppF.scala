package example.http4s

import caliban.GraphQL._
import caliban.interop.cats.{ CatsInterop, InjectEnv }
import caliban.schema.GenericSchema
import caliban.{ CalibanError, GraphQL, Http4sAdapter, RootResolver }
import cats.data.{ Kleisli, OptionT }
import cats.MonadThrow
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.{ Async, IO, IOApp, Resource }
import cats.effect.std.Dispatcher
import cats.mtl.Local
import cats.mtl.syntax.local._
import org.http4s.HttpApp
import org.http4s.server.Server
import org.http4s.{ HttpRoutes, Request }
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.{ Router, ServiceErrorHandler }
import org.typelevel.ci._
import zio.{ Runtime, ZEnvironment }

/**
 * The examples shows how to utilize contextual interop between cats-effect and ZIO.
 *
 * Run server:
 * examples/runMain example.http4s.AuthExampleAppF
 *
 * Send a request:
 * curl -X POST -H "X-Token: my-token" -d '{"query": "query { token }"}' localhost:8088/api/graphql
 *
 * Response:
 * {"data":{"token":"my-token"}}
 */
object AuthExampleAppF extends IOApp.Simple {

  type AuthLocal[F[_]] = Local[F, AuthInfo]
  object AuthLocal {
    def apply[F[_]](implicit ev: AuthLocal[F]): AuthLocal[F] = ev

    def token[F[_]: MonadThrow](implicit local: AuthLocal[F]): F[AuthInfo.Token] =
      local.ask.flatMap {
        case t: AuthInfo.Token => MonadThrow[F].pure(t)
        case AuthInfo.Empty    => MonadThrow[F].raiseError(MissingToken())
      }
  }

  sealed trait AuthInfo
  object AuthInfo {
    final case object Empty               extends AuthInfo
    final case class Token(token: String) extends AuthInfo
  }

  final case class MissingToken() extends Throwable

  // http4s middleware that extracts a token from the request and executes the request with AuthInfo available in the scope
  object AuthMiddleware {
    private val TokenHeader = ci"X-Token"

    def httpRoutes[F[_]: MonadThrow: AuthLocal](routes: HttpRoutes[F]): HttpRoutes[F] =
      Kleisli { (req: Request[F]) =>
        req.headers.get(TokenHeader) match {
          case Some(token) => routes.run(req).scope(AuthInfo.Token(token.head.value): AuthInfo)
          case None        => OptionT.liftF(MonadThrow[F].raiseError(MissingToken()))
        }
      }
  }

  // Simple service that returns the token coming from the request
  final case class Query[F[_]](token: F[String])

  class GQL[F[_]: MonadThrow: AuthLocal](implicit interop: CatsInterop[F, AuthInfo]) {

    def createGraphQL: GraphQL[AuthInfo] = {
      val schema: GenericSchema[AuthInfo] = new GenericSchema[AuthInfo] {}
      import schema._
      import caliban.interop.cats.implicits._ // summons `Schema[Auth, F[String]]` instance

      graphQL(RootResolver(query))
    }

    private def query: Query[F] =
      Query(
        token = AuthLocal.token[F].map(authInfo => authInfo.token)
      )
  }

  class Api[F[_]: Async: AuthLocal](implicit interop: CatsInterop[F, AuthInfo]) extends Http4sDsl[F] {

    def httpApp(graphQL: GraphQL[AuthInfo]): F[HttpApp[F]] =
      for {
        routes <- createRoutes(graphQL)
      } yield Router("/api/graphql" -> AuthMiddleware.httpRoutes(routes)).orNotFound

    def createRoutes(graphQL: GraphQL[AuthInfo]): F[HttpRoutes[F]] =
      for {
        interpreter <- interop.toEffect(graphQL.interpreter)
      } yield Http4sAdapter.makeHttpServiceF[F, AuthInfo, CalibanError](interpreter)

    // http4s error handler to customize the response for our throwable
    def errorHandler: ServiceErrorHandler[F] = _ => { case MissingToken() => Forbidden() }
  }

  def program[F[_]: Async: AuthLocal](implicit
    runtime: Runtime[AuthInfo],
    injector: InjectEnv[F, AuthInfo]
  ): Resource[F, Server] = {

    def makeHttpServer(httpApp: HttpApp[F], errorHandler: ServiceErrorHandler[F]): Resource[F, Server] =
      BlazeServerBuilder[F]
        .withServiceErrorHandler(errorHandler)
        .bindHttp(8088, "localhost")
        .withHttpApp(httpApp)
        .resource

    Dispatcher.parallel[F].flatMap { dispatcher =>
      implicit val interop: CatsInterop.Contextual[F, AuthInfo] = CatsInterop.contextual(dispatcher)

      val gql = new GQL[F]
      val api = new Api[F]

      for {
        httpApp    <- Resource.eval(api.httpApp(gql.createGraphQL))
        httpServer <- makeHttpServer(httpApp, api.errorHandler)
      } yield httpServer
    }
  }

  def run: IO[Unit] = {
    type Effect[A] = Kleisli[IO, AuthInfo, A]

    implicit val runtime: Runtime[AuthInfo] = Runtime.default.withEnvironment(ZEnvironment(AuthInfo.Empty))

    program[Effect].useForever.run(AuthInfo.Empty).void
  }

}
