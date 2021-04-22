package example.play

import caliban.GraphQL.graphQL
import caliban.PlayAdapter.RequestWrapper
import caliban.schema.GenericSchema
import caliban.{PlayRouter, RootResolver}

import play.api.Mode
import play.api.mvc.{DefaultControllerComponents, RequestHeader, Result, Results}
import play.core.server.{AkkaHttpServer, ServerConfig}
import zio.internal.Platform
import zio.{FiberRef, Has, RIO, Runtime, URIO, ZIO, ZLayer}
import scala.io.StdIn.readLine

import zio.blocking.Blocking
import zio.random.Random

object AuthExampleApp extends App {
  case class AuthToken(value: String)

  type Auth = Has[FiberRef[Option[AuthToken]]]

  object AuthWrapper extends RequestWrapper[Auth] {
    override def apply[R <: Auth](ctx: RequestHeader)(effect: URIO[R, Result]): URIO[R, Result] =
      ctx.headers.get("token") match {
        case Some(token) => ZIO.accessM[Auth](_.get.set(Some(AuthToken(token)))) *> effect
        case _           => ZIO.succeed(Results.Forbidden)
      }
  }

  val schema: GenericSchema[Auth] = new GenericSchema[Auth] {}
  import schema._
  case class Query(token: RIO[Auth, Option[String]])
  private val resolver = RootResolver(Query(ZIO.accessM[Auth](_.get.get).map(_.map(_.value))))
  private val api      = graphQL(resolver)

  // Note that we must initialize the runtime with any FiberRefs we intend to
  // pass on so that they are present in the environment for our ResultWrapper(s)
  // For the auth we wrap in an option, but you could just as well use something
  // like AuthToken("__INVALID") or a sealed trait hierarchy with an invalid member
  val initLayer                       = ZLayer.fromEffect(FiberRef.make(Option.empty[AuthToken])) ++ Blocking.live ++ Random.live
  implicit val runtime: Runtime[Auth with Blocking with Random] = Runtime.unsafeFromLayer(initLayer, Platform.default)

  val interpreter = runtime.unsafeRun(api.interpreter)

  val server = AkkaHttpServer.fromRouterWithComponents(
    ServerConfig(
      mode = Mode.Dev,
      port = Some(8088),
      address = "127.0.0.1"
    )
  ) { components =>
    PlayRouter(
      interpreter,
      DefaultControllerComponents(
        components.defaultActionBuilder,
        components.playBodyParsers,
        components.messagesApi,
        components.langs,
        components.fileMimeTypes,
        components.executionContext
      ),
      requestWrapper = AuthWrapper
    )(runtime, components.materializer).routes
  }

  println("Server online at http://localhost:8088/\nPress RETURN to stop...")
  readLine()
  server.stop()
}
