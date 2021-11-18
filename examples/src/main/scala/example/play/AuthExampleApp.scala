package example.play

import akka.actor.ActorSystem
import caliban.GraphQL.graphQL
import caliban.interop.tapir.RequestInterceptor
import caliban.schema.GenericSchema
import caliban.{ PlayAdapter, RootResolver }
import play.api.Mode
import play.api.routing._
import play.api.routing.sird._
import play.core.server.{ AkkaHttpServer, ServerConfig }
import sttp.model.StatusCode
import sttp.tapir.json.play._
import sttp.tapir.model.ServerRequest
import zio.blocking.Blocking
import zio.internal.Platform
import zio.random.Random
import zio.stream.ZStream
import zio.{ FiberRef, Has, RIO, Runtime, ZIO }

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn.readLine

object AuthExampleApp extends App {
  case class AuthToken(value: String)

  type Auth = Has[FiberRef[Option[AuthToken]]]

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  object AuthWrapper extends RequestInterceptor[Auth] {
    def apply[R1 <: Auth](request: ServerRequest): ZIO[R1, StatusCode, Unit] =
      request.header("token") match {
        case Some(token) => ZIO.accessM[Auth](_.get.set(Some(AuthToken(token))))
        case None        => ZIO.fail(StatusCode.Forbidden)
      }
  }

  val schema: GenericSchema[Auth] = new GenericSchema[Auth] {}
  import schema._
  case class Query(token: RIO[Auth, Option[String]])
  case class Mutation(x: RIO[Auth, Option[String]])
  case class Subscription(x: ZStream[Auth, Throwable, Option[String]])
  private val resolver            = RootResolver(
    Query(ZIO.accessM[Auth](_.get.get).map(_.map(_.value))),
    Mutation(ZIO.some("foo")),
    Subscription(ZStream.empty)
  )
  private val api                 = graphQL(resolver)

  // Note that we must initialize the runtime with any FiberRefs we intend to
  // pass on so that they are present in the environment for our ResultWrapper(s)
  // For the auth we wrap in an option, but you could just as well use something
  // like AuthToken("__INVALID") or a sealed trait hierarchy with an invalid member
  val initLayer                                                 = FiberRef.make(Option.empty[AuthToken]).toLayer ++ Blocking.live ++ Random.live
  implicit val runtime: Runtime[Auth with Blocking with Random] = Runtime.unsafeFromLayer(initLayer, Platform.default)

  val interpreter = runtime.unsafeRun(api.interpreter)

  val server = AkkaHttpServer.fromRouterWithComponents(
    ServerConfig(
      mode = Mode.Dev,
      port = Some(8088),
      address = "127.0.0.1"
    )
  ) { _ =>
    Router.from {
      case req @ POST(p"/api/graphql") =>
        PlayAdapter.makeHttpService(interpreter, requestInterceptor = AuthWrapper).apply(req)
      case req @ GET(p"/ws/graphql")   => PlayAdapter.makeWebSocketService(interpreter).apply(req)
    }.routes
  }

  println("Server online at http://localhost:8088/\nPress RETURN to stop...")
  readLine()
  server.stop()
}
