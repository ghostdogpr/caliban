package example.play

import akka.actor.ActorSystem
import caliban._
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import caliban.interop.tapir.TapirAdapter.TapirResponse
import caliban.schema.GenericSchema
import play.api.Mode
import play.api.routing._
import play.api.routing.sird._
import play.core.server.{ AkkaHttpServer, ServerConfig }
import sttp.model.StatusCode
import sttp.tapir.json.play._
import sttp.tapir.model.ServerRequest
import zio.stream.ZStream
import zio.{ FiberRef, RIO, Runtime, ULayer, Unsafe, ZIO, ZLayer }

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn.readLine

object AuthExampleApp extends App {
  case class AuthToken(value: String)

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  val auth =
    ZLayer {
      for {
        request   <- ZIO.service[ServerRequest]
        authToken <- request.headers.collectFirst {
                       case header if header.is("token") => header.value
                     } match {
                       case Some(token) => ZIO.succeed(AuthToken(token))
                       case _           => ZIO.fail(TapirResponse(StatusCode.Forbidden))
                     }
      } yield authToken
    }

  val schema: GenericSchema[AuthToken] = new GenericSchema[AuthToken] {}
  import schema.auto._
  case class Query(token: RIO[AuthToken, String])
  case class Mutation(x: RIO[AuthToken, String])
  case class Subscription(x: ZStream[AuthToken, Throwable, String])
  private val resolver                 = RootResolver(
    Query(ZIO.serviceWith[AuthToken](_.value)),
    Mutation(ZIO.succeed("foo")),
    Subscription(ZStream.empty)
  )
  private val api                      = graphQL(resolver)

  implicit val runtime: Runtime[Any] = Runtime.default

  val interpreter = Unsafe.unsafe(implicit u => runtime.unsafe.run(api.interpreter).getOrThrow())

  val server = AkkaHttpServer.fromRouterWithComponents(
    ServerConfig(
      mode = Mode.Dev,
      port = Some(8088),
      address = "127.0.0.1"
    )
  ) { _ =>
    Router.from {
      case req @ POST(p"/api/graphql") =>
        PlayAdapter.makeHttpService(HttpInterpreter(interpreter).configure(auth)).apply(req)
      case req @ GET(p"/ws/graphql")   =>
        PlayAdapter.makeWebSocketService(WebSocketInterpreter(interpreter).configure(auth)).apply(req)
    }.routes
  }

  println("Server online at http://localhost:8088/\nPress RETURN to stop...")
  readLine()
  server.stop()
}
