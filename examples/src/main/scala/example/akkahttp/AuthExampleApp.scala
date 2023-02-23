package example.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.{ getFromResource, path, _ }
import caliban.GraphQL._
import caliban.interop.tapir.RequestInterceptor
import caliban.interop.tapir.TapirAdapter.TapirResponse
import caliban.schema.GenericSchema
import caliban.{ AkkaHttpAdapter, RootResolver }
import sttp.model.StatusCode
import sttp.tapir.json.circe._
import sttp.tapir.model.ServerRequest
import zio.{ FiberRef, RIO, Runtime, Unsafe, ZIO, ZLayer }

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn
import zio.Scope
import _root_.cats.effect.Fiber

object AuthExampleApp extends App {

  case class AuthToken(value: String)

  type Auth = FiberRef[Option[AuthToken]]

  object AuthInterceptor extends RequestInterceptor[Any, Auth] {
    override def apply[R, A](
      request: ServerRequest
    )(effect: ZIO[R with Auth, TapirResponse, A]): ZIO[R, TapirResponse, A] =
      request.headers.collectFirst {
        case header if header.is("token") => header.value
      } match {
        case Some(token) =>
          effect.provideSomeLayer[R](ZLayer.scoped[Any](FiberRef.make(Option.apply(AuthToken(token)))))
        case _           => ZIO.fail(TapirResponse(StatusCode.Forbidden))
      }
  }

  val schema: GenericSchema[Auth] = new GenericSchema[Auth] {}
  import schema._
  case class Query(token: RIO[Auth, Option[String]])
  private val resolver            = RootResolver(Query(ZIO.serviceWithZIO[Auth](_.get).map(_.map(_.value))))
  private val api                 = graphQL(resolver)

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  // Note that we must initialize the runtime with any FiberRefs we intend to
  // pass on so that they are present in the environment for our ContextWrapper(s)
  // For the auth we wrap in an option, but you could just as well use something
  // like AuthToken("__INVALID") or a sealed trait hierarchy with an invalid member
  // val initLayer: ZLayer[Any, Nothing, FiberRef[Option[AuthToken]]] =
  //   ZLayer.scoped(FiberRef.make(Option.empty[AuthToken]))

  // implicit val runtime: Runtime[Auth] = Unsafe.unsafe(implicit u => Runtime.unsafe.fromLayer(initLayer))
  implicit val runtime: Runtime[Any] = zio.Runtime.default

  val interpreter = Unsafe.unsafe(implicit u => runtime.unsafe.run(api.interpreter).getOrThrow())
  val adapter     = AkkaHttpAdapter.default

  val route =
    path("api" / "graphql") {
      adapter.makeHttpService[Any, Auth, Throwable](interpreter, requestInterceptor = AuthInterceptor)
    } ~ path("graphiql") {
      getFromResource("graphiql.html")
    }

  val bindingFuture = Http().newServerAt("localhost", 8088).bind(route)
  println(s"Server online at http://localhost:8088/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())
}
