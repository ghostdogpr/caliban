package example.akkahttp.performance

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

object AuthExampleAppPerformance extends App {

  case class AuthToken(value: String)

  type Auth = FiberRef[Option[AuthToken]]

  object AuthInterceptor extends RequestInterceptor[Auth, Auth] {
    override def apply[R <: Auth, A](
      request: ServerRequest
    )(effect: ZIO[R with Auth, TapirResponse, A]): ZIO[R, TapirResponse, A] =
      request.headers.collectFirst {
        case header if header.is("token") => header.value
      } match {
        case Some(token) => ZIO.serviceWithZIO[Auth](_.set(Some(AuthToken(token)))) *> effect
        case _           => ZIO.fail(TapirResponse(StatusCode.Forbidden))
      }
  }

  case class IP(value: String)

  type ClientIP = FiberRef[Option[IP]]

  object ClientIPInterceptor extends RequestInterceptor[ClientIP, ClientIP] {
    override def apply[R <: ClientIP, A](request: ServerRequest)(
      effect: ZIO[R with ClientIP, TapirResponse, A]
    ): ZIO[R with ClientIP, TapirResponse, A] = {
      val ip = request
        .header("X-Forwarded-For")
        .orElse(request.header("X-Real-IP"))
        .orElse(request.header("Remote-Address"))
        .orElse(request.connectionInfo.remote.map(_.getAddress.getHostAddress))
        .map(IP)
      ZIO.serviceWithZIO[ClientIP](_.set(ip)) *> effect
    }
  }

  val schema: GenericSchema[Auth with ClientIP] = new GenericSchema[Auth with ClientIP] {}
  import schema._
  case class Query(token: RIO[Auth, Option[String]], ip: RIO[ClientIP, Option[String]])
  private val resolver                          = RootResolver(
    Query(
      token = ZIO.serviceWithZIO[Auth](_.get).map(_.map(_.value)),
      ip = ZIO.serviceWithZIO[ClientIP](_.get).map(_.map(_.value))
    )
  )
  private val api                               = graphQL(resolver)

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  // Note that we must initialize the runtime with any FiberRefs we intend to
  // pass on so that they are present in the environment for our ContextWrapper(s)
  // For the auth we wrap in an option, but you could just as well use something
  // like AuthToken("__INVALID") or a sealed trait hierarchy with an invalid member
  val initLayer: ZLayer[Any, Nothing, Auth with ClientIP] =
    ZLayer.scoped(FiberRef.make(Option.empty[AuthToken])) ++ ZLayer.scoped(FiberRef.make(Option.empty[IP]))

  implicit val runtime: Runtime[Auth with ClientIP] = Unsafe.unsafe(implicit u => Runtime.unsafe.fromLayer(initLayer))

  val interpreter = Unsafe.unsafe(implicit u => runtime.unsafe.run(api.interpreter).getOrThrow())
  val adapter     = AkkaHttpAdapter.default

  val route =
    path("api" / "graphql") {
      adapter.makeHttpService[Auth with ClientIP, Any, Throwable](interpreter, requestInterceptor = AuthInterceptor)
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
