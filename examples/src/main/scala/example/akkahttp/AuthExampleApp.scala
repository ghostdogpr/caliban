package example.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import caliban.GraphQL._
import caliban.{ AkkaHttpAdapter, RootResolver }
import caliban.interop.tapir.RequestInterceptor
import caliban.interop.tapir.TapirAdapter.TapirResponse
import caliban.schema.GenericSchema
import sttp.model.StatusCode
import sttp.tapir.json.circe._
import sttp.tapir.model.ServerRequest
import zio.{ RIO, Runtime, Unsafe, ZIO, ZLayer }

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

object AuthExampleApp extends App {

  case class AuthToken(value: String)

  type Auth = AuthToken

  object AuthInterceptor extends RequestInterceptor[Any, Auth] {
    override def apply[R, A](
      request: ServerRequest
    )(effect: ZIO[R with Auth, TapirResponse, A]): ZIO[R, TapirResponse, A] =
      request.headers.collectFirst {
        case header if header.is("token") => header.value
      } match {
        case Some(token) =>
          effect.provideSomeLayer[R](ZLayer.succeed[Auth](AuthToken(token)))
        case _           => ZIO.fail(TapirResponse(StatusCode.Forbidden))
      }
  }

  case class IP(value: String)

  type ClientIP = Option[IP]

  object ClientIPInterceptor extends RequestInterceptor[Any, ClientIP] {
    override def apply[R, A](request: ServerRequest)(
      effect: ZIO[R with ClientIP, TapirResponse, A]
    ): ZIO[R, TapirResponse, A] =
      for {
        ip       <- ZIO.attempt {
                      request
                        .header("X-Forwarded-For")
                        .orElse(request.header("X-Real-IP"))
                        .orElse(request.header("Remote-Address"))
                        .orElse(request.connectionInfo.remote.map(_.getAddress.getHostAddress))
                        .map(IP)
                    }.orDie
        clientIP <- effect.provideSomeLayer[R](ZLayer.succeed[ClientIP](ip))
      } yield clientIP
  }

  val schema: GenericSchema[Auth with ClientIP] = new GenericSchema[Auth with ClientIP] {}
  import schema._
  case class Query(token: RIO[Auth, String], ip: RIO[ClientIP, String])
  private val resolver                          = RootResolver(
    Query(
      token = ZIO.serviceWith[Auth](_.value),
      ip = ZIO.serviceWith[ClientIP](_.map(_.value).getOrElse("no ip"))
    )
  )
  private val api                               = graphQL(resolver)

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  // This is the runtime needed in order to instantiate the route
  implicit val runtime: Runtime[Any] = zio.Runtime.default

  val interpreter = Unsafe.unsafe(implicit u => runtime.unsafe.run(api.interpreter).getOrThrow())
  val adapter     = AkkaHttpAdapter.default

  // The interceptors will eliminate the part of the environment
  // which is needed at runtime in order to evaluate the API routes
  val interceptors: RequestInterceptor[Any, Auth with ClientIP] = AuthInterceptor |+| ClientIPInterceptor

  val route =
    path("api" / "graphql") {
      adapter.makeHttpService[Any, Auth with ClientIP, Throwable](
        interpreter,
        requestInterceptor = interceptors
      )
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
