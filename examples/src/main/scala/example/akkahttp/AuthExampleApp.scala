package example.akkahttp

import caliban.AkkaHttpAdapter.ContextWrapper
import caliban.GraphQL._
import caliban.RootResolver
import caliban.interop.circe.AkkaHttpCirceAdapter
import caliban.schema.GenericSchema

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ HttpResponse, StatusCodes }
import akka.http.scaladsl.server.Directives.{ getFromResource, path, _ }
import akka.http.scaladsl.server.RequestContext
import zio.internal.Platform
import zio.{ FiberRef, Has, RIO, Runtime, URIO, ZIO, ZLayer }
import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

import zio.blocking.Blocking
import zio.random.Random

object AuthExampleApp extends App with AkkaHttpCirceAdapter {

  case class AuthToken(value: String)

  type Auth = Has[FiberRef[Option[AuthToken]]]

  object AuthWrapper extends ContextWrapper[Auth, HttpResponse] {
    override def apply[R <: Auth, A >: HttpResponse](
      ctx: RequestContext
    )(effect: URIO[R, A]): URIO[R, A] =
      ctx.request.headers.collectFirst {
        case header if header.name.toLowerCase == "token" => header.value
      } match {
        case Some(token) => ZIO.accessM[Auth](_.get.set(Some(AuthToken(token)))) *> effect
        case _           => ZIO.succeed(HttpResponse(StatusCodes.Forbidden))
      }
  }

  val schema: GenericSchema[Auth] = new GenericSchema[Auth] {}
  import schema._
  case class Query(token: RIO[Auth, Option[String]])
  private val resolver = RootResolver(Query(ZIO.accessM[Auth](_.get.get).map(_.map(_.value))))
  private val api      = graphQL(resolver)

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  // Note that we must initialize the runtime with any FiberRefs we intend to
  // pass on so that they are present in the environment for our ContextWrapper(s)
  // For the auth we wrap in an option, but you could just as well use something
  // like AuthToken("__INVALID") or a sealed trait hierarchy with an invalid member
  val initLayer                       = ZLayer.fromEffect(FiberRef.make(Option.empty[AuthToken])) ++ Blocking.live ++ Random.live
  implicit val runtime: Runtime[Auth with Blocking with Random] = Runtime.unsafeFromLayer(initLayer, Platform.default)

  val interpreter = runtime.unsafeRun(api.interpreter)

  val route =
    path("api" / "graphql") {
      adapter.makeHttpService(interpreter, contextWrapper = AuthWrapper)
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
