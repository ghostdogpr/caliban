package example.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.{ getFromResource, path, _ }
import caliban._
import caliban.interop.tapir.HttpAdapter
import caliban.interop.tapir.TapirAdapter.TapirResponse
import caliban.schema.GenericSchema
import sttp.model.StatusCode
import sttp.tapir.json.circe._
import sttp.tapir.model.ServerRequest
import zio.{ Runtime, URIO, Unsafe, ZIO, ZLayer }

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

object AuthExampleApp extends App {

  case class AuthToken(value: String)

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

  val conf = ZLayer.makeSome[ServerRequest, AuthToken](auth, Configurator.setSkipValidation(true))

  val schema: GenericSchema[AuthToken] = new GenericSchema[AuthToken] {}
  import schema.auto._
  case class Query(token: URIO[AuthToken, String])
  private val resolver                 = RootResolver(Query(ZIO.serviceWith[AuthToken](_.value)))
  private val api                      = graphQL(resolver)

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  implicit val runtime: Runtime[Any]                      = Runtime.default

  val interpreter = Unsafe.unsafe(implicit u => runtime.unsafe.run(api.interpreter).getOrThrow())
  val adapter     = AkkaHttpAdapter.default

  val route =
    path("api" / "graphql") {
      adapter.makeHttpService(HttpAdapter(interpreter).configure(conf))
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
