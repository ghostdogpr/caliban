package example.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import caliban.{ AkkaHttpAdapter, CalibanError, GraphQL, GraphQLInterpreter }
import example.ExampleData.sampleCharacters
import example.{ ExampleApi, ExampleService }
import sttp.tapir.json.circe._
import zio.{ Runtime, Unsafe, ZIO, ZLayer }

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

object ExampleApp extends App {

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  val layer                                               = ZLayer.make[GraphQL[Any]](ExampleService.make(sampleCharacters), ExampleApi.layer)
  implicit val runtime: Runtime[GraphQL[Any]]             = Unsafe.unsafe(implicit u => Runtime.unsafe.fromLayer(layer))

  val interpreter =
    Unsafe.unsafe(implicit u => runtime.unsafe.run(ZIO.serviceWithZIO[GraphQL[Any]](_.interpreter)).getOrThrow())
  val adapter     = AkkaHttpAdapter.default

  /**
   * curl -X POST \
   * http://localhost:8088/api/graphql \
   * -H 'Host: localhost:8088' \
   * -H 'Content-Type: application/json' \
   * -d '{
   * "query": "query { characters { name }}"
   * }'
   */
  val route =
    path("api" / "graphql") {
      adapter.makeHttpService(HttpInterpreter(interpreter))
    } ~ path("ws" / "graphql") {
      adapter.makeWebSocketService(WebSocketInterpreter(interpreter))
    } ~ path("graphiql") {
      getFromResource("graphiql.html")
    }

  val bindingFuture = Http().newServerAt("localhost", 8088).bind(route)
  println(s"Server online at http://localhost:8088/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete { _ =>
      system.terminate()
    }
}
