package example.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import caliban.AkkaHttpAdapter
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import example.ExampleData.sampleCharacters
import example.ExampleService.ExampleService
import example.{ ExampleApi, ExampleService }
import sttp.tapir.json.circe._
import zio.{ Runtime, Unsafe }

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

object ExampleApp extends App {

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  implicit val runtime: Runtime[ExampleService]           =
    Unsafe.unsafe(implicit u => Runtime.unsafe.fromLayer(ExampleService.make(sampleCharacters)))

  val interpreter = Unsafe.unsafe(implicit u => runtime.unsafe.run(ExampleApi.api.interpreter).getOrThrow())
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
