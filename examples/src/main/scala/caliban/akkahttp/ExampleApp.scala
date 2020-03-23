package caliban.akkahttp

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import caliban.ExampleData.sampleCharacters
import caliban.{ AkkaHttpAdapter, ExampleApi, ExampleService }
import zio.{ Runtime, ZEnv }
import caliban.interop.circe.CirceJsonBackend

object ExampleApp extends App {

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  implicit val runtime: Runtime[ZEnv]                     = Runtime.default

  val interpreter = runtime.unsafeRun(
    ExampleService
      .make(sampleCharacters)
      .memoize
      .use(layer => ExampleApi.api.interpreter.map(_.provideCustomLayer(layer)))
  )

  val adapter = new AkkaHttpAdapter(new CirceJsonBackend)

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
      adapter.makeHttpService(interpreter)
    } ~ path("ws" / "graphql") {
      adapter.makeWebSocketService(interpreter)
    } ~ path("graphiql") {
      getFromResource("graphiql.html")
    }

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8088)
  println(s"Server online at http://localhost:8088/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())

}
