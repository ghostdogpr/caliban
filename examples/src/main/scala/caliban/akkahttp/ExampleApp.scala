package caliban.akkahttp

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import caliban.ExampleData.sampleCharacters
import caliban.ExampleService.ExampleService
import caliban.interop.circe.AkkaHttpCirceAdapter
import caliban.{ ExampleApi, ExampleService }
import zio.clock.Clock
import zio.console.Console
import zio.internal.Platform
import zio.Runtime

/**
 * There's also an [[caliban.interop.play.AkkaHttpPlayJsonAdapter]] if you use play-json and don't want to have circe dependency
 */
object ExampleApp extends App with AkkaHttpCirceAdapter {

  implicit val system: ActorSystem                        = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  implicit val runtime: Runtime[ExampleService with Console with Clock] =
    Runtime.unsafeFromLayer(ExampleService.make(sampleCharacters) ++ Console.live ++ Clock.live, Platform.default)

  val interpreter = runtime.unsafeRun(ExampleApi.api.interpreter)

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
