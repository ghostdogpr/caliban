package example.finch

import example.ExampleData.sampleCharacters
import example.ExampleService.ExampleService
import example.{ ExampleApi, ExampleService }

import caliban.FinchAdapter

import com.twitter.io.{ Buf, BufReader, Reader }
import com.twitter.util.Await
import io.finch.Endpoint
import zio.clock.Clock
import zio.console.Console
import zio.internal.Platform
import zio.interop.catz._
import zio.{ Runtime, Task }

object ExampleApp extends App with Endpoint.Module[Task] {

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
  import com.twitter.finagle.Http
  import io.finch._
  import io.finch.circe._

  val endpoint = "api" :: "graphql" :: FinchAdapter.makeHttpService(interpreter)

  val graphiqlBuf = {
    val stream = getClass.getResourceAsStream("/graphiql.html")
    BufReader.readAll(Reader.fromStream(stream))
  }

  val grapihql: Endpoint[Task, Buf] = get("graphiql") {
    graphiqlBuf.map(Ok)
  }

  val services = Bootstrap.serve[Application.Json](endpoint).serve[Text.Html](grapihql).toService

  val server = Http.server.serve(":8088", services)

  println(s"Server online at http://localhost:8088/\nPress RETURN to stop...")
  Await.ready(server)

}
