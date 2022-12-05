package example.jsoniter

import caliban.interop.tapir.TapirAdapter
import caliban.{ CalibanError, GraphQLInterpreter }
import example.ExampleData._
import example.ExampleService.ExampleService
import example.{ ExampleApi, ExampleService }
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }
import zhttp.http._
import zhttp.service.Server
import zio._
import zio.stream._

/**
 * Notice: The jsoniter interop can be used only on JDK versions 11+.
 *
 * If using jsoniter with a schema that contains recursive types, make sure to limit the maximum depth of queries using
 * the `maxDepth` wrapper to a value of 512 or less
 */
object ExampleApp extends ZIOAppDefault {
  import sttp.tapir.json.jsoniter._ // Required

  private def httpRoutes(interpreter: GraphQLInterpreter[ExampleService, CalibanError]) = {
    val endpoints = TapirAdapter.makeHttpService[ExampleService, CalibanError](interpreter)
    ZioHttpInterpreter(ZioHttpServerOptions.default[ExampleService]).toHttp(endpoints)
  }

  private val graphiql = Http.fromStream(ZStream.fromResource("graphiql.html"))

  override def run =
    (for {
      interpreter <- ExampleApi.api.interpreter
      _           <- Server
                       .start(
                         8088,
                         Http.collectHttp[Request] {
                           case _ -> !! / "api" / "graphql" => httpRoutes(interpreter)
                           case _ -> !! / "graphiql"        => graphiql
                         }
                       )
                       .forever
    } yield ())
      .provideLayer(ExampleService.make(sampleCharacters))
      .exitCode
}
