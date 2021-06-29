package example.derivation

import zio._
import zio.duration._
import zio.stream._
import zhttp.http._
import zhttp.service.Server
import caliban.{RootResolver, ZHttpAdapter}
import caliban.schema.Schema
import ExampleApi.{Character, sampleCharacters}
import caliban.GraphQL.graphQL
import caliban.wrappers.ApolloTracing.apolloTracing
import caliban.wrappers.Wrappers.{maxDepth, maxFields, printErrors, printSlowQueries, timeout}
import zio.random.Random

object ExampleApp extends App {
  private val graphiql =
    Http.succeed(Response.http(content = HttpData.fromStream(ZStream.fromResource("graphiql.html"))))

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    (for {
      _          <- console.putStrLn(
                      ExampleApi.api.render
                    )
      interpreter <- ExampleApi.api.interpreter
      _          <- Server
                      .start(
                        8088,
                        Http.route {
                          case _ -> Root / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
                          case _ -> Root / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
                          case _ -> Root / "graphiql"        => graphiql
                        }
                      )
                      .forever
    } yield ())
      .provideCustomLayer(Random.live)
      .exitCode
  }
}
