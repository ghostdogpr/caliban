package example.derivation

import zio._
import zio.duration._
import zio.stream._
import zhttp.http._
import zhttp.service.Server
import caliban.{ RootResolver, ZHttpAdapter }
import caliban.schema.Schema
import ExampleData.{ sampleCharacters, Character }
import caliban.GraphQL.graphQL
import caliban.wrappers.ApolloTracing.apolloTracing
import caliban.wrappers.Wrappers.{ maxDepth, maxFields, printErrors, printSlowQueries, timeout }

object ExampleApp extends App {
  private val graphiql =
    Http.succeed(Response.http(content = HttpData.fromStream(ZStream.fromResource("graphiql.html"))))

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    val api = graphQL(RootResolver(sampleCharacters)) @@
      maxFields(200) @@               // query analyzer that limit query fields
      maxDepth(30) @@                 // query analyzer that limit query depth
      timeout(3.seconds) @@           // wrapper that fails slow queries
      printSlowQueries(500.millis) @@ // wrapper that logs slow queries
      printErrors @@                  // wrapper that logs errors
      apolloTracing                   // wrapper for https://github.com/apollographql/apollo-tracing
    (for {
      _          <- console.putStrLn(
                      api.render
                    )
      interpreter <- api.interpreter
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
    } yield ()).exitCode
  }
}
