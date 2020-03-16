package caliban.finch

import scala.language.postfixOps
import caliban.ExampleData.{ sampleCharacters, Character, CharacterArgs, CharactersArgs, Role }
import caliban.ExampleService.ExampleService
import caliban.GraphQL.graphQL
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.GenericSchema
import caliban.{ ExampleService, FinchAdapter, RootResolver }
import com.twitter.io.{ Buf, BufReader, Reader }
import com.twitter.util.Await
import io.circe.Json
import io.finch.Endpoint
import zio.interop.catz._
import zio.stream.ZStream
import zio.{ Runtime, Task, URIO }

object ExampleApp extends App with GenericSchema[ExampleService] with Endpoint.Module[Task] {

  implicit val runtime = Runtime.default

  implicit val roleSchema           = gen[Role]
  implicit val characterSchema      = gen[Character]
  implicit val characterArgsSchema  = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[ExampleService, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[ExampleService, Option[Character]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[ExampleService, Boolean])
  case class Subscriptions(characterDeleted: ZStream[ExampleService, Nothing, String])

  import caliban.wrappers.ApolloTracing.apolloTracing
  import caliban.wrappers.Wrappers._
  import zio.duration._

  val api =
    graphQL(
      RootResolver(
        Queries(
          args => ExampleService.getCharacters(args.origin),
          args => ExampleService.findCharacter(args.name)
        ),
        Mutations(args => ExampleService.deleteCharacter(args.name)),
        Subscriptions(ExampleService.deletedEvents)
      )
    ) @@
      maxFields(200) @@               // query analyzer that limit query fields
      maxDepth(30) @@                 // query analyzer that limit query depth
      timeout(3 seconds) @@           // wrapper that fails slow queries
      printSlowQueries(500 millis) @@ // wrapper that logs slow queries
      apolloTracing                   // wrapper for https://github.com/apollographql/apollo-tracing

  val interpreter = runtime.unsafeRun(
    ExampleService.make(sampleCharacters).memoize.use(layer => api.interpreter.map(_.provideCustomLayer(layer)))
  )

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
  val endpoint: Endpoint[Task, Json] = "api" :: "graphql" :: FinchAdapter.makeHttpService(interpreter)

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
