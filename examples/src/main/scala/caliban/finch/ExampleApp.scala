package caliban.finch

import caliban.ExampleData.{ sampleCharacters, Character, CharacterArgs, CharactersArgs, Role }
import caliban.GraphQL.graphQL
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.GenericSchema
import caliban.{ ExampleService, FinchAdapter, GraphQL, RootResolver }
import com.twitter.io.{ Buf, BufReader, Reader }
import com.twitter.util.Await
import io.circe.Json
import io.finch.Endpoint
import zio.clock.Clock
import zio.console.Console
import zio.interop.catz._
import zio.stream.ZStream
import zio.{ Runtime, Task, URIO }
import scala.language.postfixOps

object ExampleApp extends App with GenericSchema[Console with Clock] with Endpoint.Module[Task] {

  implicit val runtime = Runtime.unsafeFromLayer(Console.live ++ Clock.live)

  implicit val roleSchema           = gen[Role]
  implicit val characterSchema      = gen[Character]
  implicit val characterArgsSchema  = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[Console, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[Console, Option[Character]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[Console, Boolean])
  case class Subscriptions(characterDeleted: ZStream[Console, Nothing, String])

  import caliban.wrappers.ApolloTracing.apolloTracing
  import caliban.wrappers.Wrappers._
  import zio.duration._

  def makeApi(service: ExampleService): GraphQL[Console with Clock] =
    graphQL(
      RootResolver(
        Queries(
          args => service.getCharacters(args.origin),
          args => service.findCharacter(args.name)
        ),
        Mutations(args => service.deleteCharacter(args.name)),
        Subscriptions(service.deletedEvents)
      )
    ) @@
      maxFields(200) @@               // query analyzer that limit query fields
      maxDepth(30) @@                 // query analyzer that limit query depth
      timeout(3 seconds) @@           // wrapper that fails slow queries
      printSlowQueries(500 millis) @@ // wrapper that logs slow queries
      apolloTracing                   // wrapper for https://github.com/apollographql/apollo-tracing

  val service     = runtime.unsafeRun(ExampleService.make(sampleCharacters))
  val interpreter = runtime.unsafeRun(makeApi(service).interpreter)

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
