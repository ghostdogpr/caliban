package caliban.finch

import scala.io.StdIn
import caliban.ExampleData.{ sampleCharacters, Character, CharacterArgs, CharactersArgs, Role }
import caliban.GraphQL.graphQL
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.GenericSchema
import caliban.{ ExampleService, FinchHttpAdapter, RootResolver }
import com.twitter.finagle.Http
import io.finch.Endpoint
import zio.clock.Clock
import zio.console.Console
import zio.stream.ZStream
import zio.{ DefaultRuntime, Task, URIO }
import com.twitter.util.Await

object ExampleApp extends App with GenericSchema[Console with Clock] with Endpoint.Module[Task] {

  implicit val defaultRuntime = new DefaultRuntime {}

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

  val interpreter = defaultRuntime.unsafeRun(
    ExampleService
      .make(sampleCharacters)
      .map(service => {
        graphQL(
          RootResolver(
            Queries(
              args => service.getCharacters(args.origin),
              args => service.findCharacter(args.name)
            ),
            Mutations(args => service.deleteCharacter(args.name)),
            Subscriptions(service.deletedEvents)
          )
        ).interpreter
      })
  )
  import io.finch._
  import io.finch.circe._

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

  val route = Bootstrap
    .serve[Application.Json](FinchHttpAdapter.makeHttpService(interpreter))

  val server = Http.server.serve(":8080", route)

  println(s"Server online at http://localhost:8088/\nPress RETURN to stop...")
  Await.ready(server)

}
