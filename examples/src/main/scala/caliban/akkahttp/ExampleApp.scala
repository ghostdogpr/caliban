package caliban.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import caliban.ExampleData.{Character, CharacterArgs, CharactersArgs, Role, sampleCharacters}
import caliban.GraphQL.graphQL
import caliban.schema.Annotations.{GQLDeprecated, GQLDescription}
import caliban.schema.GenericSchema
import caliban.{AkkaHttpAdapter, ExampleService, RootResolver}
import zio.clock.Clock
import zio.console.Console
import zio.stream.ZStream
import zio.{DefaultRuntime, URIO}

import scala.io.StdIn

object ExampleApp extends App with GenericSchema[Console with Clock] {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  implicit val defaultRuntime = new DefaultRuntime {}

  implicit val roleSchema = gen[Role]
  implicit val characterSchema = gen[Character]
  implicit val characterArgsSchema = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

  case class Queries(
                      @GQLDescription("Return all characters from a given origin")
                      characters: CharactersArgs => URIO[Console, List[Character]],
                      @GQLDeprecated("Use `characters`")
                      character: CharacterArgs => URIO[Console, Option[Character]]
                    )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[Console, Boolean])
  case class Subscriptions(characterDeleted: ZStream[Console, Nothing, String])

  val interpreter = defaultRuntime.unsafeRun(ExampleService.make(sampleCharacters).map(service => {
    graphQL(
      RootResolver(
        Queries(
          args => service.getCharacters(args.origin),
          args => service.findCharacter(args.name)
        ),
        Mutations(args => service.deleteCharacter(args.name)),
        Subscriptions(service.deletedEvents)))
  }))

  /**
   * curl -X POST \
   * http://localhost:8080/api/graphql \
   * -H 'Host: localhost:8080' \
   * -H 'Content-Type: application/json' \
   * -d '{
   * "query": "query { characters { name }}"
   * }'
   */

  val route =
      path("api" / "graphql") {
        AkkaHttpAdapter.makeRestService(interpreter)
      }

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())

}
