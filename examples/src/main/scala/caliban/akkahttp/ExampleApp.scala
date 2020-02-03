package caliban.akkahttp

import scala.io.StdIn
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import zio.{ DefaultRuntime, URIO }
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.stream.Materializer
import caliban.ExampleData.{ sampleCharacters, Character, CharacterArgs, CharactersArgs, Role }
import caliban.GraphQL.graphQL
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.GenericSchema
import caliban.{ AkkaHttpAdapter, ExampleService, GraphQL, RootResolver }
import zio.clock.Clock
import zio.console.Console
import zio.stream.ZStream

object ExampleApp extends App with GenericSchema[Console with Clock] {

  implicit val system           = ActorSystem()
  implicit val materializer     = Materializer.matFromSystem
  implicit val executionContext = system.dispatcher
  implicit val defaultRuntime   = new DefaultRuntime {}

  implicit val roleSchema           = gen[Role]
  implicit val characterSchema      = gen[Character]
  implicit val characterArgsSchema  = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

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
    )

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[Console, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[Console, Option[Character]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[Console, Boolean])
  case class Subscriptions(characterDeleted: ZStream[Console, Nothing, String])

  val interpreter =
    ExampleService
      .make(sampleCharacters)
      .map(service => makeApi(service).interpreter)

  val apiRoute   = AkkaHttpAdapter.makeHttpServiceM(interpreter)
  val wsApiRoute = AkkaHttpAdapter.makeWebSocketServiceM(interpreter)

  /**
   * curl -X POST \
   * http://localhost:8088/api/graphql \
   * -H 'Host: localhost:8088' \
   * -H 'Content-Type: application/json' \
   * -d '{
   * "query": "query { characters { name }}"
   * }'
   */
  val route: Route =
    path("api" / "graphql") {
      apiRoute // do not call makeHttpServiceM here, or it will be recreated at every call
    } ~ path("ws" / "graphql") {
      wsApiRoute
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
