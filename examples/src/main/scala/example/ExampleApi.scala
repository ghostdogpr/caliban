package example

import example.ExampleData._
import example.ExampleService.ExampleService
import caliban.GraphQL
import caliban.GraphQL.graphQL
import caliban.RootResolver
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription, GQLName }
import caliban.schema.{ GenericSchema, Schema }
import caliban.wrappers.ApolloTracing.apolloTracing
import caliban.wrappers.Wrappers._
import zio.{ URIO, ZIO }
import zio.clock.Clock
import zio.console.Console
import zio.duration._
import zio.stream.ZStream

import scala.language.postfixOps

object ExampleApi extends GenericSchema[ExampleService] {

  sealed trait ConnectedBy

  object ConnectedBy {
    case object Origin extends ConnectedBy
    case object Ship   extends ConnectedBy
  }

  case class ConnectionArgs(by: ConnectedBy)

  @GQLName("Character")
  case class CharacterZIO(
    name: String,
    nicknames: List[String],
    origin: Origin,
    role: Option[Role],
    connections: ConnectionArgs => URIO[ExampleService, List[CharacterZIO]]
  )

  def character2CharacterZIO(ch: Character): CharacterZIO =
    CharacterZIO(
      name = ch.name,
      nicknames = ch.nicknames,
      origin = ch.origin,
      role = ch.role,
      connections = args =>
        (args.by match {
          case ConnectedBy.Origin => ExampleService.getCharacters(Some(ch.origin))
          case ConnectedBy.Ship   =>
            ExampleService.getCharacters(None).map { characters =>
              val maybeShip = ch.role.collectFirst {
                case Role.Captain(shipName)  => shipName
                case Role.Pilot(shipName)    => shipName
                case Role.Engineer(shipName) => shipName
                case Role.Mechanic(shipName) => shipName
              }
              characters
                .filter(_.role.exists {
                  case Role.Captain(shipName)  => maybeShip.contains(shipName)
                  case Role.Pilot(shipName)    => maybeShip.contains(shipName)
                  case Role.Engineer(shipName) => maybeShip.contains(shipName)
                  case Role.Mechanic(shipName) => maybeShip.contains(shipName)
                })

            }
        }).map(_.filter(_.name == ch.name).map(character2CharacterZIO))
    )

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[ExampleService, List[CharacterZIO]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[ExampleService, Option[CharacterZIO]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[ExampleService, Boolean])
  case class Subscriptions(characterDeleted: ZStream[ExampleService, Nothing, String])

  implicit val connectedBySchema: Schema[Any, ConnectedBy]                   = Schema.gen
  implicit val roleSchema: Schema[Any, Role]                                 = Schema.gen
  implicit val characterArgsSchema: Schema[Any, CharacterArgs]               = Schema.gen
  implicit val charactersArgsSchema: Schema[Any, CharactersArgs]             = Schema.gen
  implicit lazy val characterZIOSchema: Schema[ExampleService, CharacterZIO] = gen
  implicit val queriesSchema: Schema[ExampleService, Queries]                = gen

  val api: GraphQL[Console with Clock with ExampleService] =
    graphQL(
      RootResolver(
        Queries(
          args => ExampleService.getCharacters(args.origin).map(_.map(character2CharacterZIO)),
          args => ExampleService.findCharacter(args.name).map(_.map(character2CharacterZIO))
        ),
        Mutations(args => ExampleService.deleteCharacter(args.name)),
        Subscriptions(ExampleService.deletedEvents)
      )
    ) @@
      maxFields(200) @@               // query analyzer that limit query fields
      maxDepth(30) @@                 // query analyzer that limit query depth
      timeout(3 seconds) @@           // wrapper that fails slow queries
      printSlowQueries(500 millis) @@ // wrapper that logs slow queries
      printErrors @@                  // wrapper that logs errors
      apolloTracing                   // wrapper for https://github.com/apollographql/apollo-tracing

}
