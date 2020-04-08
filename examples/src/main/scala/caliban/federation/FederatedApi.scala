package caliban.federation

import caliban.ExampleService.ExampleService
import caliban.GraphQL.graphQL
import caliban.{ ExampleService, GraphQL, RootResolver }
import caliban.federation.EpisodeService.EpisodeService
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.GenericSchema
import caliban.wrappers.Wrappers.{ maxDepth, maxFields, printSlowQueries, timeout }
import zio.URIO
import zio.clock.Clock
import zio.console.Console
import zquery.ZQuery
import zio.duration._
import caliban.wrappers.ApolloTracing._
import zio.stream.ZStream
import scala.language.postfixOps

object FederatedApi {

  object Characters extends GenericSchema[ExampleService] {
    import caliban.ExampleData.{ Character, CharacterArgs, CharactersArgs, Episode, Role }

    case class Queries(
      @GQLDescription("Return all characters from a given origin")
      characters: CharactersArgs => URIO[ExampleService, List[Character]],
      @GQLDeprecated("Use `characters`")
      character: CharacterArgs => URIO[ExampleService, Option[Character]]
    )
    case class Mutations(deleteCharacter: CharacterArgs => URIO[ExampleService, Boolean])
    case class Subscriptions(characterDeleted: ZStream[ExampleService, Nothing, String])

    implicit val roleSchema           = gen[Role]
    implicit val characterSchema      = gen[Character]
    implicit val characterArgsSchema  = gen[CharacterArgs]
    implicit val charactersArgsSchema = gen[CharactersArgs]
    implicit val episodeSchema        = gen[Episode]

    val api: GraphQL[Console with Clock with ExampleService] =
      federate(
        graphQL(
          RootResolver(
            Queries(
              args => ExampleService.getCharacters(args.origin),
              args => ExampleService.findCharacter(args.name)
            ),
            Mutations(args => ExampleService.deleteCharacter(args.name))
          )
        ) @@
          maxFields(200) @@               // query analyzer that limit query fields
          maxDepth(30) @@                 // query analyzer that limit query depth
          timeout(3 seconds) @@           // wrapper that fails slow queries
          printSlowQueries(500 millis) @@ // wrapper that logs slow queries
          apolloTracing, // wrapper for https://github.com/apollographql/apollo-tracing
        EntityResolver.from[CharacterArgs](args => ZQuery.fromEffect(ExampleService.findCharacter(args.name))) :: Nil
      )
  }

  object Episodes extends GenericSchema[EpisodeService] {
    import caliban.federation.FederationData.episodes.{ Episode, EpisodeArgs }

    case class Queries(
      episode: EpisodeArgs => URIO[EpisodeService, Option[Episode]]
    )

    implicit val episodeArgsSchema = gen[EpisodeArgs]
    implicit val episodeSchema     = gen[Episode]

    val api: GraphQL[Console with Clock with EpisodeService] =
      federate(
        graphQL(
          RootResolver(
            Queries(
              args => EpisodeService.getEpisode(args.name),
            )
          )
        ) @@
          maxFields(200) @@               // query analyzer that limit query fields
          maxDepth(30) @@                 // query analyzer that limit query depth
          timeout(3 seconds) @@           // wrapper that fails slow queries
          printSlowQueries(500 millis) @@ // wrapper that logs slow queries
          apolloTracing, // wrapper for https://github.com/apollographql/apollo-tracing
        EntityResolver.from[EpisodeArgs](args => ZQuery.fromEffect(EpisodeService.getEpisode(args.name))) :: Nil
      )
  }

}
