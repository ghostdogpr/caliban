package caliban.federation

import caliban.ExampleService.ExampleService
import caliban.GraphQL.graphQL
import caliban.{ ExampleService, GraphQL, RootResolver }
import caliban.federation.EpisodeService.EpisodeService
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
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
    import caliban.ExampleData.{ Character, CharacterArgs, CharactersArgs, Episode, EpisodeArgs, Role }

    case class Queries(
      @GQLDescription("Return all characters from a given origin")
      characters: CharactersArgs => URIO[ExampleService, List[Character]],
      @GQLDeprecated("Use `characters`")
      character: CharacterArgs => URIO[ExampleService, Option[Character]]
    )
    case class Mutations(deleteCharacter: CharacterArgs => URIO[ExampleService, Boolean])
    case class Subscriptions(characterDeleted: ZStream[ExampleService, Nothing, String])

    implicit val roleSchema                                          = gen[Role]
    implicit lazy val episodeSchema: Schema[ExampleService, Episode] = gen[Episode]
    implicit val characterSchema                                     = gen[Character]
    implicit val characterArgsSchema                                 = gen[CharacterArgs]
    implicit val charactersArgsSchema                                = gen[CharactersArgs]
    implicit val episodeArgs                                         = gen[EpisodeArgs]
    implicit val episodeArgBuilder: ArgBuilder[EpisodeArgs]          = ArgBuilder.gen[EpisodeArgs]

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
        List(
          EntityResolver.from[CharacterArgs](args => ZQuery.fromEffect(ExampleService.findCharacter(args.name))),
          EntityResolver.from[EpisodeArgs](
            args =>
              ZQuery
                .fromEffect(ExampleService.getCharactersByEpisode(args.season, args.episode))
                .map(
                  characters =>
                    Some(
                      Episode(
                        args.season,
                        args.episode,
                        ZQuery.succeed(characters)
                      )
                  )
              )
          )
        )
      )
  }

  object Episodes extends GenericSchema[EpisodeService] {
    import caliban.federation.FederationData.episodes.{ Episode, EpisodeArgs }
    import caliban.federation.FederationData.episodes.EpisodesArgs

    case class Queries(
      episode: EpisodeArgs => URIO[EpisodeService, Option[Episode]],
      episodes: EpisodesArgs => URIO[EpisodeService, List[Episode]]
    )

    implicit val episodeArgsSchema  = gen[EpisodeArgs]
    implicit val episodesArgsSchema = gen[EpisodesArgs]
    implicit val episodeSchema      = gen[Episode]

    val api: GraphQL[Console with Clock with EpisodeService] =
      federate(
        graphQL(
          RootResolver(
            Queries(
              args => EpisodeService.getEpisode(args.season, args.episode),
              args => EpisodeService.getEpisodes(args.season)
            )
          )
        ) @@
          maxFields(200) @@               // query analyzer that limit query fields
          maxDepth(30) @@                 // query analyzer that limit query depth
          timeout(3 seconds) @@           // wrapper that fails slow queries
          printSlowQueries(500 millis) @@ // wrapper that logs slow queries
          apolloTracing, // wrapper for https://github.com/apollographql/apollo-tracing
        EntityResolver.from[EpisodeArgs](
          args => ZQuery.fromEffect(EpisodeService.getEpisode(args.season, args.episode))
        ) :: Nil
      )
  }

}
