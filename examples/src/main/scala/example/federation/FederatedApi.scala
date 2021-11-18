package example.federation

import example.federation.CharacterService.CharacterService
import example.federation.EpisodeService.EpisodeService
import caliban.GraphQL.graphQL
import caliban.federation.{ EntityResolver, federated }
import caliban.federation.tracing.ApolloFederatedTracing
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
import caliban.wrappers.Wrappers.{ maxDepth, maxFields, printSlowQueries, timeout }
import caliban.{ GraphQL, RootResolver }

import zio.URIO
import zio.clock.Clock
import zio.console.Console
import zio.duration._
import zio.query.ZQuery
import zio.stream.ZStream

import scala.language.postfixOps

object FederatedApi {

  val standardWrappers = maxFields(200) |+| // query analyzer that limit query fields
    maxDepth(30) |+|                 // query analyzer that limit query depth
    timeout(3 seconds) |+|           // wrapper that fails slow queries
    printSlowQueries(500 millis) |+| // wrapper that logs slow queries
    ApolloFederatedTracing.wrapper   // wrapper for https://github.com/apollographql/apollo-tracing

  object Characters extends GenericSchema[CharacterService] {
    import example.federation.FederationData.characters.{
      Character,
      CharacterArgs,
      CharactersArgs,
      Episode,
      EpisodeArgs,
      Role
    }

    case class Queries(
      @GQLDescription("Return all characters from a given origin")
      characters: CharactersArgs => URIO[CharacterService, List[Character]],
      @GQLDeprecated("Use `characters`")
      character: CharacterArgs => URIO[CharacterService, Option[Character]]
    )
    case class Mutations(deleteCharacter: CharacterArgs => URIO[CharacterService, Boolean])
    case class Subscriptions(characterDeleted: ZStream[CharacterService, Nothing, String])

    implicit val roleSchema: Schema[Any, Role]                         = Schema.gen
    implicit lazy val episodeSchema: Schema[CharacterService, Episode] = Schema.gen
    implicit val characterSchema: Schema[CharacterService, Character]  = Schema.gen
    implicit val characterArgsSchema: Schema[Any, CharacterArgs]       = Schema.gen
    implicit val charactersArgsSchema: Schema[Any, CharactersArgs]     = Schema.gen
    implicit val episodeArgs: Schema[Any, EpisodeArgs]                 = Schema.gen
    implicit val episodeArgBuilder: ArgBuilder[EpisodeArgs]            = ArgBuilder.gen

    val withFederation = federated(        
      EntityResolver.from[CharacterArgs](args => ZQuery.fromEffect(CharacterService.findCharacter(args.name))),
      EntityResolver.from[EpisodeArgs](args =>
          ZQuery
            .fromEffect(CharacterService.getCharactersByEpisode(args.season, args.episode))
            .map(characters =>
              Some(
                Episode(
                  args.season,
                  args.episode,
                  ZQuery.succeed(characters)
                )
              )
            )
        ))

    val api: GraphQL[Console with Clock with CharacterService] =
        graphQL(
          RootResolver(
            Queries(
              args => CharacterService.getCharacters(args.origin),
              args => CharacterService.findCharacter(args.name)
            ),
            Mutations(args => CharacterService.deleteCharacter(args.name))
          )
        ) @@ standardWrappers @@ withFederation
  }

  object Episodes extends GenericSchema[EpisodeService] {
    import example.federation.FederationData.episodes.{ Episode, EpisodeArgs, EpisodesArgs }

    case class Queries(
      episode: EpisodeArgs => URIO[EpisodeService, Option[Episode]],
      episodes: EpisodesArgs => URIO[EpisodeService, List[Episode]]
    )

    implicit val episodeArgsSchema: Schema[Any, EpisodeArgs]   = Schema.gen
    implicit val episodesArgsSchema: Schema[Any, EpisodesArgs] = Schema.gen
    implicit val episodeSchema: Schema[Any, Episode]           = Schema.gen

    val api: GraphQL[Console with Clock with EpisodeService] =
        graphQL(
          RootResolver(
            Queries(
              args => EpisodeService.getEpisode(args.season, args.episode),
              args => EpisodeService.getEpisodes(args.season)
            )
          )
        ) @@ standardWrappers @@ federated(
        EntityResolver.from[EpisodeArgs](args =>
          ZQuery.fromEffect(EpisodeService.getEpisode(args.season, args.episode))
        )
        )

  }

}
