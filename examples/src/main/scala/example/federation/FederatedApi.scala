package example.federation

import caliban.GraphQL.graphQL
import caliban.federation.EntityResolver
import caliban.federation.tracing.ApolloFederatedTracing
import caliban.federation.v1.{ federated, GQLKey }
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.{ ArgBuilder, GenericSchema, Schema }
import caliban.schema.ArgBuilder.auto._
import caliban.wrappers.Wrapper
import caliban.wrappers.Wrappers.{ maxDepth, maxFields, printSlowQueries, timeout }
import caliban.{ GraphQL, GraphQLAspect, RootResolver }
import zio._
import zio.query.ZQuery
import zio.stream.ZStream

import scala.language.postfixOps

object FederatedApi {

  val standardWrappers: Wrapper[Any] =
    maxFields(200) |+|                 // query analyzer that limit query fields
      maxDepth(30) |+|                 // query analyzer that limit query depth
      timeout(3 seconds) |+|           // wrapper that fails slow queries
      printSlowQueries(500 millis) |+| // wrapper that logs slow queries
      ApolloFederatedTracing.wrapper   // wrapper for https://github.com/apollographql/apollo-tracing

  object Characters extends GenericSchema[CharacterService] {
    import example.federation.FederationData.characters._
    import auto._

    case class Queries(
      @GQLDescription("Return all characters from a given origin")
      characters: CharactersArgs => URIO[CharacterService, List[Character]],
      @GQLDeprecated("Use `characters`")
      character: CharacterArgs => URIO[CharacterService, Option[Character]]
    )
    case class Mutations(deleteCharacter: CharacterArgs => URIO[CharacterService, Boolean])
    case class Subscriptions(characterDeleted: ZStream[CharacterService, Nothing, String])

    implicit val originSchema: Schema[Any, Origin]                     = Schema.gen
    implicit val roleSchema: Schema[Any, Role]                         = Schema.gen
    implicit val characterSchema: Schema[CharacterService, Character] =
      // build this one manually because recursion breaks magnolia
      obj("Character", None, directives = List(GQLKey("name").directive))(implicit ft =>
        List(
          field("name")(_.name),
          field("nicknames")(_.nicknames),
          field("origin")(_.origin),
          field("role")(_.role),
          field("starredIn")(_.starredIn)
        )
      )
    implicit lazy val episodeSchema: Schema[CharacterService, Episode] = gen
    implicit val characterArgsSchema: Schema[Any, CharacterArgs]       = Schema.gen
    implicit val charactersArgsSchema: Schema[Any, CharactersArgs]     = Schema.gen
    implicit val episodeArgs: Schema[Any, EpisodeArgs]                 = Schema.gen
    implicit val episodeArgBuilder: ArgBuilder[EpisodeArgs]            = ArgBuilder.gen

    val withFederation: GraphQLAspect[Nothing, CharacterService] =
      federated(
        EntityResolver.from[CharacterArgs](args => ZQuery.fromZIO(CharacterService.findCharacter(args.name))),
        EntityResolver.from[EpisodeArgs](args =>
          ZQuery
            .fromZIO(CharacterService.getCharactersByEpisode(args.season, args.episode))
            .map(characters =>
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

    val api: GraphQL[CharacterService] =
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
    import auto._

    case class Queries(
      episode: EpisodeArgs => URIO[EpisodeService, Option[Episode]],
      episodes: EpisodesArgs => URIO[EpisodeService, List[Episode]]
    )

    implicit val episodeArgsSchema: Schema[Any, EpisodeArgs]   = Schema.gen
    implicit val episodesArgsSchema: Schema[Any, EpisodesArgs] = Schema.gen
    implicit val episodeSchema: Schema[Any, Episode]           = Schema.gen

    val api: GraphQL[EpisodeService] =
      graphQL(
        RootResolver(
          Queries(
            args => EpisodeService.getEpisode(args.season, args.episode),
            args => EpisodeService.getEpisodes(args.season)
          )
        )
      ) @@ standardWrappers @@ federated(
        EntityResolver.from[EpisodeArgs](args => ZQuery.fromZIO(EpisodeService.getEpisode(args.season, args.episode)))
      )

  }

}
