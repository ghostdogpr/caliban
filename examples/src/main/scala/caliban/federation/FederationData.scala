package caliban.federation

import caliban.federation
import caliban.schema.Annotations.GQLDirective

object FederationData {

  object episodes {
    @GQLDirective(federation.Key("season episode"))
    case class Episode(
      name: String,
      season: Int,
      episode: Int,
    )

    @GQLDirective(federation.Key("name"))
    @GQLDirective(federation.Extend)
    case class Character(@GQLDirective(federation.External) name: String)

    case class EpisodeArgs(season: Int, episode: Int)
    case class EpisodesArgs(season: Option[Int])

    val sampleEpisodes = List(
      Episode("Dulcinea", season = 1, episode = 1),
      Episode("The Big Empty", season = 1, episode = 2),
      Episode("Safe", season = 2, episode = 1)
    )
  }

}
