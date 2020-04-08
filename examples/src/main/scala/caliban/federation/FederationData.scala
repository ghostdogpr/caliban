package caliban.federation

import caliban.federation
import caliban.schema.Annotations.GQLDirective

object FederationData {

  object episodes {
    @GQLDirective(federation.Key("name"))
    case class Episode(
      name: String,
      season: Int,
      characters: List[Character] = Nil
    )

    @GQLDirective(federation.Key("name"))
    @GQLDirective(federation.Extend)
    case class Character(@GQLDirective(federation.External) name: String)

    case class EpisodeArgs(name: String)

    val sampleEpisodes = List(
      Episode("Dulcinea", season = 1)
    )
  }

}
