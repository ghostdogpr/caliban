package example.federation

import caliban.quick._
import example.federation.FederationData.characters.sampleCharacters
import example.federation.FederationData.episodes.sampleEpisodes
import zio._

object FederatedApp extends ZIOAppDefault {

  val episodesApi   = FederatedApi.Episodes.api.runServer(8089, "/api/graphql", Some("/graphiql"))
  val charactersApi = FederatedApi.Characters.api.runServer(8088, "/api/graphql", Some("/graphiql"))

  override def run =
    (episodesApi zipPar charactersApi)
      .provide(
        EpisodeService.make(sampleEpisodes),
        CharacterService.make(sampleCharacters)
      )
}
