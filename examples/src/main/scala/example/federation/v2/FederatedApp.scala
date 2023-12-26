package example.federation.v2

import caliban.quick._
import example.federation.v2.FederationData.characters.sampleCharacters
import example.federation.v2.FederationData.episodes.sampleEpisodes
import zio._

object FederatedApp extends ZIOAppDefault {

  val characterServer =
    FederatedApi.Characters.api.runServer(8088, "/api/graphql")

  val episodeServer =
    FederatedApi.Episodes.api.runServer(8089, "/api/graphql")

  def run =
    (characterServer race episodeServer)
      .provide(EpisodeService.make(sampleEpisodes), CharacterService.make(sampleCharacters))
}
