package example.federation.v2

import caliban.ZHttpAdapter
import example.federation.v2.FederationData.characters.sampleCharacters
import example.federation.v2.FederationData.episodes.sampleEpisodes
import zhttp.http._
import zhttp.service.Server
import zio._

object FederatedApp extends ZIOAppDefault {

  val characterServer = for {
    interpreter <- FederatedApi.Characters.api.interpreter
    _           <- Server
                     .start(
                       8088,
                       Http.collectHttp[Request] { case _ -> !! / "api" / "graphql" =>
                         ZHttpAdapter.makeHttpService(interpreter)
                       }
                     )
                     .forever
  } yield ()

  val episodeServer = for {
    interpreter <- FederatedApi.Episodes.api.interpreter
    _           <- Server
                     .start(
                       8089,
                       Http.collectHttp[Request] { case _ -> !! / "api" / "graphql" =>
                         ZHttpAdapter.makeHttpService(interpreter)
                       }
                     )
                     .forever
  } yield ()

  override def run =
    (characterServer race episodeServer)
      .provideLayer(EpisodeService.make(sampleEpisodes) ++ CharacterService.make(sampleCharacters))
      .exitCode
}
