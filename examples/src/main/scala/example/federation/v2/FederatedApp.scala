package example.federation.v2

import caliban.ZHttpAdapter
import caliban.interop.tapir.HttpInterpreter
import example.federation.v2.FederationData.characters.sampleCharacters
import example.federation.v2.FederationData.episodes.sampleEpisodes
import zio.http._
import zio._

object FederatedApp extends ZIOAppDefault {
  import sttp.tapir.json.circe._

  val characterServer = for {
    interpreter <- FederatedApi.Characters.api.interpreter
    config       = ZLayer.succeed(Server.Config.default.port(8088))
    _           <-
      Server
        .serve(
          Http
            .collectHttp[Request] { case _ -> !! / "api" / "graphql" =>
              ZHttpAdapter.makeHttpService(HttpInterpreter(interpreter))
            }
        )
        .provideSome[CharacterService](Server.live, config)
  } yield ()

  val episodeServer = for {
    interpreter <- FederatedApi.Episodes.api.interpreter
    config       = ZLayer.succeed(Server.Config.default.port(8089))
    _           <-
      Server
        .serve(
          Http
            .collectHttp[Request] { case _ -> !! / "api" / "graphql" =>
              ZHttpAdapter.makeHttpService(HttpInterpreter(interpreter))
            }
        )
        .provideSome[EpisodeService](Server.live, config)
  } yield ()

  override def run =
    (characterServer race episodeServer)
      .provide(EpisodeService.make(sampleEpisodes), CharacterService.make(sampleCharacters))
      .exitCode
}
