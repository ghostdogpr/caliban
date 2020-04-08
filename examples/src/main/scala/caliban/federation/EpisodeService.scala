package caliban.federation

import caliban.federation.FederationData.episodes.Episode
import zio.{ Has, Ref, UIO, URIO, ZLayer }

object EpisodeService {
  type EpisodeService = Has[Service]

  trait Service {
    def getEpisode(name: String): UIO[Option[Episode]]
  }

  def getEpisode(name: String): URIO[EpisodeService, Option[Episode]] =
    URIO.accessM[EpisodeService](_.get.getEpisode(name))

  def make(initial: List[Episode]): ZLayer[Any, Nothing, EpisodeService] = ZLayer.fromEffect {
    Ref.make(initial).map { episodes =>
      new Service {
        override def getEpisode(name: String): UIO[Option[Episode]] =
          episodes.get.map(_.find(_.name == name))
      }
    }
  }
}
