package example.federation

import example.federation.FederationData.episodes.Episode

import zio.{ Has, Ref, UIO, URIO, ZLayer }

object EpisodeService {
  type EpisodeService = Has[Service]

  trait Service {
    def getEpisode(season: Int, episode: Int): UIO[Option[Episode]]
    def getEpisodes(season: Option[Int]): UIO[List[Episode]]
  }

  def getEpisode(season: Int, episode: Int): URIO[EpisodeService, Option[Episode]] =
    URIO.accessM[EpisodeService](_.get.getEpisode(season, episode))

  def getEpisodes(season: Option[Int] = None): URIO[EpisodeService, List[Episode]] =
    URIO.accessM[EpisodeService](_.get.getEpisodes(season))

  def make(initial: List[Episode]): ZLayer[Any, Nothing, EpisodeService] = ZLayer.fromEffect {
    Ref.make(initial).map { episodes =>
      new Service {
        override def getEpisode(season: Int, episode: Int): UIO[Option[Episode]] =
          episodes.get.map(_.find(e => e.season == season && e.episode == episode))

        override def getEpisodes(season: Option[Int]): UIO[List[Episode]] =
          episodes.get.map(_.filter(e => season.forall(_ == e.season)))
      }
    }
  }
}
