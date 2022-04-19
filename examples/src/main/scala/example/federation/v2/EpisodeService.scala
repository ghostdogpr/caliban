package example.federation.v2

import example.federation.v2.FederationData.episodes.Episode
import zio.{ Has, Ref, UIO, URIO, ZLayer }

trait EpisodeService {
  def getEpisode(season: Int, episode: Int): UIO[Option[Episode]]
  def getEpisodes(season: Option[Int]): UIO[List[Episode]]
}

object EpisodeService {

  def getEpisode(season: Int, episode: Int): URIO[Has[EpisodeService], Option[Episode]] =
    URIO.serviceWith(_.getEpisode(season, episode))

  def getEpisodes(season: Option[Int] = None): URIO[Has[EpisodeService], List[Episode]] =
    URIO.serviceWith(_.getEpisodes(season))

  def make(initial: List[Episode]): ZLayer[Any, Nothing, Has[EpisodeService]] =
    Ref
      .make(initial)
      .map { episodes =>
        new EpisodeService {
          override def getEpisode(season: Int, episode: Int): UIO[Option[Episode]] =
            episodes.get.map(_.find(e => e.season == season && e.episode == episode))

          override def getEpisodes(season: Option[Int]): UIO[List[Episode]] =
            episodes.get.map(_.filter(e => season.forall(_ == e.season)))
        }
      }
      .toLayer
}
