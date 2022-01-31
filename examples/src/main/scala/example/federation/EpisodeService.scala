package example.federation

import example.federation.FederationData.episodes.Episode

import zio.{ Ref, UIO, URIO, ZLayer }

trait EpisodeService {
  def getEpisode(season: Int, episode: Int): UIO[Option[Episode]]
  def getEpisodes(season: Option[Int]): UIO[List[Episode]]
}

object EpisodeService {
  def getEpisode(season: Int, episode: Int): URIO[EpisodeService, Option[Episode]] =
    URIO.serviceWithZIO[EpisodeService](_.getEpisode(season, episode))

  def getEpisodes(season: Option[Int] = None): URIO[EpisodeService, List[Episode]] =
    URIO.serviceWithZIO[EpisodeService](_.getEpisodes(season))

  def make(initial: List[Episode]): ZLayer[Any, Nothing, EpisodeService] =
    Ref
      .make(initial)
      .map { episodes =>
        new EpisodeService {
          def getEpisode(season: Int, episode: Int): UIO[Option[Episode]] =
            episodes.get.map(_.find(e => e.season == season && e.episode == episode))

          def getEpisodes(season: Option[Int]): UIO[List[Episode]] =
            episodes.get.map(_.filter(e => season.forall(_ == e.season)))
        }
      }
      .toLayer
}
