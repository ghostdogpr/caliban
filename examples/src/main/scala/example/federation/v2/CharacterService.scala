package example.federation.v2

import example.federation.v2.FederationData.characters._
import zio.stream.ZStream
import zio.{ Queue, Ref, UIO, URIO, ZIO, ZLayer }

trait CharacterService {
  def getCharactersByEpisode(season: Int, episode: Int): UIO[List[Character]]

  def getCharacters(origin: Option[Origin]): UIO[List[Character]]

  def findCharacter(name: String): UIO[Option[Character]]

  def deleteCharacter(name: String): UIO[Boolean]

  def deletedEvents: ZStream[Any, Nothing, String]
}

object CharacterService {

  def getCharacters(origin: Option[Origin]): URIO[CharacterService, List[Character]] =
    ZIO.serviceWithZIO(_.getCharacters(origin))

  def findCharacter(name: String): URIO[CharacterService, Option[Character]] =
    ZIO.serviceWithZIO(_.findCharacter(name))

  def deleteCharacter(name: String): URIO[CharacterService, Boolean] =
    ZIO.serviceWithZIO(_.deleteCharacter(name))

  def deletedEvents: ZStream[CharacterService, Nothing, String] =
    ZStream.serviceWithStream(_.deletedEvents)

  def getCharactersByEpisode(season: Int, episode: Int): URIO[CharacterService, List[Character]] =
    ZIO.serviceWithZIO(_.getCharactersByEpisode(season, episode))

  def make(initial: List[Character]): ZLayer[Any, Nothing, CharacterService] = ZLayer.fromZIO {
    for {
      characters  <- Ref.make(initial)
      subscribers <- Ref.make(List.empty[Queue[String]])
    } yield new CharacterService {

      def getCharacters(origin: Option[Origin]): UIO[List[Character]] =
        characters.get.map(_.filter(c => origin.forall(c.origin == _)))

      def findCharacter(name: String): UIO[Option[Character]] = characters.get.map(_.find(c => c.name == name))

      def deleteCharacter(name: String): UIO[Boolean] =
        characters
          .modify(list =>
            if (list.exists(_.name == name)) (true, list.filterNot(_.name == name))
            else (false, list)
          )
          .tap(deleted =>
            ZIO.when(deleted)(
              subscribers.get.flatMap(
                // add item to all subscribers
                ZIO.foreach(_)(queue =>
                  queue
                    .offer(name)
                    .catchSomeCause {
                      case cause if cause.isInterrupted =>
                        subscribers.update(_.filterNot(_ == queue)).as(false)
                    } // if queue was shutdown, remove from subscribers
                )
              )
            )
          )

      def deletedEvents: ZStream[Any, Nothing, String] = ZStream.unwrap {
        for {
          queue <- Queue.unbounded[String]
          _     <- subscribers.update(queue :: _)
        } yield ZStream.fromQueue(queue).ensuring(queue.shutdown)
      }

      override def getCharactersByEpisode(season: Int, episode: Int): UIO[List[Character]] =
        characters.get.map(_.filter(c => c.starredIn.exists(e => e.episode == episode && e.season == season)))
    }
  }
}
