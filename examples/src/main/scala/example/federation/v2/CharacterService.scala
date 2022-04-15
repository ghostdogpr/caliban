package example.federation.v2

import example.federation.v2.FederationData.characters._
import zio.stream.ZStream
import zio.{ Has, Queue, Ref, UIO, URIO, ZLayer }

trait CharacterService {
  def getCharactersByEpisode(season: Int, episode: Int): UIO[List[Character]]

  def getCharacters(origin: Option[Origin]): UIO[List[Character]]

  def findCharacter(name: String): UIO[Option[Character]]

  def deleteCharacter(name: String): UIO[Boolean]

  def deletedEvents: ZStream[Any, Nothing, String]
}

object CharacterService {

  def getCharacters(origin: Option[Origin]): URIO[Has[CharacterService], List[Character]] =
    URIO.serviceWith(_.getCharacters(origin))

  def findCharacter(name: String): URIO[Has[CharacterService], Option[Character]] =
    URIO.serviceWith(_.findCharacter(name))

  def deleteCharacter(name: String): URIO[Has[CharacterService], Boolean] =
    URIO.serviceWith(_.deleteCharacter(name))

  def deletedEvents: ZStream[Has[CharacterService], Nothing, String] =
    ZStream.serviceWithStream(_.deletedEvents)

  def getCharactersByEpisode(season: Int, episode: Int): URIO[Has[CharacterService], List[Character]] =
    URIO.serviceWith(_.getCharactersByEpisode(season, episode))

  def make(initial: List[Character]): ZLayer[Any, Nothing, Has[CharacterService]] =
    (for {
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
            UIO.when(deleted)(
              subscribers.get.flatMap(
                // add item to all subscribers
                UIO.foreach(_)(queue =>
                  queue
                    .offer(name)
                    .catchSomeCause {
                      case cause if cause.interrupted =>
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
    }).toLayer
}
