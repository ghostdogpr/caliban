package example

import example.ExampleData._
import zio.stream.ZStream
import zio.{ Has, Hub, Ref, UIO, URIO, ZLayer }

object ExampleService {

  type ExampleService = Has[Service]

  trait Service {
    def getCharacters(origin: Option[Origin]): UIO[List[Character]]

    def findCharacter(name: String): UIO[Option[Character]]

    def deleteCharacter(name: String): UIO[Boolean]

    def deletedEvents: ZStream[Any, Nothing, String]
  }

  def getCharacters(origin: Option[Origin]): URIO[ExampleService, List[Character]] =
    URIO.serviceWith(_.getCharacters(origin))

  def findCharacter(name: String): URIO[ExampleService, Option[Character]] =
    URIO.serviceWith(_.findCharacter(name))

  def deleteCharacter(name: String): URIO[ExampleService, Boolean] =
    URIO.serviceWith(_.deleteCharacter(name))

  def deletedEvents: ZStream[ExampleService, Nothing, String] =
    ZStream.accessStream(_.get.deletedEvents)

  def make(initial: List[Character]): ZLayer[Any, Nothing, ExampleService] =
    (for {
      characters  <- Ref.make(initial)
      subscribers <- Hub.unbounded[String]
    } yield new Service {

      def getCharacters(origin: Option[Origin]): UIO[List[Character]] =
        characters.get.map(_.filter(c => origin.forall(c.origin == _)))

      def findCharacter(name: String): UIO[Option[Character]] = characters.get.map(_.find(c => c.name == name))

      def deleteCharacter(name: String): UIO[Boolean] =
        characters
          .modify(list =>
            if (list.exists(_.name == name)) (true, list.filterNot(_.name == name))
            else (false, list)
          )
          .tap(deleted => UIO.when(deleted)(subscribers.publish(name)))

      def deletedEvents: ZStream[Any, Nothing, String] =
        ZStream.unwrapManaged(subscribers.subscribe.map(ZStream.fromQueue(_)))
    }).toLayer
}
