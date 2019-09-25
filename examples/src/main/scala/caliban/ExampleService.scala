package caliban

import caliban.ExampleData._
import zio.stream.ZStream
import zio.{ Queue, Ref, UIO }

class ExampleService(characters: Ref[List[Character]], subscribers: Ref[List[Queue[String]]]) {

  def getCharacters(origin: Option[Origin]): UIO[List[Character]] =
    characters.get.map(_.filter(c => origin.forall(c.origin == _)))

  def findCharacter(name: String): UIO[Option[Character]] = characters.get.map(_.find(c => c.name == name))

  def deleteCharacter(name: String): UIO[Boolean] =
    characters
      .modify(
        list =>
          if (list.exists(_.name == name)) (true, list.filterNot(_.name == name))
          else (false, list)
      )
      .tap(
        deleted =>
          UIO.when(deleted)(
            subscribers.get.flatMap(
              // add item to all subscribers
              UIO.foreach(_)(
                queue =>
                  queue
                    .offer(name)
                    .onInterrupt(subscribers.update(_.filterNot(_ == queue))) // if queue was shutdown, remove from subscribers
              )
            )
          )
      )

  def deletedEvents: ZStream[Any, Nothing, String] = ZStream.unwrap {
    for {
      queue <- Queue.unbounded[String]
      _     <- subscribers.update(queue :: _)
    } yield ZStream.fromQueue(queue)
  }
}

object ExampleService {
  def make(initial: List[Character]): UIO[ExampleService] =
    for {
      state       <- Ref.make(initial)
      subscribers <- Ref.make(List.empty[Queue[String]])
    } yield new ExampleService(state, subscribers)

  val resolver: UIO[RootResolver[Queries, Mutations, Subscriptions]] =
    make(sampleCharacters)
      .map(
        service =>
          RootResolver(
            Queries(args => service.getCharacters(args.origin), args => service.findCharacter(args.name)),
            Mutations(args => service.deleteCharacter(args.name)),
            Subscriptions(service.deletedEvents)
          )
      )
}
