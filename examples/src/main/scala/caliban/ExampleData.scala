package caliban

import caliban.ExampleData.Origin._
import caliban.ExampleData.Role._
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import zio.stream.ZStream
import zio.{ Queue, Ref, UIO }

object ExampleData {

  sealed trait Origin

  object Origin {
    case object EARTH extends Origin
    case object MARS  extends Origin
    case object BELT  extends Origin
  }

  sealed trait Role

  object Role {
    case class Captain(shipName: String)  extends Role
    case class Pilot(shipName: String)    extends Role
    case class Engineer(shipName: String) extends Role
    case class Mechanic(shipName: String) extends Role
  }

  case class Character(name: String, nicknames: List[String], origin: Origin, role: Option[Role])

  val characters = List(
    Character("James Holden", List("Jim", "Hoss"), EARTH, Some(Captain("Rocinante"))),
    Character("Naomi Nagata", Nil, BELT, Some(Engineer("Rocinante"))),
    Character("Amos Burton", Nil, EARTH, Some(Mechanic("Rocinante"))),
    Character("Alex Kamal", Nil, MARS, Some(Pilot("Rocinante"))),
    Character("Chrisjen Avasarala", Nil, EARTH, None),
    Character("Josephus Miller", List("Joe"), BELT, None),
    Character("Roberta Draper", List("Bobbie", "Gunny"), MARS, None)
  )

  case class CharactersArgs(origin: Option[Origin])
  case class CharacterArgs(name: String)

  @GQLDescription("Queries")
  case class Queries(
    @GQLDescription("Return all characters from a given origin") characters: CharactersArgs => UIO[List[Character]],
    @GQLDeprecated("Use `characters`") character: CharacterArgs => UIO[Option[Character]]
  )

  @GQLDescription("Mutations")
  case class Mutations(deleteCharacter: CharacterArgs => UIO[Boolean])

  @GQLDescription("Subscriptions")
  case class Subscriptions(characterDeleted: ZStream[Any, Nothing, String])

  val resolver: UIO[RootResolver[Queries, Mutations, Subscriptions]] =
    for {
      state <- Ref.make(characters)
      queue <- Queue.unbounded[String]
    } yield RootResolver(
      Queries(
        args => state.get.map(_.filter(c => args.origin.forall(c.origin == _))),
        args => state.get.map(_.find(c => c.name == args.name))
      ),
      Mutations(
        args =>
          state
            .modify(
              list =>
                if (list.exists(_.name == args.name)) (true, list.filterNot(_.name == args.name))
                else (false, list)
            )
            .tap(deleted => UIO.when(deleted)(queue.offer(args.name)))
      ),
      Subscriptions(ZStream.fromQueue(queue))
    )

}
