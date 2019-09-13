package caliban

import caliban.schema.Annotations.GQLDescription
import caliban.Test.Origin._
import caliban.Test.Role._
import zio.UIO

object Test {

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

  @GQLDescription("API")
  case class Query(
    @GQLDescription("Return all characters from given origin") characters: CharactersArgs => UIO[List[Character]],
    @GQLDescription("Find character by name") character: CharacterArgs => UIO[Option[Character]]
  )

  val resolver = Query(
    args => UIO(characters.filter(c => args.origin.forall(c.origin == _))),
    args => UIO(characters.find(c => c.name == args.name))
  )
}
