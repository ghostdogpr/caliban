package caliban

import caliban.TestUtils.Origin._
import caliban.TestUtils.Role._
import caliban.schema.Annotations.GQLDescription
import zio.UIO

object TestUtils {

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
  case class Query(
    @GQLDescription("Return all characters from a given origin") characters: CharactersArgs => List[Character],
    @GQLDescription("Find character by name") character: CharacterArgs => Option[Character]
  )

  @GQLDescription("Queries")
  case class QueryIO(
    @GQLDescription("Return all characters from a given origin") characters: CharactersArgs => UIO[List[Character]],
    @GQLDescription("Find character by name") character: CharacterArgs => UIO[Option[Character]]
  )

  val resolver = Query(
    args => characters.filter(c => args.origin.forall(c.origin == _)),
    args => characters.find(c => c.name == args.name)
  )
  val resolverIO = QueryIO(
    args => UIO(characters.filter(c => args.origin.forall(c.origin == _))),
    args => UIO(characters.find(c => c.name == args.name))
  )

}
