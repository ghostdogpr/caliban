package caliban

import caliban.ExampleData.Origin.{ BELT, EARTH, MARS }
import caliban.ExampleData.Role.{ Captain, Engineer, Mechanic, Pilot }
import caliban.schema.Annotations.GQLDirective

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

  @GQLDirective(federation.Key("name"))
  case class Character(name: String,
                       nicknames: List[String],
                       origin: Origin,
                       role: Option[Role],
                       starredIn: List[Episode] = Nil)

  @GQLDirective(federation.Key("name"))
  @GQLDirective(federation.Extend)
  case class Episode(@GQLDirective(federation.External) name: String)

  case class CharactersArgs(origin: Option[Origin])
  case class CharacterArgs(name: String)

  val sampleCharacters = List(
    Character("James Holden", List("Jim", "Hoss"), EARTH, Some(Captain("Rocinante"))),
    Character("Naomi Nagata", Nil, BELT, Some(Engineer("Rocinante")), List(Episode("Dulcinea"))),
    Character("Amos Burton", Nil, EARTH, Some(Mechanic("Rocinante"))),
    Character("Alex Kamal", Nil, MARS, Some(Pilot("Rocinante"))),
    Character("Chrisjen Avasarala", Nil, EARTH, None),
    Character("Josephus Miller", List("Joe"), BELT, None),
    Character("Roberta Draper", List("Bobbie", "Gunny"), MARS, None)
  )

}
