package caliban

import caliban.Annotations.GQLDescription
import caliban.Test.Job._
import caliban.Test.Origin._

object Test {

  sealed trait Origin

  object Origin {
    case object EARTH extends Origin
    case object MARS  extends Origin
    case object BELT  extends Origin
  }

  sealed trait Job

  object Job {
    case class Captain(shipName: String)  extends Job
    case class Pilot(shipName: String)    extends Job
    case class Engineer(shipName: String) extends Job
    case class Mechanic(shipName: String) extends Job
    case object Politician                extends Job
    case object Soldier                   extends Job
    case object Detective                 extends Job
  }

  case class Character(name: String, nicknames: List[String], origin: Origin, job: Option[Job])

  val characters = List(
    Character("James Holden", List("Jim", "Hoss"), EARTH, Some(Captain("Rocinante"))),
    Character("Naomi Nagata", Nil, BELT, Some(Engineer("Rocinante"))),
    Character("Amos Burton", Nil, EARTH, Some(Mechanic("Rocinante"))),
    Character("Alex Kamal", Nil, MARS, Some(Pilot("Rocinante"))),
    Character("Josephus Miller", List("Joe"), BELT, Some(Detective)),
    Character("Chrisjen Avasarala", Nil, EARTH, Some(Politician)),
    Character("Roberta Draper", List("Bobbie", "Gunny"), MARS, Some(Soldier))
  )

  case class CharactersArgs(origin: Option[Origin])
  case class CharacterArgs(name: String)

  case class Query(
    @GQLDescription("Return all characters from given origin") characters: CharactersArgs => List[Character],
    @GQLDescription("Find character by name") character: CharacterArgs => Option[Character]
  )

  val resolver = Query(
    args => characters.filter(c => args.origin.forall(c.origin == _)),
    args => characters.find(c => c.name == args.name)
  )
}
