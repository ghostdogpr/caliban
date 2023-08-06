package caliban

object Data {
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
    Character("James Holden", List("Jim", "Hoss"), Origin.EARTH, Some(Role.Captain("Rocinante"))),
    Character("Naomi Nagata", Nil, Origin.BELT, Some(Role.Engineer("Rocinante"))),
    Character("Amos Burton", Nil, Origin.EARTH, Some(Role.Mechanic("Rocinante"))),
    Character("Alex Kamal", Nil, Origin.MARS, Some(Role.Pilot("Rocinante"))),
    Character("Chrisjen Avasarala", Nil, Origin.EARTH, None),
    Character("Josephus Miller", List("Joe"), Origin.BELT, None),
    Character("Roberta Draper", List("Bobbie", "Gunny"), Origin.MARS, None)
  )
}
