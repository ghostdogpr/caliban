package caliban

object Data {
  sealed trait Origin

  case object EARTH extends Origin
  case object MARS  extends Origin
  case object BELT  extends Origin

  sealed trait Role

  case class Captain(shipName: String)  extends Role
  case class Pilot(shipName: String)    extends Role
  case class Engineer(shipName: String) extends Role
  case class Mechanic(shipName: String) extends Role

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
}
