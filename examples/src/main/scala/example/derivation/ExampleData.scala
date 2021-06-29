package example.derivation

import example.derivation.ExampleData.Origin.{ BELT, EARTH, MARS }
import example.derivation.ExampleData.Role.{ Captain, Engineer, Mechanic, Pilot }
import caliban.derivation.annotation.{ deriveSchema, GQLExclude }
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.Schema
import zio.query.ZQuery

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

  case class Character(
    name: String,
    @GQLDescription("A list of the character's nicknames") nicknames: List[String],
    origin: Origin,
    role: Option[Role]
  ) {
    @GQLDescription("Foo")
    def foo(): Int = 5

    @GQLDeprecated("Don't use Bar.")
    def bar: Int = 42

    @GQLDescription("Hey, look, an optional field")
    val baz: Option[String] = None

    def allNicknames: ZQuery[Any, Nothing, String] =
      ZQuery.succeed(nicknames.mkString(", "))

    def findNickname(pattern: String, limit: Int): List[String] =
      nicknames.filter(_.matches(pattern)).take(limit)

    // This field will not be exposed via GraphQL
    @GQLExclude
    def internal(): Unit = xyz

    // Private fields are also not exposed via GraphQL
    private val xyz: Unit = ()
  }

  object Character {
    implicit val schema: Schema[Any, Character] = caliban.derivation.deriveSchemaInstance[Character]
  }

  case class Characters(characters: List[Character])
  object Characters {
    implicit val schema: Schema[Any, Characters] = caliban.derivation.deriveSchemaInstance[Characters]
  }

  val sampleCharacters: Characters = Characters(
    List(
      Character("James Holden", List("Jim", "Hoss"), EARTH, Some(Captain("Rocinante"))),
      Character("Naomi Nagata", Nil, BELT, Some(Engineer("Rocinante"))),
      Character("Amos Burton", Nil, EARTH, Some(Mechanic("Rocinante"))),
      Character("Alex Kamal", Nil, MARS, Some(Pilot("Rocinante"))),
      Character("Chrisjen Avasarala", Nil, EARTH, None),
      Character("Josephus Miller", List("Joe"), BELT, None),
      Character("Roberta Draper", List("Bobbie", "Gunny"), MARS, None)
    )
  )
}
