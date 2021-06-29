package example.derivation

import caliban.GraphQL.graphQL
import caliban.{ GraphQL, RootResolver }
import example.derivation.ExampleApi.Origin.{ BELT, EARTH, MARS }
import example.derivation.ExampleApi.Role.{ Captain, Engineer, Mechanic, Pilot }
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription, GQLExclude }
import caliban.schema.{ GenericSchema, Schema }
import caliban.wrappers.ApolloTracing.apolloTracing
import caliban.wrappers.Wrappers.{ maxDepth, maxFields, printErrors, printSlowQueries, timeout }
import zio.ZIO
import zio.clock.Clock
import zio.console.Console
import zio.duration._
import zio.query.ZQuery
import zio.random
import zio.random.Random

object ExampleApi extends GenericSchema[Random] {

  sealed trait Origin {
    def population: Long
  }

  object Origin {
    case object EARTH extends Origin {
      override def population: Long = 30000000000L
    }
    case object MARS  extends Origin {
      override def population: Long = 4000000000L
    }
    case object BELT  extends Origin {
      override def population: Long = 50000000L
    }
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

    @GQLDescription("Randomly picks one of the nicknames")
    def randomNickname: ZIO[Random, Nothing, Option[String]] =
      if (nicknames.nonEmpty) {
        random.nextIntBounded(nicknames.size).map { idx =>
          Some(nicknames(idx))
        }
      } else ZIO.none

    // This field will not be exposed via GraphQL
    @GQLExclude
    def internal(): Unit = xyz

    // Private fields are also not exposed via GraphQL
    private val xyz: Unit = ()
  }

  case class Characters(characters: List[Character])

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

  // NOTE: the derived schema instances must be in the object extending GenericSchema directly otherwise the
  // magnolia auto-derivated instances will win
  implicit val originSchema: Schema[Random, Origin]    = caliban.derivation.deriveSchemaInstance[Random, Origin]
  implicit val charSchema: Schema[Random, Character]   = caliban.derivation.deriveSchemaInstance[Random, Character]
  implicit val charsSchema: Schema[Random, Characters] = caliban.derivation.deriveSchemaInstance[Random, Characters]

  val api: GraphQL[Console with Clock with Random] =
    graphQL(RootResolver(sampleCharacters)) @@
      maxFields(200) @@               // query analyzer that limit query fields
      maxDepth(30) @@                 // query analyzer that limit query depth
      timeout(3.seconds) @@           // wrapper that fails slow queries
      printSlowQueries(500.millis) @@ // wrapper that logs slow queries
      printErrors @@                  // wrapper that logs errors
      apolloTracing                   // wrapper for https://github.com/apollographql/apollo-tracing
}
