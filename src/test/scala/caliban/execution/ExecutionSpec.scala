package caliban.execution

import caliban.GraphQL._
import caliban.execution.ExecutionSpecUtils.Origin._
import caliban.execution.ExecutionSpecUtils.Role._
import caliban.execution.ExecutionSpecUtils._
import caliban.schema.Annotations.GQLDescription
import zio.test.Assertion._
import zio.test._
import zio.{ Task, UIO }

object ExecutionSpec
    extends DefaultRunnableSpec(
      suite("ExecutionSpec")(
        testM("simple query with fields") {
          val schema = graphQL[Query]
          val query =
            """{
              |  characters {
              |    name
              |  }
              |}""".stripMargin

          val io = schema.execute(query, resolver).map(_.mkString).run
          assertM(
            io,
            succeeds(
              equalTo(
                """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
              )
            )
          )
        },
        testM("arguments") {
          val schema = graphQL[Query]
          val query =
            """{
              |  characters(origin: MARS) {
              |    name
              |    nicknames
              |  }
              |}""".stripMargin

          val io = schema.execute(query, resolver).map(_.mkString).run
          assertM(
            io,
            succeeds(
              equalTo(
                """{"characters":[{"name":"Alex Kamal","nicknames":[]},{"name":"Roberta Draper","nicknames":["Bobbie","Gunny"]}]}"""
              )
            )
          )
        },
        testM("aliases") {
          val schema = graphQL[Query]
          val query =
            """{
              |  amos: character(name: "Amos Burton") {
              |    name
              |    nicknames
              |  }
              |}""".stripMargin

          val io = schema.execute(query, resolver).map(_.mkString).run
          assertM(
            io,
            succeeds(
              equalTo(
                """{"amos":{"name":"Amos Burton","nicknames":[]}}"""
              )
            )
          )
        },
        testM("effectful query") {
          val io = Task.runtime.map { implicit rts =>
            val schema = graphQL[Query]
            val query =
              """{
                |  characters {
                |    name
                |  }
                |}""".stripMargin
            (query, schema)
          }.flatMap { case (query, schema) => schema.execute(query, resolver).map(_.mkString).run }

          assertM(
            io,
            succeeds(
              equalTo(
                """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
              )
            )
          )
        }
      )
    )

object ExecutionSpecUtils {

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
