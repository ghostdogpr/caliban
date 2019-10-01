package caliban.execution

import caliban.GraphQL._
import caliban.TestUtils._
import caliban.parsing.adt.Value.{ BooleanValue, StringValue }
import zio.Task
import zio.test.Assertion._
import zio.test._

object ExecutionSpec
    extends DefaultRunnableSpec(
      suite("ExecutionSpec")(
        testM("skip directive") {
          val interpreter = graphQL(resolver)
          val query =
            """query test{
              |  amos: character(name: "Amos Burton") {
              |    name
              |    nicknames @skip(if: true)
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString)
          assertM(
            io,
            equalTo(
              """{"amos":{"name":"Amos Burton"}}"""
            )
          )
        },
        testM("simple query with fields") {
          val interpreter = graphQL(resolver)
          val query =
            """{
              |  characters {
              |    name
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString)
          assertM(
            io,
            equalTo(
              """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
            )
          )
        },
        testM("arguments") {
          val interpreter = graphQL(resolver)
          val query =
            """{
              |  characters(origin: MARS) {
              |    name
              |    nicknames
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString)
          assertM(
            io,
            equalTo(
              """{"characters":[{"name":"Alex Kamal","nicknames":[]},{"name":"Roberta Draper","nicknames":["Bobbie","Gunny"]}]}"""
            )
          )
        },
        testM("arguments with list coercion") {
          val interpreter = graphQL(resolver)
          val query =
            """{
              |  charactersIn(names: "Alex Kamal") {
              |    name
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString)
          assertM(
            io,
            equalTo(
              """{"charactersIn":[{"name":"Alex Kamal"}]}"""
            )
          )
        },
        testM("aliases") {
          val interpreter = graphQL(resolver)
          val query =
            """{
              |  amos: character(name: "Amos Burton") {
              |    name
              |    nicknames
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString)
          assertM(
            io,
            equalTo(
              """{"amos":{"name":"Amos Burton","nicknames":[]}}"""
            )
          )
        },
        testM("fragment") {
          val interpreter = graphQL(resolver)
          val query =
            """{
              |  amos: character(name: "Amos Burton") {
              |    ...info
              |  }
              |}
              |
              |fragment info on Character {
              |  name
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString)
          assertM(
            io,
            equalTo(
              """{"amos":{"name":"Amos Burton"}}"""
            )
          )
        },
        testM("inline fragment") {
          val interpreter = graphQL(resolver)
          val query =
            """{
              |  amos: character(name: "Amos Burton") {
              |    name
              |    role {
              |      ... on Mechanic {
              |        shipName
              |      }
              |    }
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString)
          assertM(
            io,
            equalTo(
              """{"amos":{"name":"Amos Burton","role":{"shipName":"Rocinante"}}}"""
            )
          )
        },
        testM("effectful query") {
          val io = Task.runtime.map { implicit rts =>
            val interpreter = graphQL(resolverIO)
            val query =
              """{
                |  characters {
                |    name
                |  }
                |}""".stripMargin
            (query, interpreter)
          }.flatMap { case (query, interpreter) => interpreter.execute(query).map(_.toString) }

          assertM(
            io,
            equalTo(
              """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
            )
          )
        },
        testM("mutation") {
          val io = Task.runtime.map { implicit rts =>
            val interpreter = graphQL(resolverWithMutation)
            val query =
              """mutation {
                |  deleteCharacter(name: "Amos Burton")
                |}""".stripMargin
            (query, interpreter)
          }.flatMap { case (query, interpreter) => interpreter.execute(query).map(_.toString) }

          assertM(io, equalTo("""{"deleteCharacter":{}}"""))
        },
        testM("variable") {
          val interpreter = graphQL(resolver)
          val query =
            """query test($name: String!){
              |  amos: character(name: $name) {
              |    name
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query, None, Map("name" -> StringValue("Amos Burton"))).map(_.toString)
          assertM(
            io,
            equalTo(
              """{"amos":{"name":"Amos Burton"}}"""
            )
          )
        },
        testM("skip directive") {
          val interpreter = graphQL(resolver)
          val query =
            """query test{
              |  amos: character(name: "Amos Burton") {
              |    name
              |    nicknames @skip(if: true)
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString)
          assertM(
            io,
            equalTo(
              """{"amos":{"name":"Amos Burton"}}"""
            )
          )
        },
        testM("include directive") {
          val interpreter = graphQL(resolver)
          val query =
            """query test($included: Boolean!){
              |  amos: character(name: "Amos Burton") {
              |    name
              |    nicknames @include(if: $included)
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query, None, Map("included" -> BooleanValue(false))).map(_.toString)
          assertM(
            io,
            equalTo(
              """{"amos":{"name":"Amos Burton"}}"""
            )
          )
        }
      )
    )
