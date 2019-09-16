package caliban.execution

import caliban.GraphQL._
import caliban.TestUtils._
import zio.Task
import zio.test.Assertion._
import zio.test._

object ExecutionSpec
    extends DefaultRunnableSpec(
      suite("ExecutionSpec")(
        testM("simple query with fields") {
          val schema = graphQL(resolver)
          val query =
            """{
              |  characters {
              |    name
              |  }
              |}""".stripMargin

          val io = schema.execute(query).map(_.toString).run
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
          val schema = graphQL(resolver)
          val query =
            """{
              |  characters(origin: MARS) {
              |    name
              |    nicknames
              |  }
              |}""".stripMargin

          val io = schema.execute(query).map(_.toString).run
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
          val schema = graphQL(resolver)
          val query =
            """{
              |  amos: character(name: "Amos Burton") {
              |    name
              |    nicknames
              |  }
              |}""".stripMargin

          val io = schema.execute(query).map(_.toString).run
          assertM(
            io,
            succeeds(
              equalTo(
                """{"amos":{"name":"Amos Burton","nicknames":[]}}"""
              )
            )
          )
        },
        testM("fragment") {
          val schema = graphQL(resolver)
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

          val io = schema.execute(query).map(_.toString).run
          assertM(
            io,
            succeeds(
              equalTo(
                """{"amos":{"name":"Amos Burton"}}"""
              )
            )
          )
        },
        testM("inline fragment") {
          val schema = graphQL(resolver)
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

          val io = schema.execute(query).map(_.toString).run
          assertM(
            io,
            succeeds(
              equalTo(
                """{"amos":{"name":"Amos Burton","role":{"shipName":"Rocinante"}}}"""
              )
            )
          )
        },
        testM("effectful query") {
          val io = Task.runtime.map { implicit rts =>
            val schema = graphQL(resolverIO)
            val query =
              """{
                |  characters {
                |    name
                |  }
                |}""".stripMargin
            (query, schema)
          }.flatMap { case (query, schema) => schema.execute(query).map(_.toString).run }

          assertM(
            io,
            succeeds(
              equalTo(
                """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
              )
            )
          )
        },
        testM("mutation") {
          val io = Task.runtime.map { implicit rts =>
            val schema = graphQL(resolverWithMutation)
            val query =
              """mutation {
                |  deleteCharacter(name: "Amos Burton")
                |}""".stripMargin
            (query, schema)
          }.flatMap { case (query, schema) => schema.execute(query).map(_.toString).run }

          assertM(io, succeeds(equalTo("""{"deleteCharacter":{}}""")))
        }
      )
    )
