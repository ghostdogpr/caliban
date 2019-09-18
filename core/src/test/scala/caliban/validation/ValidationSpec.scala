package caliban.validation

import caliban.CalibanError
import caliban.GraphQL._
import caliban.TestUtils._
import zio.test.Assertion._
import zio.test._

object ValidationSpec
    extends DefaultRunnableSpec(
      suite("ValidationSpec")(
        testM("operation name uniqueness") {
          val interpreter = graphQL(resolver)
          val query =
            """query a {
              |  characters {
              |    name
              |  }
              |}
              |
              |query a {
              |  characters {
              |    name
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString).run
          assertM(
            io,
            fails[CalibanError](
              hasField[CalibanError, String](
                "msg",
                _.msg,
                equalTo("Multiple operations have the same name: a.")
              )
            )
          )
        },
        testM("subscription has only one root") {
          val interpreter = graphQL(resolver)
          val query =
            """subscription s {
              |  characters {
              |    name
              |  }
              |  character(name: "Amos Burton") {
              |    name
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString).run
          assertM(
            io,
            fails[CalibanError](
              hasField[CalibanError, String](
                "msg",
                _.msg,
                equalTo("Subscription 's' has more than one root field.")
              )
            )
          )
        },
        testM("invalid field") {
          val interpreter = graphQL(resolver)
          val query =
            """{
              |  characters {
              |    unknown
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString).run
          assertM(
            io,
            fails[CalibanError](
              hasField[CalibanError, String](
                "msg",
                _.msg,
                equalTo("Field 'unknown' does not exist on type 'Character'.")
              )
            )
          )
        },
        testM("invalid field in fragment") {
          val interpreter = graphQL(resolver)
          val query =
            """query {
              |  characters {
              |    name
              |  }
              |}
              |
              |fragment f on Character {
              |  unknown
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString).run
          assertM(
            io,
            fails[CalibanError](
              hasField[CalibanError, String](
                "msg",
                _.msg,
                equalTo("Field 'unknown' does not exist on type 'Character'.")
              )
            )
          )
        }
      )
    )
