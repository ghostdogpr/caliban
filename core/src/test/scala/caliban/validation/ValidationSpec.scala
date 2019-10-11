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
        },
        testM("field on enum") {
          val interpreter = graphQL(resolver)
          val query =
            """{
              |  characters {
              |    origin {
              |      __typename
              |    }
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString).run
          assertM(
            io,
            fails[CalibanError](
              hasField[CalibanError, String](
                "msg",
                _.msg,
                equalTo("Field selection is impossible on type 'Origin'.")
              )
            )
          )
        },
        testM("missing field on object") {
          val interpreter = graphQL(resolver)
          val query =
            """{
              |  characters
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString).run
          assertM(
            io,
            fails[CalibanError](
              hasField[CalibanError, String](
                "msg",
                _.msg,
                equalTo("Field selection is mandatory on type 'Character'.")
              )
            )
          )
        },
        testM("invalid argument") {
          val interpreter = graphQL(resolver)
          val query =
            """{
              |  characters(arg: 1) {
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
                equalTo("Argument 'arg' is not defined on field 'characters' of type 'Query'.")
              )
            )
          )
        },
        testM("missing argument") {
          val interpreter = graphQL(resolver)
          val query =
            """{
              |  character(name: null) {
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
                equalTo("Required argument 'name' is null or missing on field 'character' of type 'Query'.")
              )
            )
          )
        },
        testM("duplicated fragment name") {
          val interpreter = graphQL(resolver)
          val query =
            """query {
              |  characters {
              |    f
              |  }
              |}
              |
              |fragment f on Character {
              |  name
              |}
              |fragment f on Character {
              |  name
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString).run
          assertM(
            io,
            fails[CalibanError](
              hasField[CalibanError, String](
                "msg",
                _.msg,
                equalTo("Fragment 'f' is defined more than once.")
              )
            )
          )
        },
        testM("fragment on invalid type") {
          val interpreter = graphQL(resolver)
          val query =
            """query {
              |  characters {
              |    name
              |    ... on Boolean {
              |       name
              |    }
              |  }
              |}""".stripMargin

          val io = interpreter.execute(query).map(_.toString).run
          assertM(
            io,
            fails[CalibanError](
              hasField[CalibanError, String](
                "msg",
                _.msg,
                equalTo("Inline Fragment on invalid type 'Boolean'")
              )
            )
          )
        }
      )
    )
