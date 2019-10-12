package caliban.validation

import caliban.CalibanError
import caliban.GraphQL._
import caliban.TestUtils._
import zio.UIO
import zio.test.Assertion._
import zio.test._

object ValidationSpec
    extends DefaultRunnableSpec({
      val interpreter = graphQL(resolver)
      def check(query: String, expectedMessage: String): UIO[TestResult] = {
        val io = interpreter.execute(query).map(_.toString).run
        assertM(io, fails[CalibanError](hasField[CalibanError, String]("msg", _.msg, equalTo(expectedMessage))))
      }

      suite("ValidationSpec")(
        testM("operation name uniqueness") {
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
          check(query, "Multiple operations have the same name: a.")
        },
        testM("subscription has only one root") {
          val query =
            """subscription s {
              |  characters {
              |    name
              |  }
              |  character(name: "Amos Burton") {
              |    name
              |  }
              |}""".stripMargin
          check(query, "Subscription 's' has more than one root field.")
        },
        testM("invalid field") {
          val query =
            """{
              |  characters {
              |    unknown
              |  }
              |}""".stripMargin
          check(query, "Field 'unknown' does not exist on type 'Character'.")
        },
        testM("invalid field in fragment") {
          val query =
            """query {
              |  characters {
              |    ...f
              |  }
              |}
              |
              |fragment f on Character {
              |  unknown
              |}""".stripMargin
          check(query, "Field 'unknown' does not exist on type 'Character'.")
        },
        testM("field on enum") {
          val query =
            """{
              |  characters {
              |    origin {
              |      __typename
              |    }
              |  }
              |}""".stripMargin
          check(query, "Field selection is impossible on type 'Origin'.")
        },
        testM("missing field on object") {
          val query =
            """{
              |  characters
              |}""".stripMargin
          check(query, "Field selection is mandatory on type 'Character'.")
        },
        testM("invalid argument") {
          val query =
            """{
              |  characters(arg: 1) {
              |    name
              |  }
              |}""".stripMargin
          check(query, "Argument 'arg' is not defined on field 'characters' of type 'Query'.")
        },
        testM("missing argument") {
          val query =
            """{
              |  character(name: null) {
              |    name
              |  }
              |}""".stripMargin
          check(query, "Required argument 'name' is null or missing on field 'character' of type 'Query'.")
        },
        testM("duplicated fragment name") {
          val query =
            """query {
              |  characters {
              |    ...f
              |  }
              |}
              |
              |fragment f on Character {
              |  name
              |}
              |fragment f on Character {
              |  name
              |}""".stripMargin
          check(query, "Fragment 'f' is defined more than once.")
        },
        testM("fragment on invalid type") {
          val query =
            """query {
              |  characters {
              |    name
              |    ... on Boolean {
              |       name
              |    }
              |  }
              |}""".stripMargin
          check(query, "Inline Fragment on invalid type 'Boolean'.")
        },
        testM("fragment unused") {
          val query =
            """query {
              |  characters {
              |    name
              |  }
              |}
              |
              |fragment f on Character {
              |  name
              |}""".stripMargin
          check(query, "Fragment 'f' is not used in any spread.")
        },
        testM("fragment spreads not defined") {
          val query =
            """query {
              |  characters {
              |    ...f
              |  }
              |}""".stripMargin
          check(query, "Fragment spread 'f' is not defined.")
        },
        testM("fragment cycle") {
          val query =
            """query {
              |  characters {
              |    ...f1
              |  }
              |}
              |
              |fragment f1 on Character {
              |  ...f2
              |}
              |fragment f2 on Character {
              |  ...f1
              |}""".stripMargin
          check(query, "Fragment 'f2' forms a cycle.")
        },
        testM("unsupported directive") {
          val query =
            """query {
              |  characters {
              |    name @yolo()
              |  }
              |}""".stripMargin
          check(query, "Directive 'yolo' is not supported.")
        },
        testM("variable defined twice") {
          val query =
            """query($name: String, $name: String) {
              |  characters {
              |    name
              |  }
              |}""".stripMargin
          check(query, "Variable 'name' is defined more than once.")
        },
        testM("invalid variable") {
          val query =
            """query($x: Character) {
              |  characters {
              |    name
              |  }
              |}""".stripMargin
          check(query, "Type of variable 'x' is not a valid input type.")
        },
        testM("variable not defined") {
          val query =
            """query {
              |  character(name: $x) {
              |    name
              |  }
              |}""".stripMargin
          check(query, "Variable 'x' is not defined.")
        },
        testM("variable not used") {
          val query =
            """query($x: String) {
              |  characters {
              |    name
              |  }
              |}""".stripMargin
          check(query, "Variable 'x' is not used.")
        }
      )
    })
