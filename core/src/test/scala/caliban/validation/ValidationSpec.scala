package caliban.validation

import caliban.CalibanError
import caliban.CalibanError.ValidationError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.TestUtils._
import caliban.Value.StringValue
import zio.IO
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test._

object ValidationSpec extends DefaultRunnableSpec {
  private val gql         = graphQL(resolverWithSubscription)
  private val interpreter = gql.interpreter

  def check(query: String, expectedMessage: String): IO[ValidationError, TestResult] = {
    val io = interpreter.flatMap(_.execute(query)).map(_.errors.headOption)
    assertM(io)(isSome(hasField[CalibanError, String]("msg", _.msg, equalTo(expectedMessage))))
  }

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("ValidationSpec")(
      testM("operation name uniqueness") {
        val query = gqldoc("""
             query a {
               characters {
                 name
               }
             }
             
             query a {
               characters {
                 name
               }
              }""")
        check(query, "Multiple operations have the same name: a.")
      },
      testM("subscription has only one root") {
        val query = gqldoc("""
             subscription s {
               characters {
                 name
               }
               character(name: "Amos Burton") {
                 name
               }
              }""")
        check(query, "Subscription 's' has more than one root field.")
      },
      testM("invalid field") {
        val query = gqldoc("""
             {
               characters {
                 unknown
               }
              }""")
        check(query, "Field 'unknown' does not exist on type 'Character'.")
      },
      testM("invalid field in fragment") {
        val query = gqldoc("""
             query {
               characters {
                 ...f
               }
             }
             
             fragment f on Character {
               unknown
              }""")
        check(query, "Field 'unknown' does not exist on type 'Character'.")
      },
      testM("field on enum") {
        val query = gqldoc("""
             {
               characters {
                 origin {
                   __typename
                 }
               }
              }""")
        check(query, "Field selection is impossible on type 'Origin'.")
      },
      testM("missing field on object") {
        val query = gqldoc("""
             {
               characters
              }""")
        check(query, "Field selection is mandatory on type 'Character'.")
      },
      testM("invalid argument") {
        val query = gqldoc("""
             {
               characters(arg: 1) {
                 name
               }
              }""")
        check(query, "Argument 'arg' is not defined on field 'characters' of type 'Query'.")
      },
      testM("missing argument") {
        val query = gqldoc("""
             {
               character(name: null) {
                 name
               }
              }""")
        check(query, "Required argument 'name' is null or missing on field 'character' of type 'Query'.")
      },
      testM("duplicated fragment name") {
        val query = gqldoc("""
             query {
               characters {
                 ...f
               }
             }
             
             fragment f on Character {
               name
             }
             fragment f on Character {
               name
              }""")
        check(query, "Fragment 'f' is defined more than once.")
      },
      testM("fragment on invalid type") {
        val query = gqldoc("""
             query {
               characters {
                 name
                 ... on Boolean {
                    name
                 }
               }
              }""")
        check(
          query,
          "Inline fragment is defined on invalid type 'Boolean'"
        )
      },
      testM("fragment on impossible type") {
        val query = gqldoc("""
             query {
               characters {
                 name
                 ... on Role {
                    name
                 }
               }
              }""")
        check(
          query,
          "Inline fragment spread is not possible: possible types are 'Character' and possible fragment types are 'Captain, Engineer, Mechanic, Pilot'."
        )
      },
      testM("fragment unused") {
        val query = gqldoc("""
             query {
               characters {
                 name
               }
             }
             
             fragment f on Character {
               name
              }""")
        check(query, "Fragment 'f' is not used in any spread.")
      },
      testM("fragment spreads not defined") {
        val query = gqldoc("""
             query {
               characters {
                 ...f
               }
              }""")
        check(query, "Fragment spread 'f' is not defined.")
      },
      testM("fragment cycle") {
        val query = gqldoc("""
             query {
               characters {
                 ...f1
               }
             }
             
             fragment f1 on Character {
               ...f2
             }
             fragment f2 on Character {
               ...f1
              }""")
        check(query, "Fragment 'f2' forms a cycle.")
      },
      testM("unsupported directive") {
        val query = gqldoc("""
             query {
               characters {
                 name @yolo()
               }
              }""")
        check(query, "Directive 'yolo' is not supported.")
      },
      testM("variable defined twice") {
        val query = gqldoc("""
             query($name: String, $name: String) {
               characters {
                 name
               }
              }""")
        check(query, "Variable 'name' is defined more than once.")
      },
      testM("invalid variable") {
        val query = gqldoc("""
             query($x: Character) {
               characters {
                 name
               }
              }""")
        check(query, "Type of variable 'x' is not a valid input type.")
      },
      testM("variable not defined") {
        val query = gqldoc("""
             query {
               character(name: $x) {
                 name
               }
              }""")
        check(query, "Variable 'x' is not defined.")
      },
      testM("variable not used") {
        val query = gqldoc("""
             query($x: String) {
               characters {
                 name
               }
              }""")
        check(query, "Variable 'x' is not used.")
      },
      testM("variable used in list") {
        val query = gqldoc("""
             query($x: String) {
               charactersIn(names: [$x]){
                 name
               }
              }""")
        assertM(interpreter.flatMap(_.execute(query, None, Map("x" -> StringValue("y")))).map(_.errors.headOption))(
          isNone
        )
      },
      testM("variable used in object") {
        val query = gqldoc("""
             query($x: String) {
               exists(character: { name: $x, nicknames: [], origin: EARTH })
              }""")
        assertM(interpreter.flatMap(_.execute(query, None, Map("x" -> StringValue("y")))).map(_.errors.headOption))(
          isNone
        )
      },
      testM("invalid input field") {
        val query = gqldoc("""
             query {
               exists(character: { unknown: "" })
             }""")
        check(query, "Input field 'unknown' is not defined on type 'CharacterInput'.")
      },
      testM("required input field not defined") {
        val query = gqldoc("""
             query {
               exists(character: { name: "name" })
             }""")
        check(query, "Required field 'nicknames' on object 'CharacterInput' was not provided.")
      },
      testM("directive used in wrong location") {
        val query = gqldoc("""
             query @skip(if: true) {
               characters {
                 name
               }
             }""")
        check(query, "Directive 'skip' is used in invalid location 'QUERY'.")
      },
      testM("directive used twice") {
        val query = gqldoc("""
             query {
               characters {
                 name @skip(if: true) @skip(if: true)
               }
             }""")
        check(query, "Directive 'skip' is defined twice.")
      }
    )
}
