package caliban.validation

import caliban.{ CalibanError, InputValue }
import caliban.CalibanError.ValidationError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.TestUtils._
import caliban.Value.{ BooleanValue, IntValue, StringValue }
import zio.IO
import zio.test.Assertion._
import zio.test._

object ValidationSpec extends ZIOSpecDefault {
  private val gql         = graphQL(resolverWithSubscription)
  private val interpreter = gql.interpreter

  def check(
    query: String,
    expectedMessage: String,
    variables: Map[String, InputValue] = Map.empty
  ): IO[ValidationError, TestResult] = {
    val io = interpreter.flatMap(_.execute(query, variables = variables)).map(_.errors.headOption)
    io.map(assert(_)(isSome(hasField[CalibanError, String]("msg", _.msg, equalTo(expectedMessage)))))
  }

  override def spec =
    suite("ValidationSpec")(
      test("operation name uniqueness") {
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
      test("subscription has only one root") {
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
      test("subscription doesn't have a __typename field") {
        val query = gqldoc("""
             subscription s {
               __typename
              }""")
        check(query, "Subscription 's' has a field named '__typename'.")
      },
      test("invalid field") {
        val query = gqldoc("""
             {
               characters {
                 unknown
               }
              }""")
        check(query, "Field 'unknown' does not exist on type 'Character'.")
      },
      test("invalid field in fragment") {
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
      test("field on enum") {
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
      test("missing field on object") {
        val query = gqldoc("""
             {
               characters
              }""")
        check(query, "Field selection is mandatory on type 'Character'.")
      },
      test("invalid argument") {
        val query = gqldoc("""
             {
               characters(arg: 1) {
                 name
               }
              }""")
        check(query, "Argument 'arg' is not defined on field 'characters' of type 'Query'.")
      },
      test("missing argument") {
        val query = gqldoc("""
             {
               character(name: null) {
                 name
               }
              }""")
        check(query, "Required argument 'name' is null or missing on field 'character' of type 'Query'.")
      },
      test("duplicated fragment name") {
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
      test("fragment on invalid type") {
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
      test("fragment on impossible type") {
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
      test("fragment unused") {
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
      test("fragment spreads not defined") {
        val query = gqldoc("""
             query {
               characters {
                 ...f
               }
              }""")
        check(query, "Fragment spread 'f' is not defined.")
      },
      test("fragment cycle") {
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
      test("unsupported directive") {
        val query = gqldoc("""
             query {
               characters {
                 name @yolo()
               }
              }""")
        check(query, "Directive 'yolo' is not supported.")
      },
      test("variable defined twice") {
        val query = gqldoc("""
             query($name: String, $name: String) {
               characters {
                 name
               }
              }""")
        check(query, "Variable 'name' is defined more than once.")
      },
      test("invalid variable") {
        val query = gqldoc("""
             query($x: Character) {
               characters {
                 name
               }
              }""")
        check(query, "Type of variable 'x' is not a valid input type.")
      },
      test("variable not defined") {
        val query = gqldoc("""
             query {
               character(name: $x) {
                 name
               }
              }""")
        check(query, "Variable 'x' is not defined.")
      },
      test("variable not used") {
        val query = gqldoc("""
             query($x: String) {
               characters {
                 name
               }
              }""")
        check(query, "Variable 'x' is not used.")
      },
      test("variable used in list") {
        val query = gqldoc("""
             query($x: String) {
               charactersIn(names: [$x]){
                 name
               }
              }""")
        interpreter.flatMap(_.execute(query, None, Map("x" -> StringValue("y")))).map { response =>
          assertTrue(response.errors.isEmpty)
        }
      },
      test("variable used in object") {
        val query = gqldoc("""
             query($x: String!) {
               exists(character: { name: $x, nicknames: [], origin: EARTH })
              }""")
        interpreter.flatMap(_.execute(query, None, Map("x" -> StringValue("y")))).map { response =>
          assertTrue(response.errors.isEmpty)
        }
      },
      test("invalid input field") {
        val query = gqldoc("""
             query {
               exists(character: { unknown: "" })
             }""")
        check(query, "Input field 'unknown' is not defined on type 'CharacterInput'.")
      },
      test("required input field not defined") {
        val query = gqldoc("""
             query {
               exists(character: { name: "name" })
             }""")
        check(query, "Required field 'nicknames' on object 'CharacterInput' was not provided.")
      },
      test("directive used in wrong location") {
        val query = gqldoc("""
             query @skip(if: true) {
               characters {
                 name
               }
             }""")
        check(query, "Directive 'skip' is used in invalid location 'QUERY'.")
      },
      test("directive used twice") {
        val query = gqldoc("""
             query {
               characters {
                 name @skip(if: true) @skip(if: true)
               }
             }""")
        check(query, "Directive 'skip' is defined twice.")
      },
      test("variable types don't match") {
        val query = gqldoc("""
             query($x: Int!) {
               exists(character: { name: $x, nicknames: [], origin: EARTH })
              }""")
        check(
          query,
          "Variable 'x' usage is not allowed because its type doesn't match the schema (Int instead of String).",
          Map(
            "x" -> IntValue(1)
          )
        )
      },
      test("variable cardinality is the same") {
        val query = gqldoc("""
             query($x: [String]!) {
               exists(character: { name: $x, nicknames: [], origin: EARTH })
              }""")
        check(
          query,
          "Variable 'x' usage is not allowed because it is a list but it should not be.",
          Map(
            "x" -> InputValue.ListValue(List())
          )
        )
      },
      test("variable nullability is the same") {
        val query = gqldoc("""
             query($x: String) {
               exists(character: { name: $x, nicknames: [], origin: EARTH })
              }""")
        check(query, "Variable 'x' usage is not allowed because it is nullable and doesn't have a default value.")
      },
      test("variable nullability with default") {
        val query = gqldoc("""
             query($x: String = "test") {
               exists(character: { name: $x, nicknames: [], origin: EARTH })
              }""")
        interpreter.flatMap(_.execute(query, None, Map())).map { response =>
          assertTrue(response.errors.isEmpty)
        }
      },
      test("directive with variable of the wrong type") {
        val query = gqldoc("""
             query($x: String!) {
               characters {
                 name @skip(if: $x)
               }
             }""")
        check(
          query,
          "Variable 'x' usage is not allowed because its type doesn't match the schema (String instead of Boolean).",
          Map(
            "x" -> StringValue("foo")
          )
        )
      },
      test("directive with variable of the right type") {
        val query = gqldoc("""
             query($x: Boolean!) {
               characters {
                 name @skip(if: $x)
               }
             }""")
        interpreter.flatMap(_.execute(query, None, Map("x" -> BooleanValue(true)))).map { response =>
          assertTrue(response.errors.isEmpty)
        }
      }
    )
}
