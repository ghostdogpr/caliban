package caliban.validation

import caliban._
import caliban.CalibanError.ValidationError
import caliban.InputValue.ObjectValue
import caliban.Macros.gqldoc
import caliban.TestUtils._
import caliban.Value.{ BooleanValue, IntValue, NullValue, StringValue }
import caliban.schema.Annotations.{ GQLDefault, GQLOneOfInput, GQLOneOfInputName, GQLValueType }
import caliban.schema.{ ArgBuilder, Schema }
import zio.{ IO, UIO, ZIO }
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
        check(query, "Directive 'skip' is defined more than once.")
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
      },
      test("validation works when a non-nullable field is missing but we have a default value") {
        import caliban.schema.ArgBuilder.auto._
        import caliban.schema.Schema.auto._

        case class Foo(inner: Boolean)
        case class Outer(bar: String, @GQLDefault("""{inner: true}""") foo: Foo)
        case class GetFooArgs(outer: Outer)
        case class Queries(example: GetFooArgs => UIO[String])

        val queries = Queries(example = _ => ZIO.succeed("Done"))

        val api = graphQL(RootResolver(queries))

        val query = gqldoc("""{ example(outer: { bar: "bar" }) }""")
        api.interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.errors.isEmpty)
        }
      },
      suite("@oneOf inputs") {
        import caliban.schema.Schema.auto._
        import caliban.schema.ArgBuilder.auto._

        @GQLOneOfInput
        sealed trait Foo
        object Foo {
          @GQLValueType
          case class FooString(stringValue: String) extends Foo
          @GQLOneOfInputName("fooInt")
          case class IntObj(intValue: Int) extends Foo
          case class Wrapper(fooInput: Foo)
        }

        case class Bar(foo2: Foo => String)
        case class Queries(foo: Foo.Wrapper => String, fooUnwrapped: Foo => String, bar: Bar)

        implicit val fooStringAb: ArgBuilder[Foo.FooString] = ArgBuilder.gen
        implicit val fooIntAb: ArgBuilder[Foo.IntObj]       = ArgBuilder.gen
        implicit val fooAb: ArgBuilder[Foo]                 = ArgBuilder.gen
        implicit val barSchema: Schema[Any, Bar]            = Schema.gen
        implicit val schema: Schema[Any, Queries]           = Schema.gen

        val api: GraphQL[Any] = graphQL(RootResolver(Queries(_.fooInput.toString, _.toString, Bar(_.toString))))

        def argumentsQuery(arg1: ObjectValue, arg2: ObjectValue, arg3: ObjectValue) =
          s"""
             |{
             |  foo(fooInput: ${arg1.toInputString})
             |
             |  fooUnwrapped(value: ${arg2.toInputString})
             |
             |  bar {
             |    foo2(value: ${arg3.toInputString} )
             |  }
             |}
             |""".stripMargin

        val variablesQuery =
          """
            |query Foo($args1: FooInput!, $args2: FooInput!, $args3: FooInput!){
            |  foo(fooInput: $args1)
            |
            |  fooUnwrapped(value: $args2)
            |
            |  bar {
            |    foo2(value: $args3 )
            |  }
            |}
            |""".stripMargin

        val validInputs = List(
          Map("fooString" -> StringValue("hello")),
          Map("fooInt"    -> ObjectValue(Map("intValue" -> IntValue(42))))
        ).map(ObjectValue(_))

        val invalidInputs = List(
          Map.empty[String, InputValue],
          Map("fooString" -> NullValue),
          Map("fooString" -> StringValue("foo"), "fooInt" -> NullValue),
          Map("fooString" -> StringValue("foo"), "fooInt" -> ObjectValue(Map("intValue" -> IntValue(42))))
        ).map(ObjectValue(_))

        val validVariablesCases = validInputs.map(arg => Map("args1" -> arg, "args2" -> arg, "args3" -> arg))

        List(
          test("valid field arguments") {
            val cases = validInputs.map(v => argumentsQuery(v, v, v))
            ZIO.foldLeft(cases)(assertCompletes) { case (acc, query) =>
              api.interpreter
                .flatMap(_.execute(query))
                .map(resp => acc && assertTrue(resp.errors.isEmpty))
            }
          },
          test("valid variables") {
            ZIO.foldLeft(validVariablesCases)(assertCompletes) { case (acc, variables) =>
              api.interpreter
                .flatMap(_.execute(variablesQuery, variables = variables))
                .map(resp => acc && assertTrue(resp.errors.isEmpty))
            }
          },
          test("invalid field arguments") {
            val cases = for {
              invalid <- invalidInputs
              valid    = validInputs.head
              _case   <- List(
                           argumentsQuery(invalid, valid, valid),
                           argumentsQuery(valid, invalid, valid),
                           argumentsQuery(valid, valid, invalid)
                         )
            } yield _case

            ZIO.foldLeft(cases)(assertCompletes) { case (acc, query) =>
              api.interpreter
                .flatMap(_.execute(query))
                .map(resp =>
                  acc && assertTrue(
                    resp.errors.nonEmpty && resp.errors.forall {
                      case ValidationError(msg, _, _, _) => msg.contains("is not a valid @oneOf input")
                      case _                             => false
                    }
                  )
                )
            }
          },
          test("invalid variables") {
            val cases = for {
              argName <- validVariablesCases.head.keys
              invalid <- invalidInputs
            } yield validVariablesCases.head.updated(argName, invalid)

            ZIO.foldLeft(cases)(assertCompletes) { case (acc, variables) =>
              api.interpreter
                .flatMap(_.execute(variablesQuery, variables = variables))
                .map(resp =>
                  acc && assertTrue(
                    resp.errors.nonEmpty && resp.errors.forall {
                      case ValidationError(msg, _, _, _) => msg.contains("is not a valid @oneOf input")
                      case _                             => false
                    }
                  )
                )
            }
          },
          test("schema is valid") {
            api.validateRootSchema.as(assertCompletes)
          },
          test("schema is invalid") {
            @GQLOneOfInput
            sealed trait Foo
            object Foo {
              case class Bar(value: String) extends Foo
            }

            case class Queries(foo: Foo => String)
            implicit val abInner: ArgBuilder[Foo.Bar] = ArgBuilder.gen
            implicit val ab: ArgBuilder[Foo]          = ArgBuilder.gen
            implicit val schema: Schema[Any, Queries] = Schema.gen

            graphQL(RootResolver(Queries(_.toString))).validateRootSchema
              .fold(_ => assertCompletes, _ => assertNever("Schema should be invalid"))
          }
        )
      }
    )
}
