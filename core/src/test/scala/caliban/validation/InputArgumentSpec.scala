package caliban.validation

import caliban.{ CalibanError, GraphQLRequest, InputValue, ResponseValue, RootResolver, TriState, Value }
import caliban.GraphQL._
import caliban.schema.Schema
import caliban.schema.Step
import zio.test._
import zio.test.environment.TestEnvironment

object InputArgumentSpec extends DefaultRunnableSpec {
  sealed trait Enum
  case object Valid extends Enum

  case class InputObject(int: Option[Int])

  case class BoolArg(input: Option[Boolean])
  case class BoolArgNonNull(input: Boolean)
  case class FloatArg(input: Option[Float])
  case class IntArg(input: Option[Int])
  case class ListArg(input: Option[List[String]])
  case class ListIntArg(input: Option[List[Option[Int]]])
  case class ListListIntArg(input: Option[List[Option[List[Option[Int]]]]])
  case class StringArg(input: Option[String])
  case class EnumArg(input: Option[Enum])
  case class InputArg(input: Option[InputObject])

  case class ExampleObject(a: TriState[String], b: Int)
  object ExampleObject {
    implicit val schemaTriState: Schema[Any, TriState[String]] =
      TriState.schemaCustom(Step.PureStep(Value.StringValue("__undefined__")))

    implicit val schema: Schema[Any, ExampleObject] = Schema.genMacro[ExampleObject].schema
  }
  case class ExampleObjectArg(input: Option[ExampleObject])

  case class Query(
    bool: BoolArg => String = _ => "result",
    boolNonNull: BoolArgNonNull => String = _ => "result",
    float: FloatArg => String = _ => "result",
    int: IntArg => String = _ => "result",
    list: ListArg => String = _ => "result",
    listInt: ListIntArg => Option[List[Option[Int]]] = _.input,
    listListInt: ListListIntArg => Option[List[Option[List[Option[Int]]]]] = _.input,
    string: StringArg => String = _ => "result",
    `enum`: EnumArg => String = _ => "result",
    input: InputArg => String = _ => "result",
    exampleObject: ExampleObjectArg => Option[ExampleObject] = _.input
  )
  val gql = graphQL(RootResolver(Query()))

  private def execute(query: String, variables: Map[String, InputValue] = Map.empty) = for {
    int <- gql.interpreter
    res <- int.executeRequest(GraphQLRequest(query = Some(query), variables = Some(variables)))
  } yield res

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("InputArgumentSpec")(
      testM("non-null") {
        val query =
          """query QueryName($bool: Boolean!) {
            |  bool(input: $bool)
            |}""".stripMargin

        for {
          int <- gql.interpreter
          res <- int.executeRequest(
                   GraphQLRequest(
                     query = Some(query),
                     variables = Some(Map())
                   )
                 )
        } yield assertTrue(
          res.errors == List(
            CalibanError.ValidationError(
              "Variable 'bool' is null but is specified to be non-null.",
              "The value of a variable must be compatible with its type."
            )
          )
        )
      },
      suite("bools")(
        testM("invalid coercion") {
          val query =
            """query QueryName($bool: Boolean!) {
              |  bool(input: $bool)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("bool" -> Value.StringValue("true")))
                     )
                   )
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Variable 'bool' with value \"true\" cannot be coerced into Boolean.",
                "Variable values need to follow GraphQL's coercion rules."
              )
            )
          )
        },
        testM("valid coercion") {
          val query =
            """query QueryName($bool: Boolean!) {
              |  bool(input: $bool)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("bool" -> Value.BooleanValue(true)))
                     )
                   )
          } yield assertTrue(res.errors == List())
        }
      ),
      suite("strings")(
        testM("invalid coercion") {
          val query =
            """query QueryName($string: String!) {
              |  string(input: $string)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("string" -> Value.BooleanValue(false)))
                     )
                   )
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Variable 'string' with value false cannot be coerced into String.",
                "Variable values need to follow GraphQL's coercion rules."
              )
            )
          )
        },
        testM("valid coercion") {
          val query =
            """query QueryName($string: String!) {
              |  string(input: $string)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("string" -> Value.StringValue("value")))
                     )
                   )
          } yield assertTrue(res.errors == List())
        }
      ),
      suite("ints")(
        testM("invalid coercion") {
          val query =
            """query QueryName($int: Int!) {
              |  int(input: $int)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("int" -> Value.StringValue("invalid")))
                     )
                   )
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Variable 'int' with value \"invalid\" cannot be coerced into Int.",
                "Variable values need to follow GraphQL's coercion rules."
              )
            )
          )
        },
        testM("valid coercion") {
          val query =
            """query QueryName($int: Int!) {
              |  int(input: $int)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("int" -> Value.IntValue(1)))
                     )
                   )
          } yield assertTrue(res.errors == List())
        }
      ),
      suite("floats")(
        testM("invalid coercion") {
          val query =
            """query QueryName($float: Float!) {
              |  float(input: $float)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("float" -> Value.StringValue("invalid")))
                     )
                   )
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Variable 'float' with value \"invalid\" cannot be coerced into Float.",
                "Variable values need to follow GraphQL's coercion rules."
              )
            )
          )
        },
        testM("valid coercion (int -> float)") {
          val query =
            """query QueryName($float: Float!) {
              |  float(input: $float)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("float" -> Value.IntValue.IntNumber(1)))
                     )
                   )
          } yield assertTrue(res.errors == List())
        },
        testM("valid coercion (long -> float)") {
          val query =
            """query QueryName($float: Float!) {
              |  float(input: $float)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("float" -> Value.IntValue.LongNumber(1L)))
                     )
                   )
          } yield assertTrue(res.errors == List())
        },
        testM("valid coercion (bigint -> float)") {
          val query =
            """query QueryName($float: Float!) {
              |  float(input: $float)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("float" -> Value.IntValue.BigIntNumber(BigInt(1))))
                     )
                   )
          } yield assertTrue(res.errors == List())
        }
      ),
      suite("enums")(
        testM("invalid coercion") {
          val query =
            """query QueryName($enum: Enum) {
              |  enum(input: $enum)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("enum" -> Value.IntValue(2)))
                     )
                   )
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Variable 'enum' with value 2 cannot be coerced into Enum.",
                "Variable values need to follow GraphQL's coercion rules."
              )
            )
          )
        },
        testM("valid coercion") {
          val query =
            """query QueryName($enum: Enum!) {
              |  enum(input: $enum)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("enum" -> Value.StringValue("Valid")))
                     )
                   )
          } yield assertTrue(res.errors == List())
        }
      ),
      suite("lists")(
        testM("invalid coercion") {
          val query =
            """query QueryName($list: [String!]!) {
              |  list(input: $list)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("list" -> InputValue.ListValue(List(Value.IntValue(2)))))
                     )
                   )
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Variable 'list' at index '0' with value 2 cannot be coerced into String.",
                "Variable values need to follow GraphQL's coercion rules."
              )
            )
          )
        },
        testM("invalid coercion") {
          val query =
            """query QueryName($list: [String!]!) {
              |  list(input: $list)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("list" -> Value.IntValue(2)))
                     )
                   )
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Variable 'list' with value 2 cannot be coerced into String.",
                "Variable values need to follow GraphQL's coercion rules."
              )
            )
          )
        },
        testM("valid coercion") {
          val query =
            """query QueryName($list: [String!]!) {
              |  list(input: $list)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(Map("list" -> InputValue.ListValue(List(Value.StringValue("valid")))))
                     )
                   )
          } yield assertTrue(res.errors == List())
        },
        // test cases from http://spec.graphql.org/October2021/#sec-List.Input-Coercion
        suite("[Int]")(
          testM("[1, 2, 3] -> [1, 2, 3]") {
            val query = """
              query QueryName($input: [Int!]!) {
                listInt(input: $input)
              }
            """
            val list  = List(Value.IntValue(1), Value.IntValue(2), Value.IntValue(3))
            val vars  = Map("input" -> InputValue.ListValue(list))

            for {
              res <- execute(query, vars)
            } yield assertTrue(res.data == ResponseValue.ObjectValue(List("listInt" -> ResponseValue.ListValue(list))))
          },
          testM("""[1, "b", true, 4] -> errors""") {
            val query = """
              query QueryName($input: [Int!]!) {
                listInt(input: $input)
              }
            """
            val list  = List(Value.IntValue(1), Value.StringValue("b"), Value.BooleanValue(true), Value.IntValue(4))
            val vars  = Map("input" -> InputValue.ListValue(list))

            for {
              res <- execute(query, vars)
            } yield assertTrue(
              res.errors == List(
                CalibanError.ValidationError(
                  "Variable 'input' at index '1' with value \"b\" cannot be coerced into Int.",
                  "Variable values need to follow GraphQL's coercion rules."
                )
              )
            )
          },
          testM("1 -> [1]") {
            val query = """
              query QueryName($input: [Int!]!) {
                listInt(input: $input)
              }
            """
            val input = Value.IntValue(1)
            val vars  = Map("input" -> input)

            for {
              res <- execute(query, vars)
            } yield assertTrue(
              res.data == ResponseValue.ObjectValue(List("listInt" -> ResponseValue.ListValue(List(input))))
            )
          },
          testM("null -> null") {
            val query = """
              query QueryName($input: [Int]) {
                listInt(input: $input)
              }
            """
            val input = Value.NullValue
            val vars  = Map("input" -> input)

            for {
              res <- execute(query, vars)
            } yield assertTrue(res.data == ResponseValue.ObjectValue(List("listInt" -> input)))
          }
        ),
        suite("[[Int]]")(
          testM("[[1], [2, 3]] -> [[1], [2, 3]]") {
            val query = """
              query QueryName($input: [[Int!]!]!) {
                listListInt(input: $input)
              }
            """
            val list  = List(
              InputValue.ListValue(List(Value.IntValue(1))),
              InputValue.ListValue(List(Value.IntValue(2), Value.IntValue(3)))
            )
            val vars  = Map("input" -> InputValue.ListValue(list))

            for {
              res <- execute(query, vars)
            } yield assertTrue(
              res.data == ResponseValue.ObjectValue(
                List(
                  "listListInt" -> ResponseValue.ListValue(
                    List(
                      ResponseValue.ListValue(List(Value.IntValue(1))),
                      ResponseValue.ListValue(List(Value.IntValue(2), Value.IntValue(3)))
                    )
                  )
                )
              )
            )
          },
          // note that the spec says this should be an error, but the reference implementation allows it
          testM("[1, 2, 3] -> [[1], [2], [3]]") {
            val query = """
              query QueryName($input: [[Int!]!]!) {
                listListInt(input: $input)
              }
            """
            val list  = List(Value.IntValue(1), Value.IntValue(2), Value.IntValue(3))
            val vars  = Map("input" -> InputValue.ListValue(list))

            for {
              res <- execute(query, vars)
            } yield assertTrue(
              res.data == ResponseValue.ObjectValue(
                List(
                  "listListInt" -> ResponseValue.ListValue(
                    List(
                      ResponseValue.ListValue(List(Value.IntValue(1))),
                      ResponseValue.ListValue(List(Value.IntValue(2))),
                      ResponseValue.ListValue(List(Value.IntValue(3)))
                    )
                  )
                )
              )
            )
          },
          testM("1 -> [[1]]") {
            val query = """
              query QueryName($input: [[Int!]!]!) {
                listListInt(input: $input)
              }
            """
            val input = Value.IntValue(1)
            val vars  = Map("input" -> input)

            for {
              res <- execute(query, vars)
            } yield assertTrue(
              res.data == ResponseValue.ObjectValue(
                List("listListInt" -> ResponseValue.ListValue(List(ResponseValue.ListValue(List(input)))))
              )
            )
          },
          testM("null -> null") {
            val query = """
              query QueryName($input: [[Int]]) {
                listListInt(input: $input)
              }
            """
            val input = Value.NullValue
            val vars  = Map("input" -> input)

            for {
              res <- execute(query, vars)
            } yield assertTrue(res.data == ResponseValue.ObjectValue(List("listListInt" -> input)))
          },
          testM("[1, [null], null] -> [[1], [null], null]") {
            val query = """
              query QueryName($input: [[Int]]) {
                listListInt(input: $input)
              }
            """
            val list  = List(Value.IntValue(1), InputValue.ListValue(List(Value.NullValue)), Value.NullValue)
            val vars  = Map("input" -> InputValue.ListValue(list))

            for {
              res <- execute(query, vars)
            } yield assertTrue(
              res.data == ResponseValue.ObjectValue(
                List(
                  "listListInt" -> ResponseValue.ListValue(
                    List(
                      ResponseValue.ListValue(List(Value.IntValue(1))),
                      ResponseValue.ListValue(List(Value.NullValue)),
                      Value.NullValue
                    )
                  )
                )
              )
            )
          }
        )
      ),
      suite("input objects")(
        testM("invalid coercion") {
          val query =
            """query QueryName($input: InputObjectInput!) {
              |  input(input: $input)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(
                         Map(
                           "input" -> InputValue.ObjectValue(
                             Map(
                               "int" -> Value.StringValue("invalid")
                             )
                           )
                         )
                       )
                     )
                   )
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Variable 'input' at field 'int' with value \"invalid\" cannot be coerced into Int.",
                "Variable values need to follow GraphQL's coercion rules."
              )
            )
          )
        },
        testM("valid coercion") {
          val query =
            """query QueryName($input: InputObjectInput!) {
              |  input(input: $input)
              |}""".stripMargin

          for {
            int <- gql.interpreter
            res <- int.executeRequest(
                     GraphQLRequest(
                       query = Some(query),
                       variables = Some(
                         Map(
                           "input" -> InputValue.ObjectValue(
                             Map(
                               "int" -> Value.IntValue(2)
                             )
                           )
                         )
                       )
                     )
                   )
          } yield assertTrue(res.errors == List())
        },
        // test cases from http://spec.graphql.org/October2021/#sec-Input-Objects.Input-Coercion
        testM("""{ a: "abc", b: 123 } + {} -> { a: "abc", b: 123 }""") {
          val query = """
            query QueryName {
              exampleObject(input: { a: "abc", b: 123 }) { a, b }
            }
          """

          for {
            res <- execute(query)
          } yield assertTrue(
            res.data == ResponseValue.ObjectValue(
              List(
                "exampleObject" -> ResponseValue.ObjectValue(
                  List(
                    "a" -> Value.StringValue("abc"),
                    "b" -> Value.IntValue(123)
                  )
                )
              )
            )
          )
        },
        testM("""{ a: null, b: 123 } + {} -> { a: null, b: 123 }""") {
          val query = """
            query QueryName {
              exampleObject(input: { a: null, b: 123 }) { a, b }
            }
          """

          for {
            res <- execute(query)
          } yield assertTrue(
            res.data == ResponseValue.ObjectValue(
              List(
                "exampleObject" -> ResponseValue.ObjectValue(
                  List(
                    "a" -> Value.NullValue,
                    "b" -> Value.IntValue(123)
                  )
                )
              )
            )
          )
        },
        testM("""{ b: 123 } + {} -> { b: 123 }""") {
          val query = """
            query QueryName {
              exampleObject(input: { b: 123 }) { a, b }
            }
          """

          for {
            res <- execute(query)
          } yield assertTrue(
            res.data == ResponseValue.ObjectValue(
              List(
                "exampleObject" -> ResponseValue.ObjectValue(
                  List(
                    "a" -> Value.StringValue("__undefined__"),
                    "b" -> Value.IntValue(123)
                  )
                )
              )
            )
          )
        },
        testM("""{ a: $var, b: 123 } + { var: null } -> { a: null, b: 123 }""") {
          val query = """
            query QueryName($var: String) {
              exampleObject(input: { a: $var, b: 123 }) { a, b }
            }
          """
          val vars  = Map("var" -> Value.NullValue)

          for {
            res <- execute(query, vars)
          } yield assertTrue(
            res.data == ResponseValue.ObjectValue(
              List(
                "exampleObject" -> ResponseValue.ObjectValue(
                  List(
                    "a" -> Value.NullValue,
                    "b" -> Value.IntValue(123)
                  )
                )
              )
            )
          )
        },
        testM("""{ a: $var, b: 123 } + {} -> { a: null, b: 123 }""") {
          val query = """
            query QueryName($var: String) {
              exampleObject(input: { a: $var, b: 123 }) { a, b }
            }
          """

          for {
            res <- execute(query)
          } yield assertTrue(
            res.data == ResponseValue.ObjectValue(
              List(
                "exampleObject" -> ResponseValue.ObjectValue(
                  List(
                    "a" -> Value.StringValue("__undefined__"),
                    "b" -> Value.IntValue(123)
                  )
                )
              )
            )
          )
        },
        testM("""{ b: $var } + { var: 123 } -> { b: 123 }""") {
          val query = """
            query QueryName($var: Int!) {
              exampleObject(input: { b: $var }) { a, b }
            }
          """
          val vars  = Map("var" -> Value.IntValue(123))

          for {
            res <- execute(query, vars)
          } yield assertTrue(
            res.data == ResponseValue.ObjectValue(
              List(
                "exampleObject" -> ResponseValue.ObjectValue(
                  List(
                    "a" -> Value.StringValue("__undefined__"),
                    "b" -> Value.IntValue(123)
                  )
                )
              )
            )
          )
        },
        testM("""$var + { var: { b: 123 } } -> { b: 123 }""") {
          val query = """
            query QueryName($var: ExampleObjectInput!) {
              exampleObject(input: $var) { a, b }
            }
          """
          val vars  = Map("var" -> InputValue.ObjectValue(Map("b" -> Value.IntValue(123))))

          for {
            res <- execute(query, vars)
          } yield assertTrue(
            res.data == ResponseValue.ObjectValue(
              List(
                "exampleObject" -> ResponseValue.ObjectValue(
                  List(
                    "a" -> Value.StringValue("__undefined__"),
                    "b" -> Value.IntValue(123)
                  )
                )
              )
            )
          )
        },
        testM(""""abc123" + {} -> errors""") {
          val query = """
            query QueryName {
              exampleObject(input: "abc123") { a, b }
            }
          """

          for {
            res <- execute(query)
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "InputValue 'input' of Field 'exampleObject' has invalid type: \"abc123\"",
                "Input field was supposed to be an input object."
              )
            )
          )
        },
        testM("""$var + { var: "abc123" } -> errors""") {
          val query = """
            query QueryName($var: ExampleObjectInput!) {
              exampleObject(input: $var) { a, b }
            }
          """
          val vars  = Map("var" -> Value.StringValue("abc123"))

          for {
            res <- execute(query, vars)
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Variable 'var' cannot coerce \"abc123\" to ExampleObjectInput",
                "Variable values need to follow GraphQL's coercion rules."
              )
            )
          )
        },
        testM("""{ a: "abc", b: "123" } + {} -> errors""") {
          val query = """
            query QueryName {
              exampleObject(input: { a: "abc", b: "123" }) { a, b }
            }
          """

          for {
            res <- execute(query)
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "InputValue 'input' of Field 'b' of InputObject 'ExampleObjectInput' has invalid type \"123\"",
                "Expected 'Int'"
              )
            )
          )
        },
        testM("""{ a: "abc" } + {} -> errors""") {
          val query = """
            query QueryName {
              exampleObject(input: { a: "abc" }) { a, b }
            }
          """

          for {
            res <- execute(query)
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Required field 'b' on object 'ExampleObjectInput' was not provided.",
                "Input object fields may be required. Much like a field may have required arguments, an input object may have required fields. An input field is required if it has a non‐null type and does not have a default value. Otherwise, the input object field is optional."
              )
            )
          )
        },
        testM("""{ b: $var } + {} -> errors""") {
          val query = """
            query QueryName($var: Int!) {
              exampleObject(input: { b: $var }) { a, b }
            }
          """

          for {
            res <- execute(query)
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Variable 'var' is null but is specified to be non-null.",
                "The value of a variable must be compatible with its type."
              )
            )
          )
        },
        testM("""$var + { var: { a: "abc" } } -> errors""") {
          val query = """
            query QueryName($var: ExampleObjectInput) {
              exampleObject(input: $var) { a, b }
            }
          """
          val vars  = Map("var" -> InputValue.ObjectValue(Map("a" -> Value.StringValue("abc"))))

          for {
            res <- execute(query, vars)
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Field b in InputValue 'input' of Field 'exampleObject' is null",
                "Input field was null but was supposed to be non-null."
              )
            )
          )
        },
        testM("""{ a: "abc", b: null } + {} -> errors""") {
          val query = """
            query QueryName {
              exampleObject(input: { a: "abc", b: null }) { a, b }
            }
          """

          for {
            res <- execute(query)
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "InputValue 'input' of Field 'b' of InputObject 'ExampleObjectInput' is null",
                "Input field was null but was supposed to be non-null."
              )
            )
          )
        },
        testM("""{ b: $var } + { var: null } -> errors""") {
          val query = """
            query QueryName($var: Int!) {
              exampleObject(input: { b: $var }) { a, b }
            }
          """
          val vars  = Map("var" -> Value.NullValue)

          for {
            res <- execute(query, vars)
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "InputValue 'input' of Field 'b' of InputObject 'ExampleObjectInput' is null",
                "Input field was null but was supposed to be non-null."
              )
            )
          )
        },
        testM("""{ b: 123, c: "xyz" } + {} -> errors""") {
          val query = """
            query QueryName {
              exampleObject(input: { b: 123, c: "xyz" }) { a, b }
            }
          """

          for {
            res <- execute(query)
          } yield assertTrue(
            res.errors == List(
              CalibanError.ValidationError(
                "Input field 'c' is not defined on type 'ExampleObjectInput'.",
                "Every input field provided in an input object value must be defined in the set of possible fields of that input object’s expected type."
              )
            )
          )
        }
      )
    )
}
