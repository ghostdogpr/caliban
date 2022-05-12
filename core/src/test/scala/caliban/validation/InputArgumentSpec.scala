package caliban.validation

import caliban.{ CalibanError, GraphQLRequest, InputValue, RootResolver, TestUtils, Value }
import caliban.GraphQL._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object InputArgumentSpec extends DefaultRunnableSpec {
  sealed trait Enum
  case object Valid extends Enum

  case class BoolArg(input: Option[Boolean])
  case class BoolArgNonNull(input: Boolean)
  case class FloatArg(input: Option[Float])
  case class IntArg(input: Option[Int])
  case class ListArg(input: Option[List[String]])
  case class StringArg(input: Option[String])
  case class EnumArg(input: Option[Enum])

  // TODO: INPUT_OBJECT

  case class TestOutput(value: String)
  case class Query(
    bool: BoolArg => String = _ => "result",
    boolNonNull: BoolArgNonNull => String = _ => "result",
    float: FloatArg => String = _ => "result",
    int: IntArg => String = _ => "result",
    list: ListArg => String = _ => "result",
    string: StringArg => String = _ => "result",
    `enum`: EnumArg => String = _ => "result"
  )
  val gql = graphQL(RootResolver(Query()))

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
                "Variable 'list' with value 2 cannot be coerced into into [String].",
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
        }
      )
    )
}
