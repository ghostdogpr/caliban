package caliban.execution

import caliban.CalibanError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.RootResolver
import caliban.schema.Annotations.GQLDefault
import zio.test.Assertion._
import zio.test._

import java.util.UUID

object DefaultValueSpec extends ZIOSpecDefault {
  sealed trait COLOR
  object COLOR {
    case object GREEN extends COLOR
    case object BLUE  extends COLOR
  }
  override def spec =
    suite("DefaultValueSpec")(
      suite("default value validation")(
        test("invalid string validation") {
          case class TestInput(@GQLDefault("1") string: String)
          case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(i => i.string)))
          gql.interpreter.exit.map(e => assert(e)(fails(isSubtype[CalibanError.ValidationError](anything))))
        },
        test("invalid int validation") {
          case class TestInput(@GQLDefault("\"1\"") int: Int)
          case class Query(test: TestInput => Int)
          val gql = graphQL(RootResolver(Query(i => i.int)))
          gql.interpreter.exit.map(e => assert(e)(fails(isSubtype[CalibanError.ValidationError](anything))))
        },
        test("invalid float validation") {
          case class TestInput(@GQLDefault("true") float: Float)
          case class Query(test: TestInput => Float)
          val gql = graphQL(RootResolver(Query(i => i.float)))
          gql.interpreter.exit.map(e => assert(e)(fails(isSubtype[CalibanError.ValidationError](anything))))
        },
        test("invalid id validation") {
          case class TestInput(@GQLDefault("1") id: UUID)
          case class Query(test: TestInput => UUID)
          val gql = graphQL(RootResolver(Query(i => i.id)))
          gql.interpreter.exit.map(e => assert(e)(fails(isSubtype[CalibanError.ValidationError](anything))))
        },
        test("invalid boolean validation") {
          case class TestInput(@GQLDefault("1") b: Boolean)
          case class Query(test: TestInput => Boolean)
          val gql = graphQL(RootResolver(Query(i => i.b)))
          gql.interpreter.exit.map(e => assert(e)(fails(isSubtype[CalibanError.ValidationError](anything))))
        },
        test("valid enum validation") {
          case class TestInput(@GQLDefault("GREEN") c: COLOR)
          case class Query(test: TestInput => COLOR)
          val gql = graphQL(RootResolver(Query(i => i.c)))
          gql.interpreter.map(i => assert(i)(anything))
        },
        test("valid enum validation accepts strings") {
          case class TestInput(@GQLDefault("GREEN") c: COLOR)
          case class Query(test: TestInput => COLOR)
          val gql = graphQL(RootResolver(Query(i => i.c)))
          gql.interpreter.map(i => assert(i)(anything))
        },
        test("invalid enum validation") {
          case class TestInput(@GQLDefault("PINK") c: COLOR)
          case class Query(test: TestInput => COLOR)
          val gql = graphQL(RootResolver(Query(i => i.c)))
          gql.interpreter.exit.map(e => assert(e)(fails(isSubtype[CalibanError.ValidationError](anything))))
        },
        test("invalid nullable validation") {
          case class TestInput(@GQLDefault("1") s: Option[String])
          case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(i => i.s.getOrElse("default"))))
          gql.interpreter.exit.map(e => assert(e)(fails(isSubtype[CalibanError.ValidationError](anything))))
        },
        test("valid nullable validation") {
          case class TestInput(@GQLDefault("\"1\"") s: Option[String])
          case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(i => i.s.getOrElse("default"))))
          gql.interpreter.map(i => assert(i)(anything))
        },
        test("valid nullable validation for null") {
          case class TestInput(@GQLDefault("null") s: Option[String])
          case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(i => i.s.getOrElse("default"))))
          gql.interpreter.map(i => assert(i)(anything))
        },
        test("invalid list validation") {
          case class TestInput(@GQLDefault("3") string: List[String])
          case class Query(test: TestInput => List[String])
          val gql = graphQL(RootResolver(Query(i => i.string)))
          gql.interpreter.exit.map(e => assert(e)(fails(isSubtype[CalibanError.ValidationError](anything))))
        },
        test("valid list validation") {
          case class TestInput(@GQLDefault("[\"string\"]") string: List[String])
          case class Query(test: TestInput => List[String])
          val gql = graphQL(RootResolver(Query(i => i.string)))
          gql.interpreter.map(i => assert(i)(anything))
        },
        test("invalid object validation") {
          case class Nested(field: String)
          case class TestInput(@GQLDefault("{field: 2}") nested: Nested)
          case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(v => v.nested.field)))
          gql.interpreter.exit.map(e => assert(e)(fails(isSubtype[CalibanError.ValidationError](anything))))
        },
        test("valid object validation") {
          case class Nested(field: String)
          case class TestInput(@GQLDefault("{field: \"2\"}") nested: Nested)
          case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(v => v.nested.field)))
          gql.interpreter.map(i => assert(i)(anything))
        }
      ),
      test("field default values") {
        case class TestInput(@GQLDefault("1") intValue: Int, stringValue: String)
        case class Query(testDefault: TestInput => Int)
        val api         = graphQL(RootResolver(Query(i => i.intValue)))
        val interpreter = api.interpreter
        val query       =
          """query{
            |  testDefault(stringValue: "Hi!")
            |}""".stripMargin
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"testDefault":1}""")
        }
      },
      test("invalid field default values") {
        case class TestInput(@GQLDefault("1.1") intValue: Int, stringValue: String)
        case class Query(testDefault: TestInput => Int)
        val api      = graphQL(RootResolver(Query(i => i.intValue)))
        val expected =
          "InputValue 'intValue' of Field 'testDefault' of Object 'Query' has invalid type 1.1"

        api.interpreter.exit.map(e => assert(e)(fails(hasMessage(equalTo(expected)))))
      },
      test("explicit null for a nullable field with default value is valid") {
        val query =
          """query {
            |  query(string: null)
            |}""".stripMargin

        case class TestInput(@GQLDefault("\"default\"") string: Option[String])
        case class Query(query: TestInput => String)
        val gql = graphQL(RootResolver(Query(_.string.getOrElse("default"))))

        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assertTrue(res.errors.isEmpty)
      },
      test("explicit null for a non-null field with default value is invalid") {
        val query =
          """query {
            |  query(string: null)
            |}""".stripMargin

        case class TestInput(@GQLDefault("\"default\"") string: String)
        case class Query(query: TestInput => String)
        val gql = graphQL(RootResolver(Query(_.string)))

        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assert(res.errors.headOption)(
          isSome(isSubtype[CalibanError.ValidationError](anything))
        )
      },
      test("it should render default values in the SDL") {
        case class TestInput(@GQLDefault("1") intValue: Int)
        case class Query(testDefault: TestInput => Int)
        val rendered = graphQL(RootResolver(Query(i => i.intValue))).render.trim

        assertTrue(
          rendered ==
            """|schema {
               |  query: Query
               |}
               |
               |type Query {
               |  testDefault(intValue: Int! = 1): Int!
               |}""".stripMargin.trim
        )
      },
      test("it renders in introspection") {
        val introspectionQuery = gqldoc("""
            query IntrospectionQuery {
              __schema {
                queryType { name }
                mutationType { name }
                subscriptionType { name }
                types {
                  ...FullType
                }
              }
            }

            fragment FullType on __Type {
              kind
              name
              fields(includeDeprecated: true) {
                name
                description
                args {
                  ...InputValue
                }
              }
              inputFields {
                ...InputValue
              }
            }

            fragment InputValue on __InputValue {
              name
              description
              defaultValue
            }
          """)

        case class TestInput(@GQLDefault("1") intValue: Int)
        case class Query(testDefault: TestInput => Int)
        val interpreter = graphQL(RootResolver(Query(i => i.intValue))).interpreter

        interpreter.flatMap(_.execute(introspectionQuery)).map { response =>
          assertTrue(
            response.data.toString ==
              """{"__schema":{"queryType":{"name":"Query"},"mutationType":null,"subscriptionType":null,"types":[{"kind":"SCALAR","name":"Boolean","fields":null,"inputFields":null},{"kind":"SCALAR","name":"Float","fields":null,"inputFields":null},{"kind":"SCALAR","name":"Int","fields":null,"inputFields":null},{"kind":"OBJECT","name":"Query","fields":[{"name":"testDefault","description":null,"args":[{"name":"intValue","description":null,"defaultValue":"1"}]}],"inputFields":null},{"kind":"SCALAR","name":"String","fields":null,"inputFields":null}]}}"""
          )
        }
      }
    )
}
