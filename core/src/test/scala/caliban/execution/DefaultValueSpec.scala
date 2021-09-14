package caliban.execution

import caliban.CalibanError
import caliban.CalibanError.ExecutionError
import caliban.GraphQL
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.RootResolver
import caliban.TestUtils._
import caliban.Value.BooleanValue
import caliban.Value.IntValue
import caliban.Value.StringValue
import caliban.introspection.adt.__Type
import caliban.parsing.adt.LocationInfo
import caliban.schema.Annotations.GQLDefault
import caliban.schema.Annotations.GQLInterface
import caliban.schema.Annotations.GQLName
import caliban.schema.Annotations.GQLValueType
import caliban.schema.ArgBuilder
import caliban.schema.Schema
import caliban.schema.Step
import caliban.schema.Types
import zio.IO
import zio.Task
import zio.UIO
import zio.ZIO
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._
import zio.test.environment.TestEnvironment

import java.util.UUID

object DefaultValueSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("DefaultValueSpec")(
      suite("default value validation")(
        testM("string validation") {
          case class TestInput(@GQLDefault("1") string: String)
          case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(i => i.string)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("int validation") {
          case class TestInput(@GQLDefault("\"1\"") int: Int)
          case class Query(test: TestInput => Int)
          val gql = graphQL(RootResolver(Query(i => i.int)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("float validation") {
          case class TestInput(@GQLDefault("1") float: Float)
          case class Query(test: TestInput => Float)
          val gql = graphQL(RootResolver(Query(i => i.float)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("id validation") {
          case class TestInput(@GQLDefault("1") id: UUID)
          case class Query(test: TestInput => UUID)
          val gql = graphQL(RootResolver(Query(i => i.id)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("boolean validation") {
          case class TestInput(@GQLDefault("1") b: Boolean)
          case class Query(test: TestInput => Boolean)
          val gql = graphQL(RootResolver(Query(i => i.b)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("nullable validation") {
          case class TestInput(@GQLDefault("1") s: Option[String])
          case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(i => i.s.getOrElse("default"))))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("successful nullable validation") {
          case class TestInput(@GQLDefault("\"1\"") s: Option[String])
          case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(i => i.s.getOrElse("default"))))
          assertM(gql.interpreter)(anything)
        },
        testM("list validation") {
          case class TestInput(@GQLDefault("\"string\"") string: List[String])
          case class Query(test: TestInput => List[String])
          val gql = graphQL(RootResolver(Query(i => i.string)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("successful list validation") {
          case class TestInput(@GQLDefault("[\"string\"]") string: List[String])
          case class Query(test: TestInput => List[String])
          val gql = graphQL(RootResolver(Query(i => i.string)))
          assertM(gql.interpreter)(anything)
        },
        testM("object validation") {
          case class Nested(field: String)
          case class TestInput(@GQLDefault("{field: 2}") nested: Nested)
          case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(v => v.nested.field)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("successful object validation") {
          case class Nested(field: String)
          case class TestInput(@GQLDefault("{field: \"2\"}") nested: Nested)
          case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(v => v.nested.field)))
          assertM(gql.interpreter)(anything)
        }
      ),
      testM("field default values") {
        case class TestInput(@GQLDefault("1") intValue: Int, stringValue: String)
        case class Query(testDefault: TestInput => Int)
        val api         = graphQL(RootResolver(Query(i => i.intValue)))
        val interpreter = api.interpreter
        val query       =
          """query{
            |  testDefault(stringValue: "Hi!")
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"testDefault":1}"""))
      },
      testM("invalid field default values") {
        case class TestInput(@GQLDefault("1.1") intValue: Int, stringValue: String)
        case class Query(testDefault: TestInput => Int)
        val api      = graphQL(RootResolver(Query(i => i.intValue)))
        val expected =
          "InputValue 'intValue' of Field 'testDefault' of Object 'Query' has invalid type 1.1"

        assertM(api.interpreter.run)(fails(hasMessage(equalTo(expected))))
      },
      test("it should render default values in the SDL") {
        case class TestInput(@GQLDefault("1") intValue: Int)
        case class Query(testDefault: TestInput => Int)
        val rendered = graphQL(RootResolver(Query(i => i.intValue))).render.trim

        assert(rendered)(equalTo("""|schema {
                                    |  query: Query
                                    |}
                                    |
                                    |type Query {
                                    |  testDefault(intValue: Int! = 1): Int!
                                    |}""".stripMargin.trim))
      },
      testM("it renders in introspection") {
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

        assertM(interpreter.flatMap(_.execute(introspectionQuery)).map(_.data.toString))(
          equalTo(
            """{"__schema":{"queryType":{"name":"Query"},"mutationType":null,"subscriptionType":null,"types":[{"kind":"SCALAR","name":"Boolean","fields":null,"inputFields":null},{"kind":"SCALAR","name":"Int","fields":null,"inputFields":null},{"kind":"OBJECT","name":"Query","fields":[{"name":"testDefault","description":null,"args":[{"name":"intValue","description":null,"defaultValue":"1"}]}],"inputFields":null}]}}"""
          )
        )
      }
    )
}
