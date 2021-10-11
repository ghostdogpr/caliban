package caliban.execution

import caliban.CalibanError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.RootResolver
import caliban.schema.Annotations.GQLDefault
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

import java.util.UUID

object DefaultValueSpec extends DefaultRunnableSpec {
  sealed trait COLOR
  object COLOR {
    case object GREEN extends COLOR
    case object BLUE  extends COLOR
  }
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("DefaultValueSpec")(
      suite("default value validation")(
        testM("invalid string validation") {
          final case class TestInput(@GQLDefault("1") string: String)
          final case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(i => i.string)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("invalid int validation") {
          final case class TestInput(@GQLDefault("\"1\"") int: Int)
          final case class Query(test: TestInput => Int)
          val gql = graphQL(RootResolver(Query(i => i.int)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("invalid float validation") {
          final case class TestInput(@GQLDefault("1") float: Float)
          final case class Query(test: TestInput => Float)
          val gql = graphQL(RootResolver(Query(i => i.float)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("invalid id validation") {
          final case class TestInput(@GQLDefault("1") id: UUID)
          final case class Query(test: TestInput => UUID)
          val gql = graphQL(RootResolver(Query(i => i.id)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("invalid boolean validation") {
          final case class TestInput(@GQLDefault("1") b: Boolean)
          final case class Query(test: TestInput => Boolean)
          val gql = graphQL(RootResolver(Query(i => i.b)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("valid enum validation") {
          final case class TestInput(@GQLDefault("GREEN") c: COLOR)
          final case class Query(test: TestInput => COLOR)
          val gql = graphQL(RootResolver(Query(i => i.c)))
          assertM(gql.interpreter.run)(anything)
        },
        testM("valid enum validation accepts strings") {
          final case class TestInput(@GQLDefault("\"GREEN\"") c: COLOR)
          final case class Query(test: TestInput => COLOR)
          val gql = graphQL(RootResolver(Query(i => i.c)))
          assertM(gql.interpreter.run)(anything)
        },
        testM("valid enum validation") {
          final case class TestInput(@GQLDefault("PINK") c: COLOR)
          final case class Query(test: TestInput => COLOR)
          val gql = graphQL(RootResolver(Query(i => i.c)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("invalid nullable validation") {
          final case class TestInput(@GQLDefault("1") s: Option[String])
          final case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(i => i.s.getOrElse("default"))))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("valid nullable validation") {
          final case class TestInput(@GQLDefault("\"1\"") s: Option[String])
          final case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(i => i.s.getOrElse("default"))))
          assertM(gql.interpreter)(anything)
        },
        testM("valid nullable validation for null") {
          final case class TestInput(@GQLDefault("null") s: Option[String])
          final case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(i => i.s.getOrElse("default"))))
          assertM(gql.interpreter)(anything)
        },
        testM("invalid list validation") {
          final case class TestInput(@GQLDefault("\"string\"") string: List[String])
          final case class Query(test: TestInput => List[String])
          val gql = graphQL(RootResolver(Query(i => i.string)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("valid list validation") {
          final case class TestInput(@GQLDefault("[\"string\"]") string: List[String])
          final case class Query(test: TestInput => List[String])
          val gql = graphQL(RootResolver(Query(i => i.string)))
          assertM(gql.interpreter)(anything)
        },
        testM("invalid object validation") {
          final case class Nested(field: String)
          final case class TestInput(@GQLDefault("{field: 2}") nested: Nested)
          final case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(v => v.nested.field)))
          assertM(gql.interpreter.run)(
            fails(isSubtype[CalibanError.ValidationError](anything))
          )
        },
        testM("valid object validation") {
          final case class Nested(field: String)
          final case class TestInput(@GQLDefault("{field: \"2\"}") nested: Nested)
          final case class Query(test: TestInput => String)
          val gql = graphQL(RootResolver(Query(v => v.nested.field)))
          assertM(gql.interpreter)(anything)
        }
      ),
      testM("field default values") {
        final case class TestInput(@GQLDefault("1") intValue: Int, stringValue: String)
        final case class Query(testDefault: TestInput => Int)
        val api         = graphQL(RootResolver(Query(i => i.intValue)))
        val interpreter = api.interpreter
        val query       =
          """query{
            |  testDefault(stringValue: "Hi!")
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"testDefault":1}"""))
      },
      testM("invalid field default values") {
        final case class TestInput(@GQLDefault("1.1") intValue: Int, stringValue: String)
        final case class Query(testDefault: TestInput => Int)
        val api      = graphQL(RootResolver(Query(i => i.intValue)))
        val expected =
          "InputValue 'intValue' of Field 'testDefault' of Object 'Query' has invalid type 1.1"

        assertM(api.interpreter.run)(fails(hasMessage(equalTo(expected))))
      },
      testM("explicit null for a nullable field with default value is valid") {
        val query =
          """query {
            |  query(string: null)
            |}""".stripMargin

        final case class TestInput(@GQLDefault("\"default\"") string: Option[String])
        final case class Query(query: TestInput => String)
        val gql = graphQL(RootResolver(Query(_.string.getOrElse("default"))))

        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assert(res.errors)(isEmpty)
      },
      testM("explicit null for a non-null field with default value is invalid") {
        val query =
          """query {
            |  query(string: null)
            |}""".stripMargin

        final case class TestInput(@GQLDefault("\"default\"") string: String)
        final case class Query(query: TestInput => String)
        val gql = graphQL(RootResolver(Query(_.string)))

        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assert(res.errors.headOption)(
          isSome((isSubtype[CalibanError.ValidationError](anything)))
        )
      },
      test("it should render default values in the SDL") {
        final case class TestInput(@GQLDefault("1") intValue: Int)
        final case class Query(testDefault: TestInput => Int)
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

        final case class TestInput(@GQLDefault("1") intValue: Int)
        final case class Query(testDefault: TestInput => Int)
        val interpreter = graphQL(RootResolver(Query(i => i.intValue))).interpreter

        assertM(interpreter.flatMap(_.execute(introspectionQuery)).map(_.data.toString))(
          equalTo(
            """{"__schema":{"queryType":{"name":"Query"},"mutationType":null,"subscriptionType":null,"types":[{"kind":"SCALAR","name":"Boolean","fields":null,"inputFields":null},{"kind":"SCALAR","name":"Int","fields":null,"inputFields":null},{"kind":"OBJECT","name":"Query","fields":[{"name":"testDefault","description":null,"args":[{"name":"intValue","description":null,"defaultValue":"1"}]}],"inputFields":null}]}}"""
          )
        )
      }
    )
}
