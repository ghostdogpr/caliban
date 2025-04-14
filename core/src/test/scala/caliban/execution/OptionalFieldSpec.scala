package caliban.execution

import caliban.schema.Annotations.{ GQLDefault, GQLOptional }
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import caliban.schema.{ ArgBuilder, Schema }
import caliban.{ graphQL, CalibanError, RootResolver }
import zio.test.Assertion.{ anything, fails, isSome, isSubtype }
import zio.test._

object OptionalFieldSpec extends ZIOSpecDefault {
  def missingBuilder[A: ArgBuilder]: ArgBuilder[Either[Unit, A]] = ArgBuilder.missingInput(Left(()), Right(_))

  implicit val stringBuilder: ArgBuilder[Either[Unit, String]]               = missingBuilder[String]
  implicit val optionStringBuilder: ArgBuilder[Either[Unit, Option[String]]] = missingBuilder[Option[String]]

  type S[A] = Schema[Any, A]
  def missingSchema[A: S]: Schema[Any, Either[Unit, A]]                      = Schema.optionalInputSchema(f => (m, p) => f.fold(_ => m, p))
  implicit val stringSchema: Schema[Any, Either[Unit, String]]               = missingSchema[String]
  implicit val optionStringSchema: Schema[Any, Either[Unit, Option[String]]] = missingSchema[Option[String]]

  override def spec =
    suite("OptionalFieldSpec")(
      test("invalid undefined validation") {
        case class TestInput(@GQLOptional c: Boolean)
        case class Query()
        case class Mutations(test: TestInput => Boolean)
        val gql = graphQL(RootResolver(Query(), Mutations(i => i.c)))
        gql.interpreter.exit.map(e => assert(e)(fails(isSubtype[CalibanError.ValidationError](anything))))
      },
      test("valid optional input field validation") {
        case class TestInput(@GQLOptional c: Either[Unit, String])
        case class TestQuery(c: String)
        case class Query(test: TestQuery => String)
        case class Mutations(test: TestInput => Boolean)
        val gql = graphQL(RootResolver(Query(_.c), Mutations(i => i.c.fold(_ => false, _ => true))))
        gql.interpreter.map(i => assert(i)(anything))
      },
      test("not passing optional input field for mutation is valid") {
        case class TestInput(@GQLOptional string: Either[Unit, String], mandatory: Boolean)
        case class Mutation(test: TestInput => String)
        case class Query(test: TestInput => String)

        val qgl   = graphQL(RootResolver(Query(_ => "foo"), Mutation(_.string.fold(_ => "bar", identity))))
        val query =
          """mutation {
            |  test(mandatory: true)
            |}""".stripMargin
        for {
          int <- qgl.interpreter
          res <- int.execute(query)
        } yield assertTrue(res.errors.isEmpty && res.data.toString == """{"test":"bar"}""")
      },
      test("passing optional input field for mutation is valid") {
        case class TestInput(@GQLOptional string: Either[Unit, String], mandatory: Boolean)
        case class Mutation(test: TestInput => String)
        case class Query(test: TestInput => String)

        val qgl   = graphQL(RootResolver(Query(_ => "foo"), Mutation(_.string.fold(_ => "bar", identity))))
        val query =
          """mutation {
            |  test(mandatory: true, string: "bazz")
            |}""".stripMargin
        for {
          int <- qgl.interpreter
          res <- int.execute(query)
        } yield assertTrue(res.errors.isEmpty && res.data.toString == """{"test":"bazz"}""")
      },
      test("passing explicit 'null' to optional non nullable input field for mutation is invalid") {
        case class TestInput(@GQLOptional string: Either[Unit, String], mandatory: Boolean)
        case class Mutation(test: TestInput => String)
        case class Query(test: TestInput => String)

        val qgl   = graphQL(RootResolver(Query(_ => "foo"), Mutation(_.string.fold(_ => "bar", identity))))
        val query =
          """mutation {
            |  test(mandatory: true, string: null)
            |}""".stripMargin

        for {
          int <- qgl.interpreter
          res <- int.execute(query)
        } yield assert(res.errors.headOption)(
          isSome(isSubtype[CalibanError.ValidationError](anything))
        )
      },
      test("passing 'null' in optional nullable input field for mutation is valid") {
        case class TestInput(@GQLOptional string: Either[Unit, Option[String]], mandatory: Boolean)
        case class Mutation(test: TestInput => String)
        case class Query(test: TestInput => String)

        val qgl   = graphQL(RootResolver(Query(_ => "foo"), Mutation(_.string.fold(_ => "bar", _.fold("baz")(identity)))))
        val query =
          """mutation {
            |  test(mandatory: true, string: null)
            |}""".stripMargin
        for {
          int <- qgl.interpreter
          res <- int.execute(query)
        } yield assertTrue(res.errors.isEmpty && res.data.toString == """{"test":"baz"}""")
      },
      test("passing value in optional nullable input field for mutation is valid") {
        case class TestInput(@GQLOptional string: Either[Unit, Option[String]], mandatory: Boolean)
        case class Mutation(test: TestInput => String)
        case class Query(test: TestInput => String)

        val qgl   = graphQL(RootResolver(Query(_ => "foo"), Mutation(_.string.fold(_ => "bar", _.fold("baz")(identity)))))
        val query =
          """mutation {
            |  test(mandatory: true, string: "foobar")
            |}""".stripMargin
        for {
          int <- qgl.interpreter
          res <- int.execute(query)
        } yield assertTrue(res.errors.isEmpty && res.data.toString == """{"test":"foobar"}""")
      },
      test("it should render optional input fields in the SDL") {
        case class MutationInput(@GQLOptional string: Either[Unit, String], mandatory: Boolean)
        case class Mutation(test: MutationInput => String)
        case class QueryInput(intValue: Int)
        case class Query(test: QueryInput => Int)
        val rendered =
          graphQL(RootResolver(Query(_.intValue), Mutation(_.string.fold(_ => "bar", identity)))).render.trim

        assertTrue(
          rendered ==
            """|schema {
               |  query: Query
               |  mutation: Mutation
               |}
               |
               |type Mutation {
               |  test(string: String!, mandatory: Boolean!): String!
               |}
               |
               |type Query {
               |  test(intValue: Int!): Int!
               |}""".stripMargin.trim
        )
      }
    )
}
