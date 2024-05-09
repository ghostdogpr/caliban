package caliban.transformers

import caliban.Macros.gqldoc
import caliban._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import zio.test._

object TransformerSpec extends ZIOSpecDefault {

  case class Args(arg: String)
  case class InnerObject(b: Args => String)
  case class Query(a: InnerObject)

  val api: GraphQL[Any] = graphQL(RootResolver(Query(InnerObject(_.arg))))

  override def spec =
    suite("TransformerSpec")(
      test("rename type") {
        val transformed: GraphQL[Any] = api.transform(Transformer.RenameType("InnerObject" -> "Renamed"))
        val rendered                  = transformed.render
        for {
          interpreter <- transformed.interpreter
          result      <- interpreter.execute("""{ a { b(arg: "hello") } }""").map(_.data.toString)
        } yield assertTrue(
          result == """{"a":{"b":"hello"}}""",
          rendered == """schema {
                        |  query: Query
                        |}
                        |
                        |type Query {
                        |  a: Renamed!
                        |}
                        |
                        |type Renamed {
                        |  b(arg: String!): String!
                        |}""".stripMargin
        )
      },
      test("rename field") {
        val transformed: GraphQL[Any] = api.transform(Transformer.RenameField("InnerObject" -> "b" -> "c"))
        val rendered                  = transformed.render
        for {
          interpreter <- transformed.interpreter
          result      <- interpreter.execute("""{ a { c(arg: "hello") } }""").map(_.data.toString)
        } yield assertTrue(
          result == """{"a":{"c":"hello"}}""",
          rendered ==
            """schema {
              |  query: Query
              |}
              |
              |type InnerObject {
              |  c(arg: String!): String!
              |}
              |
              |type Query {
              |  a: InnerObject!
              |}""".stripMargin
        )
      },
      test("rename argument") {
        val transformed: GraphQL[Any] = api.transform(Transformer.RenameArgument {
          "InnerObject" -> "b" -> "arg" -> "arg2"
        })
        val rendered                  = transformed.render
        for {
          interpreter <- transformed.interpreter
          result      <- interpreter.execute("""{ a { b(arg2: "hello") } }""").map(_.data.toString)
        } yield assertTrue(
          result == """{"a":{"b":"hello"}}""",
          rendered ==
            """schema {
              |  query: Query
              |}
              |
              |type InnerObject {
              |  b(arg2: String!): String!
              |}
              |
              |type Query {
              |  a: InnerObject!
              |}""".stripMargin
        )
      },
      test("filter field") {
        case class Query(a: String, b: Int)
        val api: GraphQL[Any] = graphQL(RootResolver(Query("a", 2)))

        val transformed: GraphQL[Any] = api.transform(Transformer.ExcludeField("Query" -> "b"))
        val rendered                  = transformed.render
        for {
          interpreter <- transformed.interpreter
          result      <- interpreter.execute("""{ a }""").map(_.data.toString)
        } yield assertTrue(
          result == """{"a":"a"}""",
          rendered ==
            """schema {
              |  query: Query
              |}
              |
              |type Query {
              |  a: String!
              |}""".stripMargin
        )
      },
      test("exclude field from input object") {
        case class Nested(a: String, b: Option[String], c: String)
        case class Args(a: String, b: String, l: List[String], nested: Nested)
        case class Query(foo: Args => String)
        val api: GraphQL[Any] = graphQL(RootResolver(Query(_ => "value")))

        val transformed: GraphQL[Any] = api.transform(
          Transformer.ExcludeInputField(
            "NestedInput" -> "b",
            "NestedInput" -> "c" // non-nullable field can't be removed
          )
        )

        val rendered = transformed.render
        for {
          interpreter <- transformed.interpreter
          query        = gqldoc("""{ foo(a: "asd", b: "dsa", l:[], nested: {a:"ad", c:"da"}) }""")
          result      <- interpreter.execute(query).map(_.data.toString)
        } yield assertTrue(
          result == """{"foo":"value"}""",
          rendered ==
            """schema {
              |  query: Query
              |}
              |
              |input NestedInput {
              |  a: String!
              |  c: String!
              |}
              |
              |type Query {
              |  foo(a: String!, b: String!, l: [String!]!, nested: NestedInput!): String!
              |}""".stripMargin
        )
      },
      test("filter argument") {
        case class Args(arg: Option[String])
        case class Query(a: Args => String)
        val api: GraphQL[Any] = graphQL(RootResolver(Query(_.arg.getOrElse("missing"))))

        val transformed: GraphQL[Any] = api.transform(Transformer.ExcludeArgument("Query" -> "a" -> "arg"))
        val rendered                  = transformed.render
        for {
          interpreter <- transformed.interpreter
          result      <- interpreter.execute("""{ a }""").map(_.data.toString)
        } yield assertTrue(
          result == """{"a":"missing"}""",
          rendered ==
            """schema {
              |  query: Query
              |}
              |
              |type Query {
              |  a: String!
              |}""".stripMargin
        )
      },
      test("combine transformers") {
        val transformed: GraphQL[Any] = api
          .transform(Transformer.RenameType("InnerObject" -> "Renamed"))
          .transform(Transformer.RenameField("Renamed" -> "b" -> "c"))
        val rendered                  = transformed.render
        for {
          interpreter <- transformed.interpreter
          result      <- interpreter.execute("""{ a { c(arg: "hello") } }""").map(_.data.toString)
        } yield assertTrue(
          result == """{"a":{"c":"hello"}}""",
          rendered ==
            """schema {
              |  query: Query
              |}
              |
              |type Query {
              |  a: Renamed!
              |}
              |
              |type Renamed {
              |  c(arg: String!): String!
              |}""".stripMargin
        )
      }
    )
}
