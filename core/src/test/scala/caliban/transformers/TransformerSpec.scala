package caliban.transformers

import caliban.Macros.gqldoc
import caliban._
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective
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
      suite("ExcludeArgument")(
        test("filter nullable argument") {
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
        test("cannot filter non-nullable arguments") {
          case class Args(arg1: String, arg2: Option[String], arg3: Option[String])
          case class Query(a: Args => String)
          val api: GraphQL[Any] = graphQL(
            RootResolver(
              Query(t => s"a1:${t.arg1} a2:${t.arg2.getOrElse("missing")} a3:${t.arg3.getOrElse("missing")}")
            )
          )

          val transformed: GraphQL[Any] = api.transform(
            Transformer.ExcludeArgument(
              "Query" -> "a" -> "arg1",
              "Query" -> "a" -> "arg2"
            )
          )
          val rendered                  = transformed.render
          for {
            _           <- Configurator.setSkipValidation(true)
            interpreter <- transformed.interpreter
            result      <- interpreter.execute("""{ a(arg1:"foo", arg2:"bar", arg3:"baz") }""").map(_.data.toString)
          } yield assertTrue(
            result == """{"a":"a1:foo a2:missing a3:baz"}""",
            rendered ==
              """schema {
                |  query: Query
                |}
                |
                |type Query {
                |  a(arg1: String!, arg3: String): String!
                |}""".stripMargin
          )
        }
      ),
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
      },
      suite("ExcludeDirectives") {
        case class SchemaA() extends GQLDirective(Directive("schemaA", isIntrospectable = false))
        case class SchemaB() extends GQLDirective(Directive("schemaB", isIntrospectable = false))

        test("fields") {
          case class Query(
            a: String,
            @SchemaA b: Int,
            @SchemaB c: Double,
            @SchemaA @SchemaB d: Boolean
          )
          val api: GraphQL[Any]  = graphQL(RootResolver(Query("a", 2, 3d, true)))
          val apiA: GraphQL[Any] = api.transform(Transformer.ExcludeDirectives(SchemaA()))
          val apiB: GraphQL[Any] = api.transform(Transformer.ExcludeDirectives(_.name == "schemaB"))
          val apiC: GraphQL[Any] = api.transform(Transformer.ExcludeDirectives(SchemaA(), SchemaB()))

          for {
            _        <- Configurator.setSkipValidation(true)
            res0     <- api.interpreterUnsafe.execute("""{ a b c d }""").map(_.data.toString)
            resA     <- apiA.interpreterUnsafe.execute("""{ a b c d }""").map(_.data.toString)
            resB     <- apiB.interpreterUnsafe.execute("""{ a b c d }""").map(_.data.toString)
            resC     <- apiC.interpreterUnsafe.execute("""{ a b c d }""").map(_.data.toString)
            rendered  = api.render
            renderedA = apiA.render
            renderedB = apiB.render
            renderedC = apiC.render
          } yield assertTrue(
            res0 == """{"a":"a","b":2,"c":3.0,"d":true}""",
            resA == """{"a":"a","b":null,"c":3.0,"d":null}""",
            resB == """{"a":"a","b":2,"c":null,"d":null}""",
            resC == """{"a":"a","b":null,"c":null,"d":null}""",
            rendered ==
              """schema {
                |  query: Query
                |}
                |
                |type Query {
                |  a: String!
                |  b: Int!
                |  c: Float!
                |  d: Boolean!
                |}""".stripMargin,
            renderedA ==
              """schema {
                |  query: Query
                |}
                |
                |type Query {
                |  a: String!
                |  c: Float!
                |}""".stripMargin,
            renderedB ==
              """schema {
                |  query: Query
                |}
                |
                |type Query {
                |  a: String!
                |  b: Int!
                |}""".stripMargin,
            renderedC ==
              """schema {
                |  query: Query
                |}
                |
                |type Query {
                |  a: String!
                |}""".stripMargin
          )
        } + test("input fields") {
          case class Nested(
            a: String,
            @SchemaA b: Option[Int],
            @SchemaB c: Option[Double],
            @SchemaA @SchemaB d: Option[Boolean]
          )
          case class Args(a: String, b: String, l: List[String], nested: Nested)
          case class Query(foo: Args => String)
          val api: GraphQL[Any]  = graphQL(RootResolver(Query(_ => "value")))
          val apiA: GraphQL[Any] = api.transform(Transformer.ExcludeDirectives(SchemaA()))
          val apiB: GraphQL[Any] = api.transform(Transformer.ExcludeDirectives(_.name == "schemaB"))
          val apiC: GraphQL[Any] = api.transform(Transformer.ExcludeDirectives(SchemaA(), SchemaB()))

          val rendered  = api.render
          val renderedA = apiA.render
          val renderedB = apiB.render
          val renderedC = apiC.render

          assertTrue(
            rendered ==
              """schema {
                |  query: Query
                |}
                |
                |input NestedInput {
                |  a: String!
                |  b: Int
                |  c: Float
                |  d: Boolean
                |}
                |
                |type Query {
                |  foo(a: String!, b: String!, l: [String!]!, nested: NestedInput!): String!
                |}""".stripMargin,
            renderedA ==
              """schema {
                |  query: Query
                |}
                |
                |input NestedInput {
                |  a: String!
                |  c: Float
                |}
                |
                |type Query {
                |  foo(a: String!, b: String!, l: [String!]!, nested: NestedInput!): String!
                |}""".stripMargin,
            renderedB ==
              """schema {
                |  query: Query
                |}
                |
                |input NestedInput {
                |  a: String!
                |  b: Int
                |}
                |
                |type Query {
                |  foo(a: String!, b: String!, l: [String!]!, nested: NestedInput!): String!
                |}""".stripMargin,
            renderedC ==
              """schema {
                |  query: Query
                |}
                |
                |input NestedInput {
                |  a: String!
                |}
                |
                |type Query {
                |  foo(a: String!, b: String!, l: [String!]!, nested: NestedInput!): String!
                |}""".stripMargin
          )
        }
      }
    )
}
