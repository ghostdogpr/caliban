package caliban.transformers

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
        val transformed: GraphQL[Any] = api.transform(Transformer.RenameType { case "InnerObject" => "Renamed" })
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
        val transformed: GraphQL[Any] = api.transform(Transformer.RenameField(("InnerObject", "b") -> "c"))
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
        val transformed: GraphQL[Any] = api.transform(Transformer.RenameArgument { case ("InnerObject", "b") =>
          ({ case "arg" => "arg2" }, { case "arg2" => "arg" })
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

        val transformed: GraphQL[Any] = api.transform(Transformer.FilterField { case ("Query", "b") => false })
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
      test("filter argument") {
        case class Args(arg: Option[String])
        case class Query(a: Args => String)
        val api: GraphQL[Any] = graphQL(RootResolver(Query(_.arg.getOrElse("missing"))))

        val transformed: GraphQL[Any] =
          api.transform(Transformer.FilterArgument { case ("Query", "a", "arg") => false })
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
          .transform(Transformer.RenameType { case "InnerObject" => "Renamed" })
          .transform(Transformer.RenameField(("Renamed", "b") -> "c"))
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
