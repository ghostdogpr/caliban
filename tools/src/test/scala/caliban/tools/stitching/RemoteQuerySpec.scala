package caliban.tools.stitching

import caliban.CalibanError.ValidationError
import caliban._
import caliban.Macros.gqldoc
import caliban.execution.Field
import caliban.schema.Annotations.GQLInterface
import caliban.schema.Schema.auto._
import zio._
import zio.test._
import zio.json._
import zio.json.ast.Json

object RemoteQuerySpec extends ZIOSpecDefault {
  sealed trait Union
  @GQLInterface
  sealed trait Interface

  case class A(id: String) extends Union with Interface

  case class B(id: String) extends Union with Interface

  case class C(id: String) extends Interface

  case class Queries(
    union: String => Field => UIO[Union],
    interface: Interface
  )

  def api(ref: Ref[String]): IO[ValidationError, GraphQLInterpreter[Any, CalibanError]] = {
    val api = graphQL(
      RootResolver(
        Queries(
          _ =>
            field =>
              ref
                .set(RemoteQuery.QueryRenderer.render(RemoteQuery(field)))
                .as(A("id")),
          C("id-c")
        )
      )
    )

    api.interpreter
  }

  override def spec = suite("RemoteQuerySpec")(
    test("correctly renders a query for a field") {
      val query = gqldoc("""{
              union(value: "foo\"") { ...on Interface { id }  }
            }""")

      for {
        ref    <- Ref.make[String]("")
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(
        actual == """query{union(value:"foo\""){...on Interface{id}}}"""
      )
    },
    test("correctly renders a query for a field") {
      val query = gqldoc("""{
              union(value: "bar") { ...on Interface { id } ...on A { id } ...on B { id }  }
            }""")

      for {
        ref    <- Ref.make[String]("")
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(
        actual == """query{union(value:"bar"){...on Interface{id} ...on A{id} ...on B{id}}}"""
      )
    },
    test("correctly renders a query for a field of json escaped characters") {
      val query = gqldoc("""{
               union(value: "{\"foo\": \" \\n \\\" \\r \\r\\n \\t \\\\ \\b \\f \"}") { ...on Interface { id } }
             }""")

      for {
        ref    <- Ref.make[String]("")
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get

        // What we expect from the query
        expectedValue = """"{\"foo\": \" \\n \\\" \\r \\r\\n \\t \\\\ \\b \\f \"}""""
        isValidJson   = expectedValue.fromJson[Json]
      } yield assertTrue(
        actual == s"""query{union(value:${expectedValue}){...on Interface{id}}}""",
        isValidJson.isRight
      )
    },
    test("correctly renders a query for a field of json escaped quote") {
      val query = gqldoc("""{
               union(value: "{\"foo\": \"\"}") { ...on Interface { id } }
             }""")

      for {
        ref    <- Ref.make[String]("")
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(
        actual == """query{union(value:"{\"foo\": \"\"}"){...on Interface{id}}}"""
      )
    },
    test("correctly renders a query for a field of json escaped newline") {
      val query = gqldoc("""{
               union(value: "{\"foo\": \"\\n\"}") { ...on Interface { id } }
             }""")

      for {
        ref    <- Ref.make[String]("")
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(
        actual == """query{union(value:"{\"foo\": \"\\n\"}"){...on Interface{id}}}"""
      )
    },
    test("correctly renders a query for a field of json escaped escaped quote") {
      val query = gqldoc("""{
               union(value: "{\"foo\": \"\\\"\"}") { ...on Interface { id } }
             }""")

      for {
        ref    <- Ref.make[String]("")
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(
        actual == """query{union(value:"{\"foo\": \"\\\"\"}"){...on Interface{id}}}"""
      )
    },
    test("correctly renders a query for a field of json escaped carriage return") {
      val query = gqldoc("""{
               union(value: "{\"foo\": \"\\r\"}") { ...on Interface { id } }
             }""")

      for {
        ref    <- Ref.make[String]("")
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(
        actual == """query{union(value:"{\"foo\": \"\\r\"}"){...on Interface{id}}}"""
      )
    },
    test("correctly renders a query for a field of json escaped tab") {
      val query = gqldoc("""{
               union(value: "{\"foo\": \"\\t\"}") { ...on Interface { id } }
             }""")

      for {
        ref    <- Ref.make[String]("")
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(
        actual == """query{union(value:"{\"foo\": \"\\t\"}"){...on Interface{id}}}"""
      )
    }
  )
}
