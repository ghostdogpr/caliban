package caliban.tools.stitching

import caliban.CalibanError.ValidationError
import caliban.GraphQL.graphQL
import caliban.Macros.gqldoc
import caliban.{ CalibanError, GraphQLInterpreter, RootResolver }
import caliban.execution.Field
import caliban.schema.Annotations.GQLInterface
import caliban.schema.Schema.auto._
import zio._
import zio.test._

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
      } yield assertTrue(actual == """query { union(value: "foo\"") { ...on Interface { id } } }""")
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
        actual == """query { union(value: "bar") { ...on Interface { id } ...on A { id } ...on B { id } } }"""
      )
    }
  )
}
