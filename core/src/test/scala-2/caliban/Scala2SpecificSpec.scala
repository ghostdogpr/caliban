package caliban

import caliban.GraphQL._
import caliban.TestUtils._
import caliban.introspection.adt.__DeprecatedArgs
import caliban.schema.SchemaSpec.introspect
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object Scala2SpecificSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Scala2SpecificSpec")(
      test("value classes should unwrap") {
        case class Queries(organizationId: OrganizationId, painter: WrappedPainter)
        val fieldTypes = introspect[Queries].fields(__DeprecatedArgs()).toList.flatten.map(_.`type`())
        assert(fieldTypes.map(_.ofType.flatMap(_.name)))(equalTo(Some("Long") :: Some("Painter") :: Nil))
      },
      testM("value classes") {
        case class Queries(events: List[Event], painters: List[WrappedPainter])
        val event       = Event(OrganizationId(7), "Frida Kahlo exhibition")
        val painter     = Painter("Claude Monet", "Impressionism")
        val api         = graphQL(RootResolver(Queries(event :: Nil, WrappedPainter(painter) :: Nil)))
        val interpreter = api.interpreter
        val query       =
          """query {
            |  events {
            |    organizationId
            |    title
            |  }
            |  painters {
            |    name
            |    movement
            |  }
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo(
            """{"events":[{"organizationId":7,"title":"Frida Kahlo exhibition"}],"painters":[{"name":"Claude Monet","movement":"Impressionism"}]}"""
          )
        )
      }
    )
}
