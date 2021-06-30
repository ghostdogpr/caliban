package caliban.derivation

import caliban.GraphQL.graphQL
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.Schema
import caliban.{ GraphQL, RootResolver }
import zio.ZIO
import zio.random.Random
import zio.test._
import zio.test.environment._

object EnumDerivationSpec extends DefaultRunnableSpec {
  sealed trait ExampleSum

  object ExampleSum {
    case object A extends ExampleSum
    @GQLDescription("the B")
    case object B extends ExampleSum
  }

  implicit lazy val exampleSumSchema: Schema[Any, ExampleSum] = deriveSchemaInstance[Any, ExampleSum]

  lazy val exampleValue: ExampleSum = ExampleSum.A
  lazy val api: GraphQL[Any]        = graphQL(RootResolver(exampleValue))

  val expectedSchema: String =
    """schema {
      |  query: ExampleSum
      |}
      |
      |enum ExampleSum {
      |  A
      |  "the B"
      |B
      |}""".stripMargin

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Caliban Derivation")(
      suite("Sum type without common fields")(
        test("schema rendered as expected") {
          val rendered = api.render

          assertTrue(rendered == expectedSchema)
        }
      )
    ) @@ TestAspect.exceptDotty
}
