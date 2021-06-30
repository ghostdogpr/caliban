package caliban.derivation

import caliban.GraphQL.graphQL
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.Schema
import caliban.{ GraphQL, RootResolver }
import zio.ZIO
import zio.random.Random
import zio.test._
import zio.test.environment._

object UnionDerivationSpec extends DefaultRunnableSpec {
  sealed trait ExampleSum
  object ExampleSum {
    case class A(x: Option[Int]) extends ExampleSum {
      @GQLDeprecated("test")
      def y: ZIO[Any, Nothing, String] = ZIO.succeed("A")
    }

    @GQLDescription("the B")
    case class B() extends ExampleSum
  }

  implicit val exampleSumSchema: Schema[Any, ExampleSum] = deriveSchemaInstance[Any, ExampleSum]

  val exampleValue: ExampleSum = ExampleSum.A(Some(10))
  val api: GraphQL[Any]        = graphQL(RootResolver(exampleValue))

  val expectedSchema: String =
    """schema {
      |  query: ExampleSum
      |}
      |
      |union ExampleSum = A | B
      |
      |type A {
      |  x: Int
      |  y: String! @deprecated(reason: "test")
      |}
      |
      |"the B"
      |type B {
      |  
      |}""".stripMargin

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Caliban Derivation")(
      suite("Sum type without common fields")(
        test("schema rendered as expected") {
          val rendered = api.render

          assertTrue(rendered == expectedSchema)
        }
      )
    )
}
