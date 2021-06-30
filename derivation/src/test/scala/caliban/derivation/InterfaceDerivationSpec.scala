package caliban.derivation

import caliban.GraphQL.graphQL
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription, GQLExclude }
import caliban.schema.{ GenericSchema, Schema }
import caliban.{ GraphQL, RootResolver }
import zio._
import zio.query.ZQuery
import zio.random.Random
import zio.test._
import zio.test.environment._

object InterfaceDerivationSpec extends DefaultRunnableSpec {
  sealed trait ExampleSum {
    def x: Int
    @GQLDescription("the y field")
    val y: String
  }
  object ExampleSum       {
    case class A(z: Option[Int]) extends ExampleSum {
      override def x: Int    = z.getOrElse(1)
      override val y: String = "A"
    }

    @GQLDescription("the B")
    case class B(x: Int, y: String) extends ExampleSum
  }

  implicit val exampleSumSchema: Schema[Any, ExampleSum] = deriveSchemaInstance[Any, ExampleSum]

  val exampleValue: ExampleSum = ExampleSum.A(Some(10))
  val api: GraphQL[Any]        = graphQL(RootResolver(exampleValue))

  val expectedSchema: String =
    """schema {
      |  query: ExampleSum
      |}
      |
      |interface ExampleSum {
      |  x: Int!
      |  "the y field"
      |  y: String!
      |}
      |
      |type A implements ExampleSum {
      |  z: Int
      |  x: Int!
      |  y: String!
      |}
      |
      |"the B"
      |type B implements ExampleSum {
      |  x: Int!
      |  y: String!
      |}""".stripMargin

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Caliban Derivation")(
      suite("Sum type with common fields")(
        test("schema rendered as expected") {
          val rendered = api.render

          assertTrue(rendered == expectedSchema)
        }
      )
    )
}
