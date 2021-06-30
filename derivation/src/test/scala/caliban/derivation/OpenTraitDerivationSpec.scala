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

object OpenTraitDerivationSpec extends DefaultRunnableSpec {

  trait ExampleInterface {
    def name: String

    @GQLDescription("A list of the character's nicknames")
    def nicknames: List[String]

    @GQLDescription("Foo")
    def foo(): Int

    @GQLDeprecated("Don't use Bar.")
    def bar: Int = 42

    def query: ZQuery[Any, Nothing, String]
    def op(pattern: String, limit: Int): List[String]
    @GQLDescription("Randomly picks one of the nicknames")
    def randomNickname: ZIO[Random, Nothing, Option[String]]

    // This field will not be exposed via GraphQL
    @GQLExclude
    def internal(): Unit = xyz

    // Protected methods are not exposed
    protected def internal2(): Unit = xyz

    // Private fields are also not exposed via GraphQL
    private val xyz: Unit = ()
  }

  object ExampleInterface extends GenericSchema[Random] {
    implicit lazy val exampleInterfaceSchema: Schema[Random, ExampleInterface] =
      deriveSchemaInstance[Random, ExampleInterface]

    // An implementation of ExampleApi
    case class ExampleProduct(
      name: String,
      nicknames: List[String]
    ) extends ExampleInterface {
      override def foo(): Int = 5

      override def query: ZQuery[Any, Nothing, String] =
        ZQuery.succeed(nicknames.mkString(", "))

      override def op(pattern: String, limit: Int): List[String] =
        nicknames.filter(_.matches(pattern)).take(limit)

      override def randomNickname: ZIO[Random, Nothing, Option[String]] =
        if (nicknames.nonEmpty) {
          random.nextIntBounded(nicknames.size).map { idx =>
            Some(nicknames(idx))
          }
        } else ZIO.none
    }

    val exampleValue: ExampleInterface = ExampleProduct(
      "hello",
      List("a", "b")
    )
    lazy val api: GraphQL[Random]      = graphQL(RootResolver(exampleValue))

    val expectedSchema: String =
      """schema {
        |  query: ExampleInterface
        |}
        |
        |type ExampleInterface {
        |  name: String!
        |  "A list of the character's nicknames"
        |  nicknames: [String!]!
        |  "Foo"
        |  foo: Int!
        |  bar: Int! @deprecated(reason: "Don't use Bar.")
        |  query: String!
        |  op(pattern: String!, limit: Int!): [String!]!
        |  "Randomly picks one of the nicknames"
        |  randomNickname: String
        |}""".stripMargin
  }

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Caliban Derivation")(
      suite("Open trait")(
        test("schema rendered as expected") {
          val rendered = ExampleInterface.api.render

          assertTrue(rendered == ExampleInterface.expectedSchema)
        }
      )
    ) @@ TestAspect.exceptDotty
}
