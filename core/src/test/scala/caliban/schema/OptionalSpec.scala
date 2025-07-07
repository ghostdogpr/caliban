package caliban.schema

import caliban.{ graphQL, RootResolver }
import caliban.introspection.adt.__Type
import zio._
import zio.test._

object OptionalSpec extends ZIOSpecDefault {
  import caliban.schema.Schema._

  // Case classes moved to the top
  case class Wrapper[A](value: A)

  case class Query(
    a: Wrapper[String],
    b: Wrapper[Option[String]],
    c: Wrapper[Task[String]],
    d: Wrapper[Task[Option[String]]]
  )

  override def spec = suite("OptionalSpec")(
    test("Semantic of Schema.optional is kept consistent across various nullable/canFail combinations") {
      val expected =
        """schema {
          |  query: Query
          |}
          |
          |type Query {
          |  a: String!
          |  b: String
          |  c: String
          |  d: String
          |}""".stripMargin

      // Corrected schema implementation
      implicit def wrapperSchema[A](implicit ev: Schema[Any, A]): Schema[Any, Wrapper[A]] =
        new Schema[Any, Wrapper[A]] {
          override def nullable: Boolean                                = ev.nullable
          override def canFail: Boolean                                 = ev.canFail
          def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
          def resolve(value: Wrapper[A]): Step[Any]                     =
            ev.resolve(value.value)
        }

      implicit def querySchema: Schema[Any, Query] = Schema.gen[Any, Query]

      val resolver = RootResolver(
        Query(
          Wrapper("a"),
          Wrapper(Some("b")),
          Wrapper(ZIO.succeed("c")),
          Wrapper(ZIO.succeed(Some("d")))
        )
      )
      val gql      = graphQL(resolver)

      assertTrue(gql.render == expected)
    }
  )
}
