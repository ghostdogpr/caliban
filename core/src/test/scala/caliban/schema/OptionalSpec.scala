package caliban.schema

import caliban.{ graphQL, RootResolver }
import zio._
import zio.test._

object OptionalSpec extends ZIOSpecDefault {
  import caliban.schema.Schema._

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

      implicit def wrapperSchema[A](implicit ev: Schema[Any, A]): Schema[Any, Wrapper[A]] =
        new Schema[Any, Wrapper[A]] {
          @annotation.nowarn
          override def optional = ev.optional
          def toType(isInput: Boolean, isSubscription: Boolean) = ev.toType_(isInput, isSubscription)
          def resolve(value: Wrapper[A]): Step[Any]             =
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

  case class Wrapper[A](value: A)

  case class Query(
    a: Wrapper[String],
    b: Wrapper[Option[String]],
    c: Wrapper[Task[String]],
    d: Wrapper[Task[Option[String]]]
  )
}
