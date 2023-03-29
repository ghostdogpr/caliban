package caliban.schema

import caliban._
import caliban.RootResolver
import zio.test.{ assertTrue, ZIOSpecDefault }

object Scala3DerivesSpec extends ZIOSpecDefault {

  override def spec = suite("Scala3DerivesSpec") {

    val expected =
      """schema {
        |  query: Bar
        |}

        |type Bar {
        |  foo: Foo!
        |}

        |type Foo {
        |  value: String!
        |}""".stripMargin

    List(
      test("SemiAuto derivation - default") {
        final case class Foo(value: String) derives Schema.SemiAuto
        final case class Bar(foo: Foo) derives Schema.SemiAuto

        val gql = graphQL(RootResolver(Bar(Foo("foo"))))

        assertTrue(gql.render == expected)
      },
      test("Auto derivation - default") {
        final case class Foo(value: String)
        final case class Bar(foo: Foo) derives Schema.Auto

        val gql = graphQL(RootResolver(Bar(Foo("foo"))))

        assertTrue(gql.render == expected)
      },
      test("Auto derivation reuses implicit derivations in GenericSchema") {
        case class A(a: String)
        case class Queries(as: List[A]) derives Schema.Auto

        val resolver = RootResolver(Queries(List(A("a"), A("b"))))
        val gql      = graphQL(resolver)

        val expected =
          """schema {
            |  query: Queries
            |}

            |type A {
            |  a: String!
            |}

            |type Queries {
            |  as: [A!]!
            |}""".stripMargin

        assertTrue(gql.render == expected)
      },
      test("SemiAuto derivation - custom R") {
        class Env
        object CustomSchema extends SchemaDerivation[Env]
        final case class Foo(value: String) derives CustomSchema.SemiAuto
        final case class Bar(foo: Foo) derives CustomSchema.SemiAuto

        val gql = graphQL(RootResolver(Bar(Foo("foo"))))

        assertTrue(gql.render == expected)
      },
      test("Auto derivation - custom R") {
        class Env
        object CustomSchema extends SchemaDerivation[Env]
        final case class Foo(value: String)
        final case class Bar(foo: Foo) derives CustomSchema.Auto

        val gql = graphQL(RootResolver(Bar(Foo("foo"))))

        assertTrue(gql.render == expected)
      }
    )
  }
}
