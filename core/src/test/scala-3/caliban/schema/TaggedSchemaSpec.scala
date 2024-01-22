package caliban.schema

import caliban.*
import caliban.schema.Annotations.GQLTag
import zio.*
import zio.test.*
import zio.test.Assertion.*

object TaggedSchemaSpec extends ZIOSpecDefault {
  override def spec =
    suite("TaggedSchemaSpec")(
      test("Schema specific fields") {
        val expected1 =
          """schema {
            |  query: Queries
            |}
            |
            |type Foo {
            |  valueA: String!
            |  valueC: String!
            |}
            |
            |type Queries {
            |  foo: Foo!
            |}""".stripMargin

        val expected2 =
          """schema {
            |  query: Queries
            |}
            |
            |type Foo {
            |  valueB: String!
            |  valueC: String!
            |}
            |
            |type Queries {
            |  foo: Foo!
            |}""".stripMargin

        trait Env1
        trait SchemaA  extends SchemaDerivation.Tagged[Env1, SchemaA]
        object SchemaA extends SchemaA

        trait Env2
        trait SchemaB  extends SchemaDerivation.Tagged[Env2, SchemaB]
        object SchemaB extends SchemaB

        case class Foo(
          @GQLTag[SchemaA]
          valueA: URIO[Env1, String],
          @GQLTag[SchemaB]
          valueB: URIO[Env2, String],
          valueC: String
        ) derives SchemaA.SemiAuto,
              SchemaB.SemiAutoAlt

        case class Queries(foo: Foo) derives SchemaA.SemiAuto, SchemaB.SemiAutoAlt

        val resolver            = RootResolver(Queries(Foo(ZIO.succeed("a"), ZIO.succeed("b"), "c")))
        val api1: GraphQL[Env1] = resolver.toGraphQL.forSchema[Env1, SchemaA]
        val api2: GraphQL[Env2] = resolver.toGraphQL.forSchema[Env2, SchemaB]

        val rendered1 = api1.render
        val rendered2 = api2.render

        assertTrue(rendered1 == expected1, rendered2 == expected2)
      }
    )
}
