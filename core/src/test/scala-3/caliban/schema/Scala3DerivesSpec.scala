package caliban.schema

import caliban.*
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.{ GQLDescription, GQLDirective, GQLField, GQLInterface, GQLName }
import zio.test.{ assertTrue, ZIOSpecDefault }
import zio.{ RIO, Task, ZIO }

import java.time.Instant

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
      test("Auto derivation - custom R") {
        class Env
        object CustomSchema extends SchemaDerivation[Env]
        final case class Foo(value: String)
        final case class Bar(foo: Foo) derives CustomSchema.Auto

        val gql = graphQL(RootResolver(Bar(Foo("foo"))))

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
      suite("ArgBuilder derivation") {
        val expected =
          """schema {
            |  query: Query
            |}

            |type Bar {
            |  foo: Foo!
            |}

            |type Foo {
            |  s: String!
            |}

            |type Query {
            |  f(s: String!): Bar!
            |}""".stripMargin

        List(
          test("SemiAuto") {
            final case class Foo(s: String) derives Schema.SemiAuto, ArgBuilder
            final case class Bar(foo: Foo) derives Schema.SemiAuto
            final case class Query(f: Foo => Bar) derives Schema.SemiAuto

            val gql = graphQL(RootResolver(Query(Bar(_))))

            assertTrue(gql.render == expected)
          },
          test("Auto") {
            final case class Foo(s: String) derives Schema.Auto, ArgBuilder.GenAuto
            final case class Bar(foo: Foo) derives Schema.SemiAuto
            final case class Query(f: Foo => Bar) derives Schema.SemiAuto

            val gql = graphQL(RootResolver(Query(Bar(_))))

            assertTrue(gql.render == expected)
          }
        )
      },
      suite("derivation of case classes containing Instants")(
        test("product schema") {
          final case class Foo(i: Instant) derives Schema.SemiAuto, ArgBuilder
          final case class Bar(foo: Foo) derives Schema.SemiAuto
          final case class Query(f: Foo => Bar) derives Schema.SemiAuto

          val gql = graphQL(RootResolver(Query(Bar(_))))
          assertTrue(
            gql.render ==
              """schema {
                |  query: Query
                |}
                |"An instantaneous point on the time-line represented by a standard date time string"
                |scalar Instant

                |type Bar {
                |  foo: Foo!
                |}

                |type Foo {
                |  i: Instant!
                |}

                |type Query {
                |  f(i: Instant!): Bar!
                |}""".stripMargin
          )
        },
        test("sum schema") {
          @GQLInterface
          sealed trait Foo derives Schema.SemiAuto {
            val i: Instant
          }
          object Foo                               {
            final case class FooA(i: Instant, s1: String) extends Foo derives Schema.SemiAuto, ArgBuilder
            final case class FooB(i: Instant, i1: Int)    extends Foo derives Schema.SemiAuto, ArgBuilder
          }

          final case class Bar(foo: Foo) derives Schema.SemiAuto
          final case class Query(f: Foo.FooA => Bar) derives Schema.SemiAuto

          val gql = graphQL(RootResolver(Query(Bar(_))))

          assertTrue(
            gql.render ==
              """schema {
                |  query: Query
                |}
                |"An instantaneous point on the time-line represented by a standard date time string"
                |scalar Instant

                |interface Foo {
                |  i: Instant!
                |}

                |type Bar {
                |  foo: Foo!
                |}

                |type FooA implements Foo {
                |  i: Instant!
                |  s1: String!
                |}

                |type FooB implements Foo {
                |  i: Instant!
                |  i1: Int!
                |}

                |type Query {
                |  f(i: Instant!, s1: String!): Bar!
                |}""".stripMargin
          )
        }
      ),
      suite("methods as fields") {
        val expectedSchema =
          """schema {
            |  query: Bar
            |}

            |type Bar {
            |  foo: Foo!
            |}

            |type Foo {
            |  value: String!
            |  value2: String
            |}""".stripMargin
        List(
          test("SemiAuto derivation of methods as fields") {
            final case class Foo(value: String) derives Schema.SemiAuto {
              def value1: String                   = value + 1
              @GQLField def value2: Option[String] = Some(value + 2)
            }
            final case class Bar(foo: Foo) derives Schema.SemiAuto
            val rendered = graphQL(RootResolver(Bar(Foo("foo")))).render

            assertTrue(rendered == expectedSchema)
          },
          test("custom schema derivation") {
            trait MyService
            object MySchema extends SchemaDerivation[MyService]
            final case class Foo(value: String) derives MySchema.SemiAuto {
              @GQLField def value2: RIO[MyService, Option[String]] = ZIO.some(value + 2)
            }
            final case class Bar(foo: Foo) derives MySchema.SemiAuto
            val rendered = graphQL(RootResolver(Bar(Foo("foo")))).render

            assertTrue(rendered == expectedSchema)
          },
          test("method annotations") {
            final case class Foo(value: String) derives Schema.SemiAuto {
              @GQLField
              @GQLName("value2")
              def foo: Option[String] = Some(value + 2)
            }
            final case class Bar(foo: Foo) derives Schema.SemiAuto
            val rendered = graphQL(RootResolver(Bar(Foo("foo")))).render

            assertTrue(rendered == expectedSchema)
          },
          test("Auto derivation of methods as fields") {
            final case class Foo(value: String) {
              @GQLField def value2: Option[String] = Some(value + 2)
            }
            final case class Bar(foo: Foo) derives Schema.Auto
            val rendered = graphQL(RootResolver(Bar(Foo("foo")))).render
            assertTrue(rendered == expectedSchema)
          },
          test("execution of methods as fields") {
            final case class Foo(value: String) derives Schema.SemiAuto {
              @GQLField def value2: Task[String] = ZIO.succeed(value + 2)
            }
            final case class Bar(foo: Foo) derives Schema.SemiAuto
            val gql = graphQL(RootResolver(Bar(Foo("foo"))))

            gql.interpreter.flatMap { i =>
              i.execute("{foo {value value2}}").map { v =>
                val s = v.data.toString
                assertTrue(s == """{"foo":{"value":"foo","value2":"foo2"}}""")
              }
            }
          }
        )
      },
      test("enum derivation with description") {
        sealed trait MyEnum derives Schema.SemiAuto, ArgBuilder

        object MyEnum {
          @GQLDescription("comment for ENUM1")
          case object ENUM1 extends MyEnum derives Schema.SemiAuto, ArgBuilder

          @GQLDescription("comment for ENUM2")
          case object ENUM2 extends MyEnum derives Schema.SemiAuto, ArgBuilder
        }

        final case class QueryEnum2StringArgs(select: MyEnum) derives Schema.SemiAuto, ArgBuilder
        final case class Query(enum2String: QueryEnum2StringArgs => zio.UIO[String]) derives Schema.SemiAuto

        val interpreter = graphQL(
          RootResolver(Query(enum2String = args => ZIO.succeed(args.select.toString)))
        ).interpreterUnsafe

        for {
          res1 <- interpreter.execute("{ enum2String(select: ENUM1) }")
          res2 <- interpreter.execute("{ enum2String(select: ENUM2) }")
          data1 = res1.data.toString
          data2 = res2.data.toString
        } yield assertTrue(
          data1 == """{"enum2String":"ENUM1"}""",
          data2 == """{"enum2String":"ENUM2"}"""
        )
      },
      test("union type") {
        final case class Foo(value: String) derives Schema.SemiAuto
        final case class Bar(foo: Int) derives Schema.SemiAuto
        final case class Baz(bar: Int) derives Schema.SemiAuto
        @GQLName("Payload2")
        @GQLDescription("Union type Payload")
        @GQLDirective(Directive.apply("mydirective", Map("arg" -> Value.StringValue("value"))))
        type Payload = Foo | Bar | Baz
        given Schema[Any, Payload] = Schema.unionType[Payload]

        final case class QueryInput(isFoo: Boolean) derives ArgBuilder, Schema.SemiAuto
        final case class Query(testQuery: QueryInput => zio.UIO[Payload]) derives Schema.SemiAuto

        val gql = graphQL(RootResolver(Query(i => ZIO.succeed(if (i.isFoo) Foo("foo") else Bar(1)))))

        val expectedSchema =
          """schema {
  query: Query
}

"Union type Payload"
union Payload2 @mydirective(arg: "value") = Foo | Bar | Baz

type Bar {
  foo: Int!
}

type Baz {
  bar: Int!
}

type Foo {
  value: String!
}

type Query {
  testQuery(isFoo: Boolean!): Payload2!
}""".stripMargin
        val interpreter    = gql.interpreterUnsafe

        for {
          res1 <- interpreter.execute("{ testQuery(isFoo: true){ ... on Foo { value } } }")
          res2 <- interpreter.execute("{ testQuery(isFoo: false){ ... on Bar { foo } } }")
          data1 = res1.data.toString
          data2 = res2.data.toString
        } yield assertTrue(
          data1 == """{"testQuery":{"value":"foo"}}""",
          data2 == """{"testQuery":{"foo":1}}""",
          gql.render == expectedSchema
        )
      }
    )
  }
}
