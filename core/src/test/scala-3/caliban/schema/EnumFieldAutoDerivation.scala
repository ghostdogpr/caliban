package caliban.schema

import caliban.Macros.gqldoc
import caliban.{ graphQL, RootResolver }
import zio.{ Unsafe, ZIO }
import zio.test.*

object EnumFieldAutoDerivation extends ZIOSpecDefault {
  import caliban.schema.Schema.*

  override def spec = suite("EnumFieldAutoDerivation")(
    test("A schema & argbuilder is created automatically for enums") {
      val expected =
        """schema {
          |  query: Query
          |}
          |
          |enum Bar {
          |  BarA
          |  BarB
          |}
          |
          |enum Foo {
          |  FooA
          |  FooB
          |}
          |
          |type Query {
          |  foo(value: Bar): Value!
          |}
          |
          |type Value {
          |  value: Foo!
          |}""".stripMargin

      given Schema[Any, Foo]   = Schema.derived
      given Schema[Any, Bar]   = Schema.derived
      given Schema[Any, Value] = Schema.derived
      given ArgBuilder[Bar]    = ArgBuilder.derived

      given Schema[Any, Query] = Schema.Auto.derived

      val resolver = RootResolver(Query(_ => Value(Foo.FooA)))
      val gql      = graphQL(resolver)

      assertTrue(gql.render == expected)
    },
    test("A provided Schema take priority over auto-derivation") {
      given Schema[Any, Foo.FooB.type] = throw TestError
      given Schema[Any, Foo]           = Schema.derived
      given Schema[Any, Value]         = Schema.derived
      given Schema[Any, Bar]           = Schema.derived
      given ArgBuilder[Bar]            = ArgBuilder.derived
      given Schema[Any, Query]         = Schema.derived
      val resolver                     = RootResolver(Query(_ => Value(Foo.FooA)))

      try {
        graphQL(resolver).render
        assertNever("Should have errored")
      } catch {
        case TestError => assertCompletes
        case _         => assertNever("Invalid error")
      }
    },
    test("A provided ArgBuilder take priority over auto-derivation") {
      given Schema[Any, Foo]          = Schema.derived
      given Schema[Any, Bar]          = Schema.derived
      given Schema[Any, Value]        = Schema.derived
      given ArgBuilder[Bar.BarA.type] = throw TestError
      given ArgBuilder[Bar]           = ArgBuilder.derived
      given Schema[Any, Query]        = Schema.derived

      val resolver = RootResolver(Query(_ => Value(Foo.FooA)))
      val gql      = graphQL(resolver)

      val q = """query { foo(value: BarB) { value } }"""
      gql.interpreter.flatMap(_.execute(q)).catchAllDefect(ZIO.fail(_)).either.flatMap {
        case Right(_)                                 => assertNever("Should have errored")
        case Left(e) if e.getMessage.contains("boom") => assertCompletes
        case _                                        => assertNever("wrong error")
      }
    }
  )

  enum Foo {
    case FooA, FooB
  }

  enum Bar {
    case BarA, BarB
  }

  case class Value(value: Foo)
  case class Query(foo: Option[Bar] => Value)

  case object TestError extends Throwable("boom")
}
