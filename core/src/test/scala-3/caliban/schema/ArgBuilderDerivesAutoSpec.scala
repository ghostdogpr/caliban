package caliban.schema

import caliban.*
import caliban.CalibanError.ExecutionError
import caliban.InputValue.{ ListValue, ObjectValue }
import caliban.Macros.gqldoc
import caliban.Value.{ IntValue, NullValue, StringValue }
import caliban.schema.Annotations.GQLOneOfInput
import caliban.schema.ArgBuilder.*
import zio.ZIO
import zio.test.*
import zio.test.Assertion.*

object ArgBuilderDerivesAutoSpec extends ZIOSpecDefault {
  def spec = suite("ArgBuilderDerivesAutoSpec")(
    suite("buildMissing")(
      test("works with derived case class ArgBuilders") {
        sealed abstract class Nullable[+T]
        case class SomeNullable[+T](t: T) extends Nullable[T]
        case object NullNullable          extends Nullable[Nothing]
        case object MissingNullable       extends Nullable[Nothing]

        given [A](using ev: ArgBuilder[A]): ArgBuilder[Nullable[A]] =
          new ArgBuilder[Nullable[A]] {
            def build(input: InputValue): Either[ExecutionError, Nullable[A]] = input match {
              case NullValue => Right(NullNullable)
              case _         => ev.build(input).map(SomeNullable(_))
            }

            override def buildMissing(default: Option[String]): Either[ExecutionError, Nullable[A]] =
              Right(MissingNullable)
          }

        case class Wrapper(a: Nullable[String]) derives ArgBuilder.Auto

        val derivedAB = summon[ArgBuilder[Wrapper]]

        assertTrue(
          derivedAB.build(ObjectValue(Map())) == Right(Wrapper(MissingNullable)),
          derivedAB.build(ObjectValue(Map("a" -> NullValue))) == Right(Wrapper(NullNullable)),
          derivedAB.build(ObjectValue(Map("a" -> StringValue("x")))) == Right(Wrapper(SomeNullable("x")))
        )
      }
    ),
    suite("reuses implicits defined in ArgBuilder") {
      case class InputArgs[A](value: A, list: List[A])

      val ints    = ObjectValue(Map("value" -> IntValue(1), "list" -> ListValue(List(IntValue(1), IntValue(2)))))
      val strings =
        ObjectValue(Map("value" -> StringValue("x"), "list" -> ListValue(List(StringValue("x"), StringValue("y")))))
      val obj     = ObjectValue(Map("ints" -> ints, "strings" -> strings))

      val intArgs = InputArgs(1, List(1, 2))
      val strArgs = InputArgs("x", List("x", "y"))

      List(
        test("ArgBuilder.Auto") {
          case class Wrapper(ints: InputArgs[Int], strings: InputArgs[String]) derives ArgBuilder.Auto
          val derived = summon[ArgBuilder[Wrapper]]
          assertTrue(derived.build(obj) == Right(Wrapper(intArgs, strArgs)))
        },
        test("ArgBuilder.GenAuto") {
          case class Wrapper(ints: InputArgs[Int], strings: InputArgs[String]) derives ArgBuilder.GenAuto
          val derived = summon[ArgBuilder[Wrapper]]
          assertTrue(derived.build(obj) == Right(Wrapper(intArgs, strArgs)))
        }
      )

    },
    suite("enums as oneOf inputs") {
      @GQLOneOfInput
      enum Foo derives Schema.SemiAuto, ArgBuilder {
        case FooString(stringValue: String) extends Foo
        case FooInt(intValue: Int)          extends Foo
      }
      case class Wrapper(fooInput: Foo) derives Schema.SemiAuto, ArgBuilder
      case class Queries(foo: Wrapper => String, fooUnwrapped: Foo => String) derives Schema.SemiAuto

      List(
        test("schema is derived correctly") {
          val expected =
            """schema {
              |  query: Queries
              |}
              |
              |input FooInput @oneOf {
              |  stringValue: String
              |  intValue: Int
              |}
              |
              |type Queries {
              |  foo(fooInput: FooInput!): String!
              |  fooUnwrapped(value: FooInput!): String!
              |}""".stripMargin

          val api: GraphQL[Any] = graphQL(RootResolver(Queries(_ => "", _ => "")))

          println(api.render)
          assertTrue(api.render == expected)
        },
        test("successfully validates and executes") {

          val api: GraphQL[Any] = graphQL(
            RootResolver(
              Queries(
                {
                  case Wrapper(Foo.FooString(value)) => value
                  case Wrapper(Foo.FooInt(value))    => value.toString
                },
                {
                  case Foo.FooString(value) => value
                  case Foo.FooInt(value)    => value.toString
                }
              )
            )
          )

          val cases = List(
            gqldoc("""{ foo(fooInput: {stringValue: "hello"}) }""")                 -> """{"foo":"hello"}""",
            gqldoc("""{ foo(fooInput: {intValue: 42}) }""")                         -> """{"foo":"42"}""",
            gqldoc("""{ fooUnwrapped(value: {intValue: 42}) }""")                   -> """{"fooUnwrapped":"42"}""",
            gqldoc("""query Foo($args: FooInput!){ fooUnwrapped(value: $args) }""") -> """{"fooUnwrapped":"42"}"""
          )

          ZIO.foldLeft(cases)(assertCompletes) { case (acc, (query, expected)) =>
            api.interpreter
              .flatMap(_.execute(query, variables = Map("args" -> ObjectValue(Map("intValue" -> IntValue(42))))))
              .map(response => assertTrue(response.data.toString == expected))
          }
        }
      )
    }
  )
}
