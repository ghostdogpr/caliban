package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.InputValue.ObjectValue
import caliban.schema.ArgBuilder.auto._
import caliban.Value.{ IntValue, NullValue, StringValue }
import caliban.schema.Annotations.{ GQLOneOfInput, GQLValueType }
import zio.test.Assertion._
import zio.test._

import java.time._

object ArgBuilderSpec extends ZIOSpecDefault {
  def spec = suite("ArgBuilder")(
    suite("orElse")(
      test("handles failures")(
        assert((ArgBuilder.instant orElse ArgBuilder.instantEpoch).build(IntValue.LongNumber(100)))(
          isRight(equalTo(Instant.ofEpochMilli(100)))
        )
      ),
      test("short-circuits")(
        assert((ArgBuilder.instantEpoch orElse ArgBuilder.instant).build(IntValue.LongNumber(100)))(
          isRight(equalTo(Instant.ofEpochMilli(100)))
        )
      )
    ),
    suite("long")(
      test("Long from string")(
        check(Gen.long) { value =>
          assert(ArgBuilder.long.build(StringValue(s"$value")))(
            isRight(equalTo(value))
          )
        }
      )
    ),
    suite("java.time")(
      test("Instant from epoch")(
        assert(ArgBuilder.instantEpoch.build(IntValue.LongNumber(100)))(
          isRight(equalTo(Instant.ofEpochMilli(100)))
        )
      ),
      test("Instant from string")(
        assert(ArgBuilder.instant.build(StringValue("1970-01-01T00:00:00.100Z")))(
          isRight(equalTo(Instant.ofEpochMilli(100)))
        )
      ),
      test("LocalDate from string")(
        assert(ArgBuilder.localDate.build(StringValue("1970-01-01")))(
          isRight(equalTo(LocalDate.of(1970, 1, 1)))
        )
      ),
      test("LocalDateTime from string")(
        assert(ArgBuilder.localDateTime.build(StringValue("1970-01-01T01:30")))(
          isRight(equalTo(LocalDateTime.of(1970, 1, 1, 1, 30)))
        )
      ),
      test("OffsetTime from string")(
        assert(ArgBuilder.offsetTime.build(StringValue("01:02:03.000Z")))(
          isRight(equalTo(OffsetTime.of(1, 2, 3, 0, ZoneOffset.UTC)))
        )
      ),
      test("OffsetDateTime from string")(
        assert(ArgBuilder.offsetDateTime.build(StringValue("1970-01-01T01:01:01Z")))(
          isRight(equalTo(OffsetDateTime.of(1970, 1, 1, 1, 1, 1, 0, ZoneOffset.UTC)))
        )
      ),
      test("ZonedDateTime from string")(
        assert(ArgBuilder.zonedDateTime.build(StringValue("1970-01-01T00:00:00.100Z")))(
          isRight(equalTo(ZonedDateTime.ofInstant(Instant.ofEpochMilli(100), ZoneOffset.UTC)))
        )
      )
    ),
    suite("derived build")(
      test("should fail when null is provided for case class with optional fields") {
        case class Foo(value: Option[String])
        val ab = ArgBuilder.gen[Foo]
        assertTrue(
          ab.build(NullValue).isLeft,
          // Sanity checks
          ab.build(ObjectValue(Map())) == Right(Foo(None)),
          ab.build(ObjectValue(Map("value" -> StringValue("foo")))) == Right(Foo(Some("foo"))),
          ab.build(ObjectValue(Map("bar" -> StringValue("foo")))) == Right(Foo(None))
        )
      },
      test("should fail when an empty object is provided for GQLValueType case classes") {
        @GQLValueType
        case class Foo(value: Option[String])
        val ab = ArgBuilder.gen[Foo]
        assertTrue(
          ab.build(ObjectValue(Map())).isLeft,
          // Sanity checks
          ab.build(NullValue) == Right(Foo(None)),
          ab.build(StringValue("foo")) == Right(Foo(Some("foo"))),
          ab.build(IntValue(42)).isLeft
        )
      }
    ),
    suite("buildMissing")(
      test("works with derived case class ArgBuilders") {
        sealed abstract class Nullable[+T]
        case class SomeNullable[+T](t: T) extends Nullable[T]
        case object NullNullable          extends Nullable[Nothing]
        case object MissingNullable       extends Nullable[Nothing]

        implicit def nullableArgBuilder[A](implicit ev: ArgBuilder[A]): ArgBuilder[Nullable[A]] =
          new ArgBuilder[Nullable[A]] {
            def build(input: InputValue): Either[ExecutionError, Nullable[A]] = input match {
              case NullValue => Right(NullNullable)
              case _         => ev.build(input).map(SomeNullable(_))
            }

            override def buildMissing(default: Option[String]): Either[ExecutionError, Nullable[A]] =
              Right(MissingNullable)
          }

        case class Wrapper(a: Nullable[String])

        val derivedAB = implicitly[ArgBuilder[Wrapper]]

        assertTrue(derivedAB.build(ObjectValue(Map())) == Right(Wrapper(MissingNullable))) &&
        assertTrue(derivedAB.build(ObjectValue(Map("a" -> NullValue))) == Right(Wrapper(NullNullable))) &&
        assertTrue(derivedAB.build(ObjectValue(Map("a" -> StringValue("x")))) == Right(Wrapper(SomeNullable("x"))))
      }
    ),
    suite("oneOf") {
      @GQLOneOfInput
      sealed trait Foo

      object Foo {
        case class Arg1(stringValue: String) extends Foo
        case class Arg2(fooString2: String)  extends Foo
        case class Arg3(intValue: Foo1)      extends Foo
        case class Arg4(fooInt2: Foo2)       extends Foo
        case class Arg5(altString: String)   extends Foo

      }

      case class Foo1(foo1: Int)
      case class Foo2(foo2: Int)

      implicit val foo1Ab: ArgBuilder[Foo1]   = ArgBuilder.gen
      implicit val foo2Ab: ArgBuilder[Foo2]   = ArgBuilder.gen
      implicit val f1Ab: ArgBuilder[Foo.Arg1] = ArgBuilder.gen
      implicit val f2Ab: ArgBuilder[Foo.Arg2] = ArgBuilder.gen
      implicit val f3Ab: ArgBuilder[Foo.Arg3] = ArgBuilder.gen
      implicit val f4Ab: ArgBuilder[Foo.Arg4] = ArgBuilder.gen
      implicit val f5Ab: ArgBuilder[Foo.Arg5] = {
        case ObjectValue(fields) =>
          fields.get("altString") match {
            case Some(StringValue(value)) => Right(Foo.Arg5(value))
            case _                        => Left(ExecutionError("altString to be provided"))
          }
        case _                   => Left(ExecutionError("expected object"))
      }
      val fooAb: ArgBuilder[Foo]              = ArgBuilder.gen

      List(
        test("valid input") {
          val inputs = List(
            Map("stringValue" -> StringValue("foo"))                    -> Foo.Arg1("foo"),
            Map("fooString2" -> StringValue("foo2"))                    -> Foo.Arg2("foo2"),
            Map("intValue" -> ObjectValue(Map("foo1" -> IntValue(42)))) -> Foo.Arg3(Foo1(42)),
            Map("fooInt2" -> ObjectValue(Map("foo2" -> IntValue(24))))  -> Foo.Arg4(Foo2(24))
          )

          inputs.foldLeft(assertCompletes) { case (acc, (input, expected)) =>
            acc && assertTrue(fooAb.build(ObjectValue(input)) == Right(expected))
          }
        },
        test("invalid input") {
          List(
            Map("invalid"     -> StringValue("foo")),
            Map("stringValue" -> StringValue("foo"), "fooString2" -> StringValue("foo2")),
            Map("stringValue" -> ObjectValue(Map("value" -> StringValue("foo")))),
            Map("stringValue" -> NullValue),
            Map("stringValue" -> StringValue("foo"), "invalid"    -> NullValue)
          )
            .map(input => assertTrue(fooAb.build(ObjectValue(input)).isLeft))
            .foldLeft(assertCompletes)(_ && _)

        },
        test("works with user-defined ArgBuilders") {
          val input    = Map("altString" -> StringValue("alt-foo"))
          val expected = Foo.Arg5("alt-foo")

          assertTrue(fooAb.build(ObjectValue(input)) == Right(expected))
        }
      )
    }
  )
}
