package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.InputValue.ObjectValue
import caliban.schema.ArgBuilder.auto._
import caliban.Value.{ IntValue, NullValue, StringValue }
import caliban.schema.Annotations.GQLOneOfInput
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

      @GQLOneOfInput("foo")
      sealed trait Foo

      object Foo {
        case class FooString(stringValue: String) extends Foo
        case class FooInt(intValue: Int)          extends Foo
      }

      implicit val fooStringAb: ArgBuilder[Foo.FooString] = ArgBuilder.gen
      implicit val fooIntAb: ArgBuilder[Foo.FooInt]       = ArgBuilder.gen
      val fooAb: ArgBuilder[Foo]                          = ArgBuilder.gen

      def mkInput(map: Map[String, InputValue]) = ObjectValue(Map("foo" -> ObjectValue(map)))

      List(
        test("valid input") {
          assertTrue(
            fooAb.build(mkInput(Map("stringValue" -> StringValue("foo")))) == Right(Foo.FooString("foo")),
            fooAb.build(mkInput(Map("intValue" -> IntValue(42)))) == Right(Foo.FooInt(42))
          )
        },
        test("invalid input") {
          List(
            Map("invalid"     -> StringValue("foo")),
            Map("stringValue" -> StringValue("foo"), "intValue" -> IntValue(42)),
            Map("stringValue" -> NullValue),
            Map("stringValue" -> NullValue, "invalid"           -> NullValue)
          )
            .map(input => assertTrue(fooAb.build(mkInput(input)).isLeft))
            .foldLeft(assertCompletes)(_ && _)

        }
      )
    }
  )
}
