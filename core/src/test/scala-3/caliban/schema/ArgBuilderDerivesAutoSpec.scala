package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.InputValue
import caliban.InputValue.{ ListValue, ObjectValue }
import caliban.schema.ArgBuilder
import caliban.schema.ArgBuilder.*
import caliban.Value.{ IntValue, NullValue, StringValue }
import zio.test.Assertion.*
import zio.test.*

import java.time.*
import scala.annotation.experimental

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

    }
  )
}
