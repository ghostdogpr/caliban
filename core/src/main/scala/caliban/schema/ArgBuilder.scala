package caliban.schema

import scala.language.experimental.macros
import caliban.CalibanError.ExecutionError
import caliban.parsing.adt.Value
import magnolia._
import zio.IO

/**
 * Typeclass that defines how to build an argument of type `T` from an input [[caliban.parsing.adt.Value]].
 * Every type that can be passed as an argument needs an instance of `ArgBuilder`.
 */
trait ArgBuilder[T] { self =>

  /**
   * Builds a value of type `T` from an input [[caliban.parsing.adt.Value]].
   * Fails with an [[caliban.CalibanError.ExecutionError]] if it was impossible to build the value.
   */
  def build(input: Value): IO[ExecutionError, T]

  /**
   * Builds a new `ArgBuilder` of `A` from an existing `ArgBuilder` of `T` and a function from `T` to `A`.
   * @param f a function from `T` to `A`.
   */
  def map[A](f: T => A): ArgBuilder[A] = (input: Value) => self.build(input).map(f)
}

object ArgBuilder {

  type Typeclass[T] = ArgBuilder[T]

  implicit val unit: ArgBuilder[Unit] = _ => IO.succeed(())
  implicit val int: ArgBuilder[Int] = {
    case Value.IntValue(value) => IO.succeed(value)
    case other                 => IO.fail(ExecutionError(s"Can't build an Int from input $other"))
  }
  implicit val float: ArgBuilder[Float] = {
    case Value.IntValue(value)   => IO.succeed(value.toFloat)
    case Value.FloatValue(value) => IO.succeed(value)
    case other                   => IO.fail(ExecutionError(s"Can't build a Float from input $other"))
  }
  implicit val string: ArgBuilder[String] = {
    case Value.StringValue(value) => IO.succeed(value)
    case other                    => IO.fail(ExecutionError(s"Can't build a String from input $other"))
  }
  implicit val boolean: ArgBuilder[Boolean] = {
    case Value.BooleanValue(value) => IO.succeed(value)
    case other                     => IO.fail(ExecutionError(s"Can't build a Boolean from input $other"))
  }
  implicit def option[A](implicit ev: ArgBuilder[A]): ArgBuilder[Option[A]] = {
    case Value.NullValue => IO.none
    case value           => ev.build(value).map(Some(_))
  }
  implicit def list[A](implicit ev: ArgBuilder[A]): ArgBuilder[List[A]] = {
    case Value.ListValue(items) => IO.foreachPar(items)(ev.build)
    case other                  => ev.build(other).map(List(_))
  }

  def combine[T](ctx: CaseClass[ArgBuilder, T]): ArgBuilder[T] =
    (input: Value) =>
      IO.runtime.flatMap(
        rts =>
          IO.effect(
              ctx.construct { p =>
                input match {
                  case Value.ObjectValue(fields) =>
                    rts.unsafeRun(p.typeclass.build(fields.getOrElse(p.label, Value.NullValue)))
                  case value =>
                    rts.unsafeRun(p.typeclass.build(value))
                }
              }
            )
            .mapError {
              case e: ExecutionError => e
              case e                 => ExecutionError("Exception during argument building", Some(e))
            }
      )

  def dispatch[T](ctx: SealedTrait[ArgBuilder, T]): ArgBuilder[T] = {
    case Value.EnumValue(value) =>
      ctx.subtypes.find(_.typeName.short == value).get.typeclass.build(Value.ObjectValue(Map()))
    case Value.StringValue(value) =>
      ctx.subtypes.find(_.typeName.short == value).get.typeclass.build(Value.ObjectValue(Map()))
    case other =>
      IO.fail(ExecutionError(s"Can't build an trait from input $other"))
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
