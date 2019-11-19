package caliban.schema

import scala.language.experimental.macros
import caliban.CalibanError.ExecutionError
import caliban.parsing.adt.Value
import caliban.schema.Annotations.GQLName
import magnolia._
import zio.{ FiberFailure, IO }

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

  /**
   * Builds a new `ArgBuilder` of A from an existing `ArgBuilder` of `T` and a function from `T` to `IO[ExecutionError, A]`.
   * @param f a function from `T` to IO[ExecutionError, A]
   */
  def mapM[A](f: T => IO[ExecutionError, A]): ArgBuilder[A] = (input: Value) => self.build(input).flatMap(f)
}

object ArgBuilder {

  type Typeclass[T] = ArgBuilder[T]

  implicit val unit: ArgBuilder[Unit] = _ => IO.succeed(())
  implicit val long: ArgBuilder[Long] = {
    case Value.IntValue(value) => IO.succeed(value)
    case other                 => IO.fail(ExecutionError(s"Can't build an Long from input $other"))
  }
  implicit val int: ArgBuilder[Int] = long.map(_.toInt)
  implicit val double: ArgBuilder[Double] = {
    case Value.IntValue(value)   => IO.succeed(value.toDouble)
    case Value.FloatValue(value) => IO.succeed(value)
    case other                   => IO.fail(ExecutionError(s"Can't build a Double from input $other"))
  }
  implicit val float: ArgBuilder[Float] = double.map(_.toFloat)
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
    case Value.ListValue(items) => IO.foreach(items)(ev.build)
    case other                  => ev.build(other).map(List(_))
  }

  def combine[T](ctx: CaseClass[ArgBuilder, T]): ArgBuilder[T] =
    (input: Value) =>
      IO.runtime.flatMap(
        rts =>
          IO.effect(
              ctx.construct { p =>
                (input match {
                  case Value.ObjectValue(fields) =>
                    rts.unsafeRunSync(p.typeclass.build(fields.getOrElse(p.label, Value.NullValue)))
                  case value =>
                    rts.unsafeRunSync(p.typeclass.build(value))
                }).getOrElse(
                  c =>
                    c.failures match {
                      case ex :: Nil => throw ex
                      case _         => throw FiberFailure(c)
                    }
                )
              }
            )
            .mapError {
              case e: ExecutionError => e
              case e                 => ExecutionError("Exception during argument building", None, Some(e))
            }
      )

  def dispatch[T](ctx: SealedTrait[ArgBuilder, T]): ArgBuilder[T] = input => {
    (input match {
      case Value.StringValue(value) => Some(value)
      case Value.EnumValue(value)   => Some(value)
      case _                        => None
    }) match {
      case Some(value) =>
        ctx.subtypes
          .find(
            t => t.annotations.collectFirst { case GQLName(name) => name }.contains(value) || t.typeName.short == value
          ) match {
          case Some(subtype) => subtype.typeclass.build(Value.ObjectValue(Map()))
          case None          => IO.fail(ExecutionError(s"Invalid value $value for trait ${ctx.typeName.short}"))
        }
      case None => IO.fail(ExecutionError(s"Can't build an trait from input $input"))
    }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
