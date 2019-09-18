package caliban.schema

import scala.language.experimental.macros
import scala.util.Try
import caliban.CalibanError.ExecutionError
import caliban.parsing.adt.Value
import magnolia._

trait ArgBuilder[T] {

  def build(input: Either[Value, Map[String, Value]]): Option[T]

}

object ArgBuilder {

  type Typeclass[T] = ArgBuilder[T]

  implicit val unit: ArgBuilder[Unit] = _ => Some(())
  implicit val int: ArgBuilder[Int] = {
    case Left(Value.IntValue(value)) => Some(value)
    case _                           => None
  }
  implicit val float: ArgBuilder[Float] = {
    case Left(Value.FloatValue(value)) => Some(value)
    case _                             => None
  }
  implicit val string: ArgBuilder[String] = {
    case Left(Value.StringValue(value)) => Some(value)
    case _                              => None
  }
  implicit val boolean: ArgBuilder[Boolean] = {
    case Left(Value.BooleanValue(value)) => Some(value)
    case _                               => None
  }
  implicit def option[A](implicit ev: ArgBuilder[A]): ArgBuilder[Option[A]] =
    (input: Either[Value, Map[String, Value]]) => Some(ev.build(input))

  def combine[T](ctx: CaseClass[ArgBuilder, T]): ArgBuilder[T] =
    (input: Either[Value, Map[String, Value]]) =>
      Try(ctx.construct { p =>
        input match {
          case Left(value) =>
            p.typeclass.build(Left(value)).getOrElse(throw ExecutionError("Failed to generate argument"))
          case Right(arguments) =>
            p.typeclass
              .build(Left(arguments.getOrElse(p.label, Value.NullValue)))
              .getOrElse(throw ExecutionError("Failed to generate argument"))
        }
      }).toOption

  def dispatch[T](ctx: SealedTrait[ArgBuilder, T]): ArgBuilder[T] = {
    case Left(Value.EnumValue(value)) =>
      ctx.subtypes.find(_.typeName.short == value).get.typeclass.build(Right(Map()))
    case _ => None
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
