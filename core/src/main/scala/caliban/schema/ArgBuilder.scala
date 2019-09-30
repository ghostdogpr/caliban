package caliban.schema

import scala.language.experimental.macros
import scala.util.Try
import caliban.CalibanError.ExecutionError
import caliban.parsing.adt.Value
import magnolia._

trait ArgBuilder[T] {

  def build(input: Value): Option[T]

}

object ArgBuilder {

  type Typeclass[T] = ArgBuilder[T]

  implicit val unit: ArgBuilder[Unit] = _ => Some(())
  implicit val int: ArgBuilder[Int] = {
    case Value.IntValue(value) => Some(value)
    case _                     => None
  }
  implicit val float: ArgBuilder[Float] = {
    case Value.FloatValue(value) => Some(value)
    case _                       => None
  }
  implicit val string: ArgBuilder[String] = {
    case Value.StringValue(value) => Some(value)
    case _                        => None
  }
  implicit val boolean: ArgBuilder[Boolean] = {
    case Value.BooleanValue(value) => Some(value)
    case _                         => None
  }
  implicit def option[A](implicit ev: ArgBuilder[A]): ArgBuilder[Option[A]] =
    (input: Value) => Some(ev.build(input))

  def combine[T](ctx: CaseClass[ArgBuilder, T]): ArgBuilder[T] =
    (input: Value) =>
      Try(ctx.construct { p =>
        input match {
          case Value.ObjectValue(fields) =>
            p.typeclass
              .build(fields.getOrElse(p.label, Value.NullValue))
              .getOrElse(throw ExecutionError("Failed to generate argument"))
          case value =>
            p.typeclass.build(value).getOrElse(throw ExecutionError("Failed to generate argument"))
        }
      }).toOption

  def dispatch[T](ctx: SealedTrait[ArgBuilder, T]): ArgBuilder[T] = {
    case Value.EnumValue(value) =>
      ctx.subtypes.find(_.typeName.short == value).get.typeclass.build(Value.ObjectValue(Map()))
    case Value.StringValue(value) =>
      ctx.subtypes.find(_.typeName.short == value).get.typeclass.build(Value.ObjectValue(Map()))
    case _ => None
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
