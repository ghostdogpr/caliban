package caliban

import scala.language.experimental.macros
import scala.util.Try
import caliban.Parser.Value
import magnolia._

trait ArgBuilder[T] {

  def build(input: Either[Parser.Value, Map[String, Parser.Value]]): T

}

object ArgBuilder {

  type Typeclass[T] = ArgBuilder[T]

  implicit val int: ArgBuilder[Int] = {
    case Left(Value.IntValue(value)) => value
    case _                           => throw new Exception("Invalid")
  }
  implicit val string: ArgBuilder[String] = {
    case Left(Value.StringValue(value)) => value
    case _                              => throw new Exception("Invalid")
  }
  implicit def option[A](implicit ev: ArgBuilder[A]): ArgBuilder[Option[A]] =
    (input: Either[Value, Map[String, Value]]) => Try(ev.build(input)).toOption

  def combine[T](ctx: CaseClass[ArgBuilder, T]): ArgBuilder[T] =
    (input: Either[Parser.Value, Map[String, Parser.Value]]) =>
      ctx.construct { p =>
        input match {
          case Left(value)      => p.typeclass.build(Left(value))
          case Right(arguments) => p.typeclass.build(Left(arguments.getOrElse(p.label, Parser.Value.NullValue)))
        }
      }

  def dispatch[T](ctx: SealedTrait[ArgBuilder, T]): ArgBuilder[T] =
    (input: Either[Value, Map[String, Value]]) =>
      input match {
        case Left(Parser.Value.EnumValue(value)) =>
          ctx.subtypes.find(_.typeName.short == value).get.typeclass.build(Right(Map()))
        case _ => ???
      }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
