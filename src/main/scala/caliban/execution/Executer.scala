package caliban.execution

import scala.language.experimental.macros
import caliban.execution.ResponseValue._
import caliban.parsing.adt.Selection.Field
import caliban.parsing.adt._
import magnolia._

trait Executer[T] {

  def exec(schema: T, selectionSet: List[Selection], arguments: Map[String, Value] = Map()): ResponseValue

}

object Executer {

  type Typeclass[T] = Executer[T]

  implicit val boolean: Executer[Boolean] = (a: Boolean, _: List[Selection], _: Map[String, Value]) => BooleanValue(a)
  implicit val int: Executer[Int]         = (a: Int, _: List[Selection], _: Map[String, Value]) => IntValue(a)
  implicit val float: Executer[Float]     = (a: Float, _: List[Selection], _: Map[String, Value]) => FloatValue(a)
  implicit val double: Executer[Double] = (a: Double, _: List[Selection], _: Map[String, Value]) =>
    FloatValue(a.toFloat)
  implicit val string: Executer[String] = (a: String, _: List[Selection], _: Map[String, Value]) => StringValue(a)
  implicit def option[A](implicit ev: Executer[A]): Executer[Option[A]] =
    (a: Option[A], selectionSet: List[Selection], _: Map[String, Value]) =>
      a match {
        case Some(value) => ev.exec(value, selectionSet)
        case None        => NullValue
      }
  implicit def list[A](implicit ev: Executer[A]): Executer[List[A]] =
    (items: List[A], selectionSet: List[Selection], _: Map[String, Value]) =>
      ListValue(items.map(ev.exec(_, selectionSet)))
  implicit def set[A](implicit ev: Executer[A]): Executer[Set[A]] =
    (items: Set[A], selectionSet: List[Selection], _: Map[String, Value]) =>
      ListValue(items.map(ev.exec(_, selectionSet)).toList)
  implicit def function[A, B](implicit ev1: ArgBuilder[A], ev2: Executer[B]): Executer[A => B] =
    (f: A => B, selectionSet: List[Selection], arguments: Map[String, Value]) => {
      val argValue: A = ev1.build(Right(arguments))
      ev2.exec(f(argValue), selectionSet)
    }

  def combine[T](ctx: CaseClass[Executer, T]): Executer[T] =
    (schema: T, selectionSet: List[Selection], _: Map[String, Value]) =>
      if (ctx.isObject) {
        EnumValue(ctx.typeName.short)
      } else
        ObjectValue(selectionSet.flatMap {
          case Field(alias, name, args, _, selectionSet) =>
            ctx.parameters.find(_.label == name).map { p =>
              (alias.getOrElse(name), p.typeclass.exec(p.dereference(schema), selectionSet, args))
            }
          case _ => throw new Exception("Fragments are not supported yet")
        })

  def dispatch[T](ctx: SealedTrait[Executer, T]): Executer[T] =
    (schema: T, selectionSet: List[Selection], arguments: Map[String, Value]) =>
      ctx.dispatch(schema)(subType => subType.typeclass.exec(subType.cast(schema), selectionSet, arguments))

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
