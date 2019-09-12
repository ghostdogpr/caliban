package caliban

import scala.language.experimental.macros
import caliban.parsing.adt.Selection.Field
import caliban.parsing.adt._
import magnolia._

trait Executer[T] {

  def exec(schema: T, selectionSet: List[Selection], arguments: Map[String, Value] = Map()): String

}

object Executer {

  type Typeclass[T] = Executer[T]

  implicit val boolean: Executer[Boolean] = (a: Boolean, _: List[Selection], _: Map[String, Value]) => a.toString
  implicit val int: Executer[Int]         = (a: Int, _: List[Selection], _: Map[String, Value]) => a.toString
  implicit val float: Executer[Float]     = (a: Float, _: List[Selection], _: Map[String, Value]) => a.toString
  implicit val double: Executer[Double]   = (a: Double, _: List[Selection], _: Map[String, Value]) => a.toString
  implicit val string: Executer[String]   = (a: String, _: List[Selection], _: Map[String, Value]) => s""""$a""""
  implicit def option[A](implicit ev: Executer[A]): Executer[Option[A]] =
    (a: Option[A], selectionSet: List[Selection], _: Map[String, Value]) =>
      a match {
        case Some(value) => ev.exec(value, selectionSet)
        case None        => "null"
      }
  implicit def list[A](implicit ev: Executer[A]): Executer[List[A]] =
    (items: List[A], selectionSet: List[Selection], _: Map[String, Value]) =>
      items.map(ev.exec(_, selectionSet)).mkString("[", ",", "]")
  implicit def set[A](implicit ev: Executer[A]): Executer[Set[A]] =
    (items: Set[A], selectionSet: List[Selection], _: Map[String, Value]) =>
      items.map(ev.exec(_, selectionSet)).mkString("[", ",", "]")
  implicit def function[A, B](implicit ev1: ArgBuilder[A], ev2: Executer[B]): Executer[A => B] =
    (f: A => B, selectionSet: List[Selection], arguments: Map[String, Value]) => {
      val argValue: A = ev1.build(Right(arguments))
      ev2.exec(f(argValue), selectionSet)
    }

  def combine[T](ctx: CaseClass[Executer, T]): Executer[T] =
    (schema: T, selectionSet: List[Selection], _: Map[String, Value]) =>
      if (ctx.isObject) {
        s""""${ctx.typeName.short}""""
      } else
        selectionSet.flatMap {
          case Field(alias, name, args, _, selectionSet) =>
            ctx.parameters.find(_.label == name).map { p =>
              s""""${alias.getOrElse(name)}":${p.typeclass.exec(p.dereference(schema), selectionSet, args)}"""
            }
          case _ => throw new Exception("Fragments are not supported yet")
        }.mkString("{", ",", "}")

  def dispatch[T](ctx: SealedTrait[Executer, T]): Executer[T] =
    (schema: T, selectionSet: List[Selection], arguments: Map[String, Value]) =>
      ctx.dispatch(schema)(subType => subType.typeclass.exec(subType.cast(schema), selectionSet, arguments))

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
