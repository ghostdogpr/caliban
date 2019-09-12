package caliban

import scala.language.experimental.macros
import caliban.Parser.Value
import magnolia._

trait Executer[T] {

  def exec(schema: T, selectionSet: List[Parser.Selection], arguments: Map[String, Parser.Value] = Map()): String

}

object Executer {

  type Typeclass[T] = Executer[T]

  implicit val boolean: Executer[Boolean] = (a: Boolean, _: List[Parser.Selection], _: Map[String, Parser.Value]) =>
    a.toString
  implicit val int: Executer[Int] = (a: Int, _: List[Parser.Selection], _: Map[String, Parser.Value]) => a.toString
  implicit val string: Executer[String] = (a: String, _: List[Parser.Selection], _: Map[String, Parser.Value]) =>
    s""""$a""""
  implicit def option[A](implicit ev: Executer[A]): Executer[Option[A]] =
    (a: Option[A], selectionSet: List[Parser.Selection], _: Map[String, Parser.Value]) =>
      a match {
        case Some(value) => ev.exec(value, selectionSet)
        case None        => "null"
      }
  implicit def list[A](implicit ev: Executer[A]): Executer[List[A]] =
    (items: List[A], selectionSet: List[Parser.Selection], _: Map[String, Parser.Value]) =>
      items.map(ev.exec(_, selectionSet)).mkString("[", ",", "]")
  implicit def set[A](implicit ev: Executer[A]): Executer[Set[A]] =
    (items: Set[A], selectionSet: List[Parser.Selection], _: Map[String, Parser.Value]) =>
      items.map(ev.exec(_, selectionSet)).mkString("[", ",", "]")
  implicit def function[A, B](implicit ev1: ArgBuilder[A], ev2: Executer[B]): Executer[A => B] =
    (f: A => B, selectionSet: List[Parser.Selection], arguments: Map[String, Parser.Value]) => {
      val argValue: A = ev1.build(Right(arguments))
      ev2.exec(f(argValue), selectionSet)
    }

  def combine[T](ctx: CaseClass[Executer, T]): Executer[T] =
    (schema: T, selectionSet: List[Parser.Selection], _: Map[String, Parser.Value]) =>
      if (ctx.isObject) {
        s""""${ctx.typeName.short}""""
      } else
        selectionSet.flatMap {
          case Parser.Field(alias, name, args, _, selectionSet) =>
            ctx.parameters.find(_.label == name).map { p =>
              s""""${alias.getOrElse(name)}":${p.typeclass.exec(p.dereference(schema), selectionSet, args)}"""
            }
          case _ => throw new Exception("Fragments are not supported yet")
        }.mkString("{", ",", "}")

  def dispatch[T](ctx: SealedTrait[Executer, T]): Executer[T] =
    (schema: T, selectionSet: List[Parser.Selection], arguments: Map[String, Value]) =>
      ctx.dispatch(schema)(subType => subType.typeclass.exec(subType.cast(schema), selectionSet, arguments))

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
