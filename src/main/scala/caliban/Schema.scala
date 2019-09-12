package caliban

import scala.language.experimental.macros
import caliban.Annotations.GQLDescription
import caliban.Types.{
  makeEnum,
  makeInputObject,
  makeList,
  makeNonNull,
  makeObject,
  makeScalar,
  makeUnion,
  Argument,
  Field,
  Type,
  TypeKind
}
import magnolia._

trait Schema[T] {
  def optional: Boolean         = false
  def arguments: List[Argument] = Nil
  def toType: Type
}

object Schema {

  implicit val intScalar: Schema[Int] = new Schema[Int] {
    override def toType: Type = makeScalar("Int")
  }
  implicit val stringScalar: Schema[String] = new Schema[String] {
    override def toType: Type = makeScalar("String")
  }
  implicit def option[A](implicit ev: Schema[A]): Schema[Option[A]] = new Typeclass[Option[A]] {
    override def optional: Boolean = true
    override def toType: Type      = ev.toType
  }
  implicit def list[A](implicit ev: Schema[A]): Schema[List[A]] = new Typeclass[List[A]] {
    override def toType: Type = makeList(ev.toType)
  }
  implicit def set[A](implicit ev: Schema[A]): Schema[Set[A]] = new Typeclass[Set[A]] {
    override def toType: Type = makeList(ev.toType)
  }
  implicit def function[A, B](implicit ev1: Schema[A], ev2: Schema[B]): Schema[A => B] = new Typeclass[A => B] {
    override def arguments: List[Argument] = {
      val incomingType = ev1.toType
      incomingType.kind match {
        case TypeKind.OBJECT =>
          incomingType.fields.map { f =>
            val mappedFieldType = (f.`type`.kind, f.`type`.ofType) match {
              case (TypeKind.NON_NULL, Some(Type(TypeKind.OBJECT, name, description, fields, _, _, _))) =>
                makeNonNull(makeInputObject(name, description, fields))
              case (TypeKind.OBJECT, _) => makeInputObject(f.`type`.name, f.`type`.description, f.`type`.fields)
              case _                    => f.`type`
            }
            Argument(f.name, f.description, mappedFieldType)
          }
        case _ => Nil
      }
    }
    override def optional: Boolean = ev2.optional
    override def toType: Type      = ev2.toType
  }

  type Typeclass[T] = Schema[T]

  def combine[T](ctx: CaseClass[Schema, T]): Schema[T] = new Schema[T] {
    override def toType: Type =
      makeObject(
        Some(ctx.typeName.short),
        ctx.annotations.collectFirst { case GQLDescription(desc) => desc },
        ctx.parameters
          .map(
            p =>
              Field(
                p.label,
                p.annotations.collectFirst { case GQLDescription(desc) => desc },
                p.typeclass.arguments,
                if (p.typeclass.optional) p.typeclass.toType else makeNonNull(p.typeclass.toType)
              )
          )
          .toList
      )
  }

  def dispatch[T](ctx: SealedTrait[Schema, T]): Schema[T] = new Typeclass[T] {
    override def toType: Type = {
      val subtypes = ctx.subtypes.map(_.typeclass.toType).toList
      val isEnum = subtypes.forall {
        case Type(TypeKind.OBJECT, _, _, Nil, _, _, _) => true
        case _                                         => false
      }
      if (isEnum && subtypes.nonEmpty)
        makeEnum(
          Some(ctx.typeName.short),
          ctx.annotations.collectFirst { case GQLDescription(desc) => desc },
          subtypes.collect {
            case Type(TypeKind.OBJECT, Some(name), _, _, _, _, _) => name
          }
        )
      else
        makeUnion(
          Some(ctx.typeName.short),
          ctx.annotations.collectFirst { case GQLDescription(desc) => desc },
          subtypes
        )
    }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
