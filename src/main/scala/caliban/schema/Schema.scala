package caliban.schema

import scala.language.experimental.macros
import caliban.CalibanError.ExecutionError
import caliban.parsing.adt.{ Selection, Value }
import caliban.schema.Annotations.GQLDescription
import caliban.schema.ResponseValue._
import caliban.schema.Types._
import magnolia._
import zio.{ IO, UIO }

trait Schema[T] {
  def optional: Boolean         = false
  def arguments: List[Argument] = Nil
  def toType: Type
  def exec(
    value: T,
    selectionSet: List[Selection],
    arguments: Map[String, Value] = Map()
  ): IO[ExecutionError, ResponseValue]
}

object Schema {

  implicit val booleanSchema: Schema[Boolean] = new Schema[Boolean] {
    override def toType: Type = makeScalar("Boolean")
    override def exec(
      value: Boolean,
      selectionSet: List[Selection],
      arguments: Map[String, Value]
    ): IO[ExecutionError, ResponseValue] =
      UIO(BooleanValue(value))
  }
  implicit val intSchema: Schema[Int] = new Schema[Int] {
    override def toType: Type = makeScalar("Int")
    override def exec(
      value: Int,
      selectionSet: List[Selection],
      arguments: Map[String, Value]
    ): IO[ExecutionError, ResponseValue] =
      UIO(IntValue(value))
  }
  implicit val floatSchema: Schema[Float] = new Schema[Float] {
    override def toType: Type = makeScalar("Float")
    override def exec(
      value: Float,
      selectionSet: List[Selection],
      arguments: Map[String, Value]
    ): IO[ExecutionError, ResponseValue] =
      UIO(FloatValue(value))
  }
  implicit val doubleSchema: Schema[Double] = new Schema[Double] {
    override def toType: Type = makeScalar("Float")
    override def exec(
      value: Double,
      selectionSet: List[Selection],
      arguments: Map[String, Value]
    ): IO[ExecutionError, ResponseValue] = UIO(FloatValue(value.toFloat))
  }
  implicit val stringSchema: Schema[String] = new Schema[String] {
    override def toType: Type = makeScalar("String")
    override def exec(
      value: String,
      selectionSet: List[Selection],
      arguments: Map[String, Value]
    ): IO[ExecutionError, ResponseValue] = UIO(StringValue(value))
  }
  implicit def optionSchema[A](implicit ev: Schema[A]): Schema[Option[A]] = new Typeclass[Option[A]] {
    override def optional: Boolean = true
    override def toType: Type      = ev.toType
    override def exec(
      value: Option[A],
      selectionSet: List[Selection],
      arguments: Map[String, Value]
    ): IO[ExecutionError, ResponseValue] = value match {
      case Some(value) => ev.exec(value, selectionSet)
      case None        => UIO(NullValue)
    }
  }
  implicit def listSchema[A](implicit ev: Schema[A]): Schema[List[A]] = new Typeclass[List[A]] {
    override def toType: Type = makeList(ev.toType)
    override def exec(
      value: List[A],
      selectionSet: List[Selection],
      arguments: Map[String, Value]
    ): IO[ExecutionError, ResponseValue] = IO.collectAll(value.map(ev.exec(_, selectionSet))).map(ListValue)
  }
  implicit def setSchema[A](implicit ev: Schema[A]): Schema[Set[A]] = new Typeclass[Set[A]] {
    override def toType: Type = makeList(ev.toType)
    override def exec(
      value: Set[A],
      selectionSet: List[Selection],
      arguments: Map[String, Value]
    ): IO[ExecutionError, ResponseValue] = IO.collectAll(value.map(ev.exec(_, selectionSet))).map(ListValue)
  }
  implicit def functionSchema[A, B](implicit arg1: ArgBuilder[A], ev1: Schema[A], ev2: Schema[B]): Schema[A => B] =
    new Typeclass[A => B] {
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
      override def exec(
        value: A => B,
        selectionSet: List[Selection],
        arguments: Map[String, Value]
      ): IO[ExecutionError, ResponseValue] = {
        val argValue: A = arg1.build(Right(arguments))
        ev2.exec(value(argValue), selectionSet)
      }
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

    override def exec(
      value: T,
      selectionSet: List[Selection],
      arguments: Map[String, Value]
    ): IO[ExecutionError, ResponseValue] =
      if (ctx.isObject) {
        UIO(EnumValue(ctx.typeName.short))
      } else {
        IO.collectAll(selectionSet.map {
            case Selection.Field(alias, name, args, _, selectionSet) =>
              ctx.parameters
                .find(_.label == name)
                .map(p => p.typeclass.exec(p.dereference(value), selectionSet, args))
                .getOrElse(UIO.succeed(NullValue))
                .map((alias.getOrElse(name), _))
            case _ => IO.fail(ExecutionError("Fragments are not supported yet"))
          })
          .map(ObjectValue)
      }
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

    override def exec(
      value: T,
      selectionSet: List[Selection],
      arguments: Map[String, Value]
    ): IO[ExecutionError, ResponseValue] =
      ctx.dispatch(value)(subType => subType.typeclass.exec(subType.cast(value), selectionSet, arguments))
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
