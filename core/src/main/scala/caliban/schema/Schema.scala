package caliban.schema

import scala.language.experimental.macros
import caliban.CalibanError.ExecutionError
import caliban.execution.Executor.mergeSelectionSet
import caliban.parsing.adt.ExecutableDefinition.FragmentDefinition
import caliban.parsing.adt.{ Selection, Value }
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.ResponseValue._
import caliban.schema.Types._
import magnolia._
import zio.{ IO, UIO }

trait Schema[T] {
  def optional: Boolean             = false
  def arguments: List[__InputValue] = Nil
  def toType(isInput: Boolean = false): __Type
  def exec(
    value: T,
    selectionSet: List[Selection],
    arguments: Map[String, Value],
    fragments: Map[String, FragmentDefinition]
  ): IO[ExecutionError, ResponseValue]
}

object Schema {

  implicit val unitSchema: Schema[Unit] = new Schema[Unit] {
    override def toType(isInput: Boolean = false): __Type = makeObject(None, None, Nil)
    override def exec(
      value: Unit,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] = UIO(ObjectValue(Nil))
  }
  implicit val booleanSchema: Schema[Boolean] = new Schema[Boolean] {
    override def toType(isInput: Boolean = false): __Type = makeScalar("Boolean")
    override def exec(
      value: Boolean,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] =
      UIO(BooleanValue(value))
  }
  implicit val intSchema: Schema[Int] = new Schema[Int] {
    override def toType(isInput: Boolean = false): __Type = makeScalar("Int")
    override def exec(
      value: Int,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] =
      UIO(IntValue(value))
  }
  implicit val floatSchema: Schema[Float] = new Schema[Float] {
    override def toType(isInput: Boolean = false): __Type = makeScalar("Float")
    override def exec(
      value: Float,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] =
      UIO(FloatValue(value))
  }
  implicit val doubleSchema: Schema[Double] = new Schema[Double] {
    override def toType(isInput: Boolean = false): __Type = makeScalar("Float")
    override def exec(
      value: Double,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] = UIO(FloatValue(value.toFloat))
  }
  implicit val stringSchema: Schema[String] = new Schema[String] {
    override def toType(isInput: Boolean = false): __Type = makeScalar("String")
    override def exec(
      value: String,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] = UIO(StringValue(value))
  }
  implicit def optionSchema[A](implicit ev: Schema[A]): Schema[Option[A]] = new Typeclass[Option[A]] {
    override def optional: Boolean                        = true
    override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
    override def exec(
      value: Option[A],
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] = value match {
      case Some(value) => ev.exec(value, selectionSet, arguments, fragments)
      case None        => UIO(NullValue)
    }
  }
  implicit def listSchema[A](implicit ev: Schema[A]): Schema[List[A]] = new Typeclass[List[A]] {
    override def toType(isInput: Boolean = false): __Type = makeList(ev.toType(isInput))
    override def exec(
      value: List[A],
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] =
      IO.collectAll(value.map(ev.exec(_, selectionSet, arguments, fragments))).map(ListValue)
  }
  implicit def setSchema[A](implicit ev: Schema[A]): Schema[Set[A]] = new Typeclass[Set[A]] {
    override def toType(isInput: Boolean = false): __Type = makeList(ev.toType(isInput))
    override def exec(
      value: Set[A],
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] =
      IO.collectAll(value.map(ev.exec(_, selectionSet, arguments, fragments))).map(ListValue)
  }
  implicit def functionUnitSchema[A](implicit ev: Schema[A]): Schema[() => A] = new Typeclass[() => A] {
    override def optional: Boolean                        = ev.optional
    override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
    override def exec(
      value: () => A,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] = ev.exec(value(), selectionSet, Map(), fragments)
  }
  implicit def functionSchema[A, B](implicit arg1: ArgBuilder[A], ev1: Schema[A], ev2: Schema[B]): Schema[A => B] =
    new Typeclass[A => B] {
      override def arguments: List[__InputValue]            = ev1.toType(true).inputFields.getOrElse(Nil)
      override def optional: Boolean                        = ev2.optional
      override def toType(isInput: Boolean = false): __Type = ev2.toType(isInput)
      override def exec(
        value: A => B,
        selectionSet: List[Selection],
        arguments: Map[String, Value],
        fragments: Map[String, FragmentDefinition]
      ): IO[ExecutionError, ResponseValue] = {
        val argValue: A = arg1.build(Right(arguments))
        ev2.exec(value(argValue), selectionSet, Map(), fragments)
      }
    }

  type Typeclass[T] = Schema[T]

  def combine[T](ctx: CaseClass[Schema, T]): Schema[T] = new Schema[T] {
    override def toType(isInput: Boolean = false): __Type = {
      val name        = ctx.typeName.short
      val description = ctx.annotations.collectFirst { case GQLDescription(desc) => desc }
      if (isInput)
        makeInputObject(
          Some(name),
          description,
          ctx.parameters
            .map(
              p =>
                __InputValue(
                  p.label,
                  p.annotations.collectFirst { case GQLDescription(desc) => desc },
                  () =>
                    if (p.typeclass.optional) p.typeclass.toType(isInput) else makeNonNull(p.typeclass.toType(isInput)),
                  None
                )
            )
            .toList
        )
      else
        makeObject(
          Some(name),
          description,
          ctx.parameters
            .map(
              p =>
                __Field(
                  p.label,
                  p.annotations.collectFirst { case GQLDescription(desc) => desc },
                  p.typeclass.arguments,
                  () =>
                    if (p.typeclass.optional) p.typeclass.toType(isInput) else makeNonNull(p.typeclass.toType(isInput)),
                  p.annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
                  p.annotations.collectFirst { case GQLDeprecated(reason) => reason }
                )
            )
            .toList
        )
    }

    override def exec(
      value: T,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] =
      if (ctx.isObject) {
        UIO(ResponseValue.EnumValue(ctx.typeName.short))
      } else {
        val mergedSelectionSet = mergeSelectionSet(selectionSet, ctx.typeName.short, fragments)
        IO.collectAll(mergedSelectionSet.map {
            case Selection.Field(alias, name, args, _, selectionSet) =>
              ctx.parameters
                .find(_.label == name)
                .map(p => p.typeclass.exec(p.dereference(value), selectionSet, args, fragments))
                .getOrElse(UIO.succeed(NullValue))
                .map((alias.getOrElse(name), _))
          })
          .map(ObjectValue)
      }
  }

  def dispatch[T](ctx: SealedTrait[Schema, T]): Schema[T] = new Typeclass[T] {
    override def toType(isInput: Boolean = false): __Type = {
      val subtypes =
        ctx.subtypes.map(s => s.typeclass.toType(isInput) -> s.annotations).toList.sortBy(_._1.name.getOrElse(""))
      val isEnum = subtypes.forall {
        case (t, _) if t.fields(DeprecatedArgs(Some(true))).forall(_.isEmpty) && t.inputFields.forall(_.isEmpty) => true
        case _                                                                                                   => false
      }
      if (isEnum && subtypes.nonEmpty)
        makeEnum(
          Some(ctx.typeName.short),
          ctx.annotations.collectFirst { case GQLDescription(desc) => desc },
          subtypes.collect {
            case (__Type(__TypeKind.OBJECT, Some(name), description, _, _, _, _, _, _), annotations) =>
              Types.__EnumValue(
                name,
                description,
                annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
                annotations.collectFirst { case GQLDeprecated(reason) => reason }
              )
          }
        )
      else
        makeUnion(
          Some(ctx.typeName.short),
          ctx.annotations.collectFirst { case GQLDescription(desc) => desc },
          subtypes.map(_._1)
        )
    }

    override def exec(
      value: T,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition]
    ): IO[ExecutionError, ResponseValue] =
      ctx.dispatch(value)(subType => subType.typeclass.exec(subType.cast(value), selectionSet, arguments, fragments))
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
