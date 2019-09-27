package caliban.schema

import scala.language.experimental.macros
import caliban.CalibanError.ExecutionError
import caliban.execution.Executor.mergeSelectionSet
import caliban.introspection.adt._
import caliban.parsing.adt.ExecutableDefinition.FragmentDefinition
import caliban.parsing.adt.{ Selection, Value }
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription, GQLName }
import caliban.schema.ResponseValue._
import caliban.schema.Types._
import magnolia._
import zio.{ IO, UIO, ZIO }

trait Schema[T] { self =>
  def optional: Boolean             = false
  def arguments: List[__InputValue] = Nil
  def toType(isInput: Boolean = false): __Type
  def exec(
    value: T,
    selectionSet: List[Selection],
    arguments: Map[String, Value],
    fragments: Map[String, FragmentDefinition],
    parallel: Boolean
  ): IO[ExecutionError, ResponseValue]

  def contramap[A](f: A => T): Schema[A] = new Schema[A] {
    override def optional: Boolean                = self.optional
    override def arguments: List[__InputValue]    = self.arguments
    override def toType(isInput: Boolean): __Type = self.toType(isInput)
    override def exec(
      value: A,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition],
      parallel: Boolean
    ): IO[ExecutionError, ResponseValue] = self.exec(f(value), selectionSet, arguments, fragments, parallel)
  }
}

object Schema {

  def scalarSchema[A](name: String, description: Option[String], makeResponse: A => ResponseValue): Schema[A] =
    new Schema[A] {
      override def toType(isInput: Boolean): __Type = makeScalar(name, description)
      override def exec(
        value: A,
        selectionSet: List[Selection],
        arguments: Map[String, Value],
        fragments: Map[String, FragmentDefinition],
        parallel: Boolean
      ): IO[ExecutionError, ResponseValue] = IO.succeed(makeResponse(value))
    }

  implicit val unitSchema: Schema[Unit]       = scalarSchema("Unit", None, _ => ObjectValue(Nil))
  implicit val booleanSchema: Schema[Boolean] = scalarSchema("Boolean", None, BooleanValue)
  implicit val stringSchema: Schema[String]   = scalarSchema("String", None, StringValue)
  implicit val intSchema: Schema[Int]         = scalarSchema("Int", None, IntValue)
  implicit val floatSchema: Schema[Float]     = scalarSchema("Float", None, FloatValue)
  implicit val doubleSchema: Schema[Double]   = floatSchema.contramap(_.toFloat)
  implicit def optionSchema[A](implicit ev: Schema[A]): Schema[Option[A]] = new Typeclass[Option[A]] {
    override def optional: Boolean                        = true
    override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
    override def exec(
      value: Option[A],
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition],
      parallel: Boolean
    ): IO[ExecutionError, ResponseValue] = value match {
      case Some(value) => ev.exec(value, selectionSet, arguments, fragments, parallel)
      case None        => UIO(NullValue)
    }
  }
  implicit def listSchema[A](implicit ev: Schema[A]): Schema[List[A]] = new Typeclass[List[A]] {
    override def toType(isInput: Boolean = false): __Type = {
      val t = ev.toType(isInput)
      makeList(if (ev.optional) t else makeNonNull(t))
    }
    override def exec(
      value: List[A],
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition],
      parallel: Boolean
    ): IO[ExecutionError, ResponseValue] =
      IO.collectAllPar(value.map(ev.exec(_, selectionSet, arguments, fragments, parallel))).map(ListValue)
  }
  implicit def setSchema[A](implicit ev: Schema[A]): Schema[Set[A]]           = listSchema[A].contramap(_.toList)
  implicit def functionUnitSchema[A](implicit ev: Schema[A]): Schema[() => A] = ev.contramap(_())
  implicit def tupleSchema[A, B](implicit ev1: Schema[A], ev2: Schema[B]): Schema[(A, B)] =
    new Typeclass[(A, B)] {
      override def toType(isInput: Boolean = false): __Type = {
        val typeA     = ev1.toType(isInput)
        val typeB     = ev2.toType(isInput)
        val typeAName = typeA.name.getOrElse("")
        val typeBName = typeB.name.getOrElse("")
        makeObject(
          Some(s"Tuple$typeAName$typeBName"),
          Some(s"A tuple of $typeAName and $typeBName"),
          List(
            __Field(
              "_1",
              Some("First element of the tuple"),
              Nil,
              () => if (ev1.optional) typeA else makeNonNull(typeA),
              isDeprecated = false,
              None
            ),
            __Field(
              "_2",
              Some("Second element of the tuple"),
              Nil,
              () => if (ev1.optional) typeB else makeNonNull(typeB),
              isDeprecated = false,
              None
            )
          )
        )
      }

      override def exec(
        value: (A, B),
        selectionSet: List[Selection],
        arguments: Map[String, Value],
        fragments: Map[String, FragmentDefinition],
        parallel: Boolean
      ): IO[ExecutionError, ResponseValue] = {
        val mergedSelectionSet = mergeSelectionSet(selectionSet, "", fragments)
        ZIO
          .foldLeft(mergedSelectionSet)(Vector.empty[(String, ResponseValue)]) {
            case (result, field) =>
              field.name match {
                case "_1" =>
                  ev1.exec(value._1, selectionSet, arguments, fragments, parallel).map(a => result :+ (field.name -> a))
                case "_2" =>
                  ev2.exec(value._2, selectionSet, arguments, fragments, parallel).map(a => result :+ (field.name -> a))
                case _ => IO.succeed(result)
              }
          }
          .map(fields => ObjectValue(fields.toList))
      }
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
        fragments: Map[String, FragmentDefinition],
        parallel: Boolean
      ): IO[ExecutionError, ResponseValue] =
        arg1.build(Right(arguments)) match {
          case Some(argValue) => ev2.exec(value(argValue), selectionSet, Map(), fragments, parallel)
          case None           => IO.fail(ExecutionError(s"Failed to generate arguments from $arguments"))
        }
    }

  type Typeclass[T] = Schema[T]

  def combine[T](ctx: CaseClass[Schema, T]): Schema[T] = new Schema[T] {
    override def toType(isInput: Boolean = false): __Type =
      if (isInput)
        makeInputObject(
          Some(getName(ctx)),
          getDescription(ctx),
          ctx.parameters
            .map(
              p =>
                __InputValue(
                  p.label,
                  getDescription(p),
                  () =>
                    if (p.typeclass.optional) p.typeclass.toType(isInput) else makeNonNull(p.typeclass.toType(isInput)),
                  None
                )
            )
            .toList
        )
      else
        makeObject(
          Some(getName(ctx)),
          getDescription(ctx),
          ctx.parameters
            .map(
              p =>
                __Field(
                  p.label,
                  getDescription(p),
                  p.typeclass.arguments,
                  () =>
                    if (p.typeclass.optional) p.typeclass.toType(isInput) else makeNonNull(p.typeclass.toType(isInput)),
                  p.annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
                  p.annotations.collectFirst { case GQLDeprecated(reason) => reason }
                )
            )
            .toList
        )

    override def exec(
      value: T,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition],
      parallel: Boolean
    ): IO[ExecutionError, ResponseValue] =
      if (ctx.isObject) {
        UIO(ResponseValue.EnumValue(getName(ctx)))
      } else {
        val mergedSelectionSet = mergeSelectionSet(selectionSet, getName(ctx), fragments)
        val resolveFields = mergedSelectionSet.map {
          case Selection.Field(alias, name @ "__typename", _, _, _) =>
            UIO(alias.getOrElse(name) -> ResponseValue.StringValue(getName(ctx)))
          case Selection.Field(alias, name, args, _, selectionSet) =>
            ctx.parameters
              .find(_.label == name)
              .map(p => p.typeclass.exec(p.dereference(value), selectionSet, args, fragments, parallel))
              .getOrElse(UIO.succeed(NullValue))
              .map((alias.getOrElse(name), _))
        }
        (if (parallel) IO.collectAllPar(resolveFields) else IO.collectAll(resolveFields)).map(ObjectValue)
      }
  }

  def dispatch[T](ctx: SealedTrait[Schema, T]): Schema[T] = new Typeclass[T] {
    override def toType(isInput: Boolean = false): __Type = {
      val subtypes =
        ctx.subtypes.map(s => s.typeclass.toType(isInput) -> s.annotations).toList.sortBy(_._1.name.getOrElse(""))
      val isEnum = subtypes.forall {
        case (t, _) if t.fields(__DeprecatedArgs(Some(true))).forall(_.isEmpty) && t.inputFields.forall(_.isEmpty) =>
          true
        case _ => false
      }
      if (isEnum && subtypes.nonEmpty)
        makeEnum(
          Some(getName(ctx)),
          getDescription(ctx),
          subtypes.collect {
            case (__Type(_, Some(name), description, _, _, _, _, _, _), annotations) =>
              __EnumValue(
                name,
                description,
                annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
                annotations.collectFirst { case GQLDeprecated(reason) => reason }
              )
          }
        )
      else makeUnion(Some(getName(ctx)), getDescription(ctx), subtypes.map(_._1))
    }

    override def exec(
      value: T,
      selectionSet: List[Selection],
      arguments: Map[String, Value],
      fragments: Map[String, FragmentDefinition],
      parallel: Boolean
    ): IO[ExecutionError, ResponseValue] =
      ctx.dispatch(value)(
        subType => subType.typeclass.exec(subType.cast(value), selectionSet, arguments, fragments, parallel)
      )
  }

  private def getName(annotations: Seq[Any], typeName: TypeName): String =
    annotations.collectFirst { case GQLName(name) => name }.getOrElse(typeName.short)

  private def getName[Typeclass[_], Type](ctx: CaseClass[Typeclass, Type]): String =
    getName(ctx.annotations, ctx.typeName)

  private def getName[Typeclass[_], Type](ctx: SealedTrait[Typeclass, Type]): String =
    getName(ctx.annotations, ctx.typeName)

  private def getDescription(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLDescription(desc) => desc }

  private def getDescription[Typeclass[_], Type](ctx: CaseClass[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  private def getDescription[Typeclass[_], Type](ctx: SealedTrait[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  private def getDescription[Typeclass[_], Type](ctx: Param[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
