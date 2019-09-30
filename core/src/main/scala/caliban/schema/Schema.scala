package caliban.schema

import scala.language.experimental.macros
import caliban.CalibanError.ExecutionError
import caliban.introspection.adt._
import caliban.parsing.adt.Value
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription, GQLName }
import caliban.schema.ResolvedValue.{ ResolvedListValue, ResolvedObjectValue }
import caliban.schema.ResponseValue._
import caliban.schema.Types._
import magnolia._
import zio.stream.ZStream
import zio.{ IO, Runtime, UIO, ZIO }

trait Schema[T] { self =>
  def optional: Boolean             = false
  def arguments: List[__InputValue] = Nil
  def toType(isInput: Boolean = false): __Type
  def resolve(value: T, arguments: Map[String, Value]): IO[ExecutionError, ResolvedValue]

  def contramap[A](f: A => T): Schema[A] = new Schema[A] {
    override def optional: Boolean                = self.optional
    override def arguments: List[__InputValue]    = self.arguments
    override def toType(isInput: Boolean): __Type = self.toType(isInput)
    override def resolve(value: A, arguments: Map[String, Value]): IO[ExecutionError, ResolvedValue] =
      self.resolve(f(value), arguments)
  }
}

object Schema {

  def scalarSchema[A](name: String, description: Option[String], makeResponse: A => ResponseValue): Schema[A] =
    new Schema[A] {
      override def toType(isInput: Boolean): __Type = makeScalar(name, description)
      override def resolve(value: A, arguments: Map[String, Value]): IO[ExecutionError, ResponseValue] =
        IO.succeed(makeResponse(value))
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
    override def resolve(value: Option[A], arguments: Map[String, Value]): IO[ExecutionError, ResolvedValue] =
      value match {
        case Some(value) => ev.resolve(value, arguments)
        case None        => UIO(NullValue)
      }
  }
  implicit def listSchema[A](implicit ev: Schema[A]): Schema[List[A]] = new Typeclass[List[A]] {
    override def toType(isInput: Boolean = false): __Type = {
      val t = ev.toType(isInput)
      makeList(if (ev.optional) t else makeNonNull(t))
    }
    override def resolve(value: List[A], arguments: Map[String, Value]): IO[ExecutionError, ResolvedValue] =
      UIO(ResolvedListValue(value.map(ev.resolve(_, arguments))))
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

      override def resolve(value: (A, B), arguments: Map[String, Value]): IO[ExecutionError, ResolvedValue] =
        UIO(
          ResolvedObjectValue(
            "",
            Map("_1" -> (_ => ev1.resolve(value._1, Map())), "_2" -> (_ => ev2.resolve(value._2, Map())))
          )
        )
    }
  implicit def functionSchema[A, B](implicit arg1: ArgBuilder[A], ev1: Schema[A], ev2: Schema[B]): Schema[A => B] =
    new Typeclass[A => B] {
      override def arguments: List[__InputValue]            = ev1.toType(true).inputFields.getOrElse(Nil)
      override def optional: Boolean                        = ev2.optional
      override def toType(isInput: Boolean = false): __Type = ev2.toType(isInput)
      override def resolve(value: A => B, arguments: Map[String, Value]): IO[ExecutionError, ResolvedValue] =
        arg1.build(Right(arguments)) match {
          case Some(argValue) => ev2.resolve(value(argValue), Map())
          case None           => IO.fail(ExecutionError(s"Failed to generate arguments from $arguments"))
        }
    }

  implicit def effectSchema[R, E <: Throwable, A](implicit ev: Schema[A], runtime: Runtime[R]): Schema[ZIO[R, E, A]] =
    new Schema[ZIO[R, E, A]] {
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(value: ZIO[R, E, A], arguments: Map[String, Value]): IO[ExecutionError, ResolvedValue] =
        value.flatMap(ev.resolve(_, arguments)).provide(runtime.Environment).mapError {
          case e: ExecutionError => e
          case other             => ExecutionError("Caught error during execution of effectful field", Some(other))
        }
    }

  implicit def streamSchema[R, E <: Throwable, A](
    implicit ev: Schema[A],
    runtime: Runtime[R]
  ): Schema[ZStream[R, E, A]] =
    new Schema[ZStream[R, E, A]] {
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(
        stream: ZStream[R, E, A],
        arguments: Map[String, Value]
      ): IO[ExecutionError, ResolvedValue] =
        IO.succeed(
          ResolvedValue.ResolvedStreamValue(stream.mapM(ev.resolve(_, arguments)).provide(runtime.Environment))
        )
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

    override def resolve(value: T, arguments: Map[String, Value]): IO[ExecutionError, ResolvedValue] =
      if (ctx.isObject) {
        UIO(ResponseValue.EnumValue(getName(ctx)))
      } else {
        UIO(
          ResolvedObjectValue(
            getName(ctx),
            ctx.parameters
              .map(p => p.label -> ((args: Map[String, Value]) => p.typeclass.resolve(p.dereference(value), args)))
              .toMap
          )
        )
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

    override def resolve(value: T, arguments: Map[String, Value]): IO[ExecutionError, ResolvedValue] =
      ctx.dispatch(value)(
        subType => subType.typeclass.resolve(subType.cast(value), arguments)
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
