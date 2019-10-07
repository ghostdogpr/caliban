package caliban.schema

import scala.language.experimental.macros
import caliban.CalibanError.ExecutionError
import caliban.introspection.adt._
import caliban.parsing.adt.Value
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription, GQLName }
import caliban.ResolvedValue.{ ResolvedListValue, ResolvedObjectValue }
import caliban.{ ResolvedValue, ResponseValue }
import caliban.ResponseValue._
import caliban.schema.Types._
import magnolia._
import zio.stream.ZStream
import zio.{ IO, UIO, ZIO }

/**
 * Typeclass that defines how to map the type `T` to the according GraphQL concepts: how to introspect it and how to resolve it.
 * `R` is the ZIO environment required by the effects in the schema (`Any` if nothing required).
 */
trait Schema[-R, T] { self =>

  /**
   * Generates a GraphQL type object from `T`.
   * @param isInput indicates if the type is passed as an argument. This is needed because GraphQL differentiates `InputType` from `ObjectType`.
   */
  def toType(isInput: Boolean = false): __Type

  /**
   * Resolves `T` by turning a value of type `T` into a valid GraphQL result of type [[ResolvedValue]].
   * @param value a value of type `T`
   * @param arguments argument values that might be required to resolve `T`
   */
  def resolve(value: T, arguments: Map[String, Value]): ZIO[R, ExecutionError, ResolvedValue]

  /**
   * Defines if the type is considered optional or non-null. Should be false except for `Option`.
   */
  def optional: Boolean = false

  /**
   * Defined the arguments of the given type. Should be empty except for `Function`.
   */
  def arguments: List[__InputValue] = Nil

  /**
   * Builds a new `Schema` of `A` from an existing `Schema` of `T` and a function from `A` to `T`.
   * @param f a function from `A` to `T`.
   */
  def contramap[A](f: A => T): Schema[R, A] = new Schema[R, A] {
    override def optional: Boolean                = self.optional
    override def arguments: List[__InputValue]    = self.arguments
    override def toType(isInput: Boolean): __Type = self.toType(isInput)
    override def resolve(value: A, arguments: Map[String, Value]): ZIO[R, ExecutionError, ResolvedValue] =
      self.resolve(f(value), arguments)
  }
}

object Schema extends GenericSchema[Any]

trait GenericSchema[R] {

  /**
   * Creates a scalar schema for a type `A`
   * @param name name of the scalar type
   * @param description description of the scalar type
   * @param makeResponse function from `A` to [[ResponseValue]] that defines how to resolve `A`
   */
  def scalarSchema[A](name: String, description: Option[String], makeResponse: A => ResponseValue): Schema[Any, A] =
    new Schema[Any, A] {
      override def toType(isInput: Boolean): __Type = makeScalar(name, description)
      override def resolve(value: A, arguments: Map[String, Value]): IO[ExecutionError, ResponseValue] =
        IO.succeed(makeResponse(value))
    }

  implicit val unitSchema: Schema[Any, Unit]       = scalarSchema("Unit", None, _ => ObjectValue(Nil))
  implicit val booleanSchema: Schema[Any, Boolean] = scalarSchema("Boolean", None, BooleanValue)
  implicit val stringSchema: Schema[Any, String]   = scalarSchema("String", None, StringValue)
  implicit val intSchema: Schema[Any, Int]         = scalarSchema("Int", None, IntValue)
  implicit val longSchema: Schema[Any, Long]       = scalarSchema("Long", None, LongValue)
  implicit val floatSchema: Schema[Any, Float]     = scalarSchema("Float", None, FloatValue)
  implicit val doubleSchema: Schema[Any, Double]   = scalarSchema("Double", None, DoubleValue)

  implicit def optionSchema[A](implicit ev: Schema[R, A]): Schema[R, Option[A]] = new Schema[R, Option[A]] {
    override def optional: Boolean                        = true
    override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
    override def resolve(value: Option[A], arguments: Map[String, Value]): ZIO[R, ExecutionError, ResolvedValue] =
      value match {
        case Some(value) => ev.resolve(value, arguments)
        case None        => UIO(NullValue)
      }
  }
  implicit def listSchema[A](implicit ev: Schema[R, A]): Schema[R, List[A]] = new Schema[R, List[A]] {
    override def toType(isInput: Boolean = false): __Type = {
      val t = ev.toType(isInput)
      makeList(if (ev.optional) t else makeNonNull(t))
    }
    override def resolve(value: List[A], arguments: Map[String, Value]): ZIO[R, ExecutionError, ResolvedValue] =
      ZIO.environment[R].map(env => ResolvedListValue(value.map(ev.resolve(_, arguments).provide(env))))
  }
  implicit def setSchema[A](implicit ev: Schema[R, A]): Schema[R, Set[A]]           = listSchema[A].contramap(_.toList)
  implicit def functionUnitSchema[A](implicit ev: Schema[R, A]): Schema[R, () => A] = ev.contramap(_())
  implicit def eitherSchema[RA, RB, A, B](
    implicit evA: Schema[RA, A],
    evB: Schema[RB, B]
  ): Schema[RA with RB, Either[A, B]] =
    new Schema[RA with RB, Either[A, B]] {
      override def toType(isInput: Boolean = false): __Type = {
        val typeA     = evA.toType(isInput)
        val typeB     = evB.toType(isInput)
        val typeAName = Types.name(typeA)
        val typeBName = Types.name(typeB)
        makeObject(
          Some(s"Either${typeAName}Or$typeBName"),
          Some(s"Either $typeAName or $typeBName"),
          List(
            __Field("left", Some("Left element of the Either"), Nil, () => typeA),
            __Field("right", Some("Right element of the Either"), Nil, () => typeB)
          )
        )
      }

      override def resolve(
        value: Either[A, B],
        arguments: Map[String, Value]
      ): ZIO[RA with RB, ExecutionError, ResolvedValue] =
        ZIO
          .environment[RA with RB]
          .map(
            env =>
              value match {
                case Left(value) =>
                  ResolvedObjectValue(
                    "",
                    Map(
                      "left"  -> (_ => evA.resolve(value, Map()).provide(env)),
                      "right" -> (_ => UIO(NullValue))
                    )
                  )
                case Right(value) =>
                  ResolvedObjectValue(
                    "",
                    Map(
                      "left"  -> (_ => UIO(NullValue)),
                      "right" -> (_ => evB.resolve(value, Map()).provide(env))
                    )
                  )
              }
          )
    }
  implicit def tupleSchema[RA, RB, A, B](implicit evA: Schema[RA, A], evB: Schema[RB, B]): Schema[RA with RB, (A, B)] =
    new Schema[RA with RB, (A, B)] {
      override def toType(isInput: Boolean = false): __Type = {
        val typeA     = evA.toType(isInput)
        val typeB     = evB.toType(isInput)
        val typeAName = Types.name(typeA)
        val typeBName = Types.name(typeB)
        makeObject(
          Some(s"Tuple${typeAName}And$typeBName"),
          Some(s"A tuple of $typeAName and $typeBName"),
          List(
            __Field(
              "_1",
              Some("First element of the tuple"),
              Nil,
              () => if (evA.optional) typeA else makeNonNull(typeA)
            ),
            __Field(
              "_2",
              Some("Second element of the tuple"),
              Nil,
              () => if (evB.optional) typeB else makeNonNull(typeB)
            )
          )
        )
      }

      override def resolve(
        value: (A, B),
        arguments: Map[String, Value]
      ): ZIO[RA with RB, ExecutionError, ResolvedValue] =
        ZIO
          .environment[RA with RB]
          .map(
            env =>
              ResolvedObjectValue(
                "",
                Map(
                  "_1" -> (_ => evA.resolve(value._1, Map()).provide(env)),
                  "_2" -> (_ => evB.resolve(value._2, Map()).provide(env))
                )
              )
          )
    }
  implicit def mapSchema[RA, RB, A, B](implicit evA: Schema[RA, A], evB: Schema[RB, B]): Schema[RA with RB, Map[A, B]] =
    new Schema[RA with RB, Map[A, B]] {
      override def toType(isInput: Boolean = false): __Type = {
        val typeA     = evA.toType(isInput)
        val typeB     = evB.toType(isInput)
        val typeAName = Types.name(typeA)
        val typeBName = Types.name(typeB)
        val kvType = makeObject(
          Some(s"KV$typeAName$typeBName"),
          Some(s"A key-value pair of $typeAName and $typeBName"),
          List(
            __Field("key", Some("Key"), Nil, () => if (evA.optional) typeA else makeNonNull(typeA)),
            __Field("value", Some("Value"), Nil, () => if (evB.optional) typeB else makeNonNull(typeB))
          )
        )
        makeList(makeNonNull(kvType))
      }

      override def resolve(
        value: Map[A, B],
        arguments: Map[String, Value]
      ): ZIO[RA with RB, ExecutionError, ResolvedValue] =
        ZIO
          .environment[RA with RB]
          .map(
            env =>
              ResolvedListValue(value.map {
                case (key, value) =>
                  ZIO.succeed(
                    ResolvedObjectValue(
                      "",
                      Map(
                        "key"   -> (_ => evA.resolve(key, Map()).provide(env)),
                        "value" -> (_ => evB.resolve(value, Map()).provide(env))
                      )
                    )
                  )
              }.toList)
          )
    }
  implicit def functionSchema[RA, RB, A, B](
    implicit arg1: ArgBuilder[A],
    ev1: Schema[RA, A],
    ev2: Schema[RB, B]
  ): Schema[RA with RB, A => B] =
    new Schema[RA with RB, A => B] {
      override def arguments: List[__InputValue]            = ev1.toType(true).inputFields.getOrElse(Nil)
      override def optional: Boolean                        = ev2.optional
      override def toType(isInput: Boolean = false): __Type = ev2.toType(isInput)
      override def resolve(
        value: A => B,
        arguments: Map[String, Value]
      ): ZIO[RA with RB, ExecutionError, ResolvedValue] =
        arg1.build(Value.ObjectValue(arguments)).flatMap(argValue => ev2.resolve(value(argValue), Map()))
    }
  implicit def effectSchema[R1 <: R, E <: Throwable, A](implicit ev: Schema[R, A]): Schema[R1, ZIO[R1, E, A]] =
    new Schema[R1, ZIO[R1, E, A]] {
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(
        value: ZIO[R1, E, A],
        arguments: Map[String, Value]
      ): ZIO[R1, ExecutionError, ResolvedValue] =
        value.flatMap(ev.resolve(_, arguments)).mapError {
          case e: ExecutionError => e
          case other             => ExecutionError("Caught error during execution of effectful field", Some(other))
        }
    }
  implicit def streamSchema[R1 <: R, E <: Throwable, A](implicit ev: Schema[R, A]): Schema[R1, ZStream[R1, E, A]] =
    new Schema[R1, ZStream[R1, E, A]] {
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(
        stream: ZStream[R1, E, A],
        arguments: Map[String, Value]
      ): ZIO[R1, ExecutionError, ResolvedValue] =
        ZIO
          .environment[R1]
          .map(env => ResolvedValue.ResolvedStreamValue(stream.mapM(ev.resolve(_, arguments)).provide(env)))
    }

  type Typeclass[T] = Schema[R, T]

  def combine[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
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

    override def resolve(value: T, arguments: Map[String, Value]): ZIO[R, ExecutionError, ResolvedValue] =
      if (ctx.isObject) {
        UIO(ResponseValue.EnumValue(getName(ctx)))
      } else {
        ZIO
          .environment[R]
          .map(
            env =>
              ResolvedObjectValue(
                getName(ctx),
                ctx.parameters
                  .map(
                    p =>
                      p.label -> (
                        (args: Map[String, Value]) => p.typeclass.resolve(p.dereference(value), args).provide(env)
                      )
                  )
                  .toMap
              )
          )
      }
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
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
      else
        makeUnion(
          Some(getName(ctx)),
          getDescription(ctx),
          subtypes.map { case (t, _) => fixEmptyUnionObject(t) }
        )
    }

    // see https://github.com/graphql/graphql-spec/issues/568
    private def fixEmptyUnionObject(t: __Type): __Type =
      t.fields(__DeprecatedArgs(Some(true))) match {
        case Some(Nil) =>
          t.copy(
            fields = (_: __DeprecatedArgs) =>
              Some(
                List(
                  __Field(
                    "_",
                    Some(
                      "Fake field because GraphQL does not support empty objects. Do not query, use __typename instead."
                    ),
                    Nil,
                    () => makeScalar("Boolean")
                  )
                )
              )
          )
        case _ => t
      }

    override def resolve(value: T, arguments: Map[String, Value]): ZIO[R, ExecutionError, ResolvedValue] =
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
