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

  /**
   * Creates an object schema for a type `A`
   * @param name name of the type
   * @param description description of the type
   * @param fields list of fields with a type description and a resolver for each field
   */
  def objectSchema[R1, A](
    name: String,
    description: Option[String],
    fields: List[(__Field, A => ZIO[R1, ExecutionError, ResolvedValue])]
  ): Schema[R1, A] =
    new Schema[R1, A] {

      override def toType(isInput: Boolean): __Type =
        if (isInput) {
          makeInputObject(Some(name), description, fields.map {
            case (f, _) => __InputValue(f.name, f.description, f.`type`, None)
          })
        } else makeObject(Some(name), description, fields.map(_._1))

      override def resolve(value: A, arguments: Map[String, Value]): ZIO[R1, ExecutionError, ResolvedValue] =
        ZIO
          .environment[R1]
          .map(
            env =>
              ResolvedObjectValue(name, fields.map {
                case (f, resolver) => f.name -> ((_: Map[String, Value]) => resolver(value).provide(env))
              }.toMap)
          )
    }

  implicit val unitSchema: Schema[Any, Unit]       = scalarSchema("Unit", None, _ => ObjectValue(Nil))
  implicit val booleanSchema: Schema[Any, Boolean] = scalarSchema("Boolean", None, BooleanValue)
  implicit val stringSchema: Schema[Any, String]   = scalarSchema("String", None, StringValue)
  implicit val intSchema: Schema[Any, Int]         = scalarSchema("Int", None, i => IntValue(i.toLong))
  implicit val longSchema: Schema[Any, Long]       = scalarSchema("Long", None, IntValue)
  implicit val floatSchema: Schema[Any, Float]     = scalarSchema("Float", None, i => FloatValue(i.toDouble))
  implicit val doubleSchema: Schema[Any, Double]   = scalarSchema("Double", None, FloatValue)

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
  ): Schema[RA with RB, Either[A, B]] = {
    val typeA: __Type       = evA.toType()
    val typeB: __Type       = evB.toType()
    val typeAName: String   = Types.name(typeA)
    val typeBName: String   = Types.name(typeB)
    val name: String        = s"Either${typeAName}Or$typeBName"
    val description: String = s"Either $typeAName or $typeBName"

    objectSchema(
      name,
      Some(description),
      List(
        __Field("left", Some("Left element of the Either"), Nil, () => typeA) -> {
          case Left(value) => evA.resolve(value, Map())
          case Right(_)    => UIO(NullValue)
        },
        __Field("right", Some("Right element of the Either"), Nil, () => typeB) -> {
          case Left(_)      => UIO(NullValue)
          case Right(value) => evB.resolve(value, Map())
        }
      )
    )
  }
  implicit def tupleSchema[RA, RB, A, B](
    implicit evA: Schema[RA, A],
    evB: Schema[RB, B]
  ): Schema[RA with RB, (A, B)] = {
    val typeA: __Type     = evA.toType()
    val typeB: __Type     = evB.toType()
    val typeAName: String = Types.name(typeA)
    val typeBName: String = Types.name(typeB)

    objectSchema(
      s"Tuple${typeAName}And$typeBName",
      Some(s"A tuple of $typeAName and $typeBName"),
      List(
        __Field("_1", Some("First element of the tuple"), Nil, () => if (evA.optional) typeA else makeNonNull(typeA)) ->
          ((tuple: (A, B)) => evA.resolve(tuple._1, Map())),
        __Field("_2", Some("Second element of the tuple"), Nil, () => if (evB.optional) typeB else makeNonNull(typeB)) ->
          ((tuple: (A, B)) => evB.resolve(tuple._2, Map()))
      )
    )
  }
  implicit def mapSchema[RA, RB, A, B](implicit evA: Schema[RA, A], evB: Schema[RB, B]): Schema[RA with RB, Map[A, B]] =
    new Schema[RA with RB, Map[A, B]] {
      val typeA: __Type       = evA.toType()
      val typeB: __Type       = evB.toType()
      val typeAName: String   = Types.name(typeA)
      val typeBName: String   = Types.name(typeB)
      val name: String        = s"KV$typeAName$typeBName"
      val description: String = s"A key-value pair of $typeAName and $typeBName"

      val kvSchema: Schema[RA with RB, (A, B)] = objectSchema(
        name,
        Some(description),
        List(
          __Field("key", Some("Key"), Nil, () => if (evA.optional) typeA else makeNonNull(typeA))
            -> ((kv: (A, B)) => evA.resolve(kv._1, Map())),
          __Field("value", Some("Value"), Nil, () => if (evB.optional) typeB else makeNonNull(typeB))
            -> ((kv: (A, B)) => evB.resolve(kv._2, Map()))
        )
      )

      override def toType(isInput: Boolean = false): __Type = makeList(makeNonNull(kvSchema.toType(isInput)))

      override def resolve(
        value: Map[A, B],
        arguments: Map[String, Value]
      ): ZIO[RA with RB, ExecutionError, ResolvedValue] =
        ZIO
          .environment[RA with RB]
          .map(env => ResolvedListValue(value.map(kvSchema.resolve(_, Map()).provide(env)).toList))
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
      override def optional: Boolean                        = ev.optional
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(
        value: ZIO[R1, E, A],
        arguments: Map[String, Value]
      ): ZIO[R1, ExecutionError, ResolvedValue] =
        value.flatMap(ev.resolve(_, arguments)).mapError(GenericSchema.effectfulExecutionError)
    }
  implicit def streamSchema[R1 <: R, E <: Throwable, A](implicit ev: Schema[R, A]): Schema[R1, ZStream[R1, E, A]] =
    new Schema[R1, ZStream[R1, E, A]] {
      override def optional: Boolean                        = ev.optional
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
    annotations.collectFirst { case GQLName(name) => name }
      .getOrElse(typeName.short + typeName.typeArguments.map(_.short).mkString)

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

object GenericSchema {

  def effectfulExecutionError(e: Throwable): ExecutionError = e match {
    case e: ExecutionError => e
    case other             => ExecutionError("Caught error during execution of effectful field", Some(other))
  }
}
