package caliban.schema

import java.util.UUID
import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.language.experimental.macros
import caliban.CalibanError.ExecutionError
import caliban.ResponseValue._
import caliban.Value._
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription, GQLDirective, GQLInputName, GQLInterface, GQLName }
import caliban.schema.Step._
import caliban.schema.Types._
import caliban.{ InputValue, ResponseValue }
import magnolia._
import zio.{ URIO, ZIO }
import zio.stream.ZStream
import zquery.ZQuery

/**
 * Typeclass that defines how to map the type `T` to the according GraphQL concepts: how to introspect it and how to resolve it.
 * `R` is the ZIO environment required by the effects in the schema (`Any` if nothing required).
 */
@implicitNotFound(
  """Cannot find a Schema for type ${T}.
     Caliban derives a Schema automatically for basic Scala types, case classes and sealed traits, but
     you need to manually provide an implicit Schema for other types that could be nested in ${T}.
     If you use a custom type as an argument, you also need to provide an implicit ArgBuilder for that type.
     See https://ghostdogpr.github.io/caliban/docs/schema.html for more information.
"""
)
trait Schema[-R, T] { self =>

  /**
   * Generates a GraphQL type object from `T`.
   * @param isInput indicates if the type is passed as an argument. This is needed because GraphQL differentiates `InputType` from `ObjectType`.
   */
  def toType(isInput: Boolean = false): __Type

  /**
   * Resolves `T` by turning a value of type `T` into an execution step that describes how to resolve the value.
   * @param value a value of type `T`
   */
  def resolve(value: T): Step[R]

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
    override def resolve(value: A): Step[R]       = self.resolve(f(value))
  }
}

object Schema extends GenericSchema[Any]

trait GenericSchema[R] extends DerivationSchema[R] {

  /**
   * Creates a scalar schema for a type `A`
   * @param name name of the scalar type
   * @param description description of the scalar type
   * @param makeResponse function from `A` to [[ResponseValue]] that defines how to resolve `A`
   */
  def scalarSchema[A](name: String, description: Option[String], makeResponse: A => ResponseValue): Schema[Any, A] =
    new Schema[Any, A] {
      override def toType(isInput: Boolean): __Type = makeScalar(name, description)
      override def resolve(value: A): Step[Any]     = PureStep(makeResponse(value))
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
    fields: List[(__Field, A => Step[R1])],
    directives: List[Directive] = List.empty
  ): Schema[R1, A] =
    new Schema[R1, A] {

      override def toType(isInput: Boolean): __Type =
        if (isInput) {
          makeInputObject(Some(customizeInputTypeName(name)), description, fields.map {
            case (f, _) => __InputValue(f.name, f.description, f.`type`, None)
          })
        } else makeObject(Some(name), description, fields.map(_._1), directives)

      override def resolve(value: A): Step[R1] =
        ObjectStep(name, fields.map { case (f, plan) => f.name -> plan(value) }.toMap)
    }

  implicit val unitSchema: Schema[Any, Unit]             = scalarSchema("Unit", None, _ => ObjectValue(Nil))
  implicit val booleanSchema: Schema[Any, Boolean]       = scalarSchema("Boolean", None, BooleanValue)
  implicit val stringSchema: Schema[Any, String]         = scalarSchema("String", None, StringValue)
  implicit val uuidSchema: Schema[Any, UUID]             = scalarSchema("ID", None, uuid => StringValue(uuid.toString))
  implicit val intSchema: Schema[Any, Int]               = scalarSchema("Int", None, IntValue(_))
  implicit val longSchema: Schema[Any, Long]             = scalarSchema("Long", None, IntValue(_))
  implicit val bigIntSchema: Schema[Any, BigInt]         = scalarSchema("BigInt", None, IntValue(_))
  implicit val doubleSchema: Schema[Any, Double]         = scalarSchema("Float", None, FloatValue(_))
  implicit val floatSchema: Schema[Any, Float]           = scalarSchema("Float", None, FloatValue(_))
  implicit val bigDecimalSchema: Schema[Any, BigDecimal] = scalarSchema("BigDecimal", None, FloatValue(_))

  implicit def optionSchema[A](implicit ev: Schema[R, A]): Schema[R, Option[A]] = new Schema[R, Option[A]] {
    override def optional: Boolean                        = true
    override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)

    override def resolve(value: Option[A]): Step[R] =
      value match {
        case Some(value) => ev.resolve(value)
        case None        => NullStep
      }
  }
  implicit def listSchema[A](implicit ev: Schema[R, A]): Schema[R, List[A]] = new Schema[R, List[A]] {
    override def toType(isInput: Boolean = false): __Type = {
      val t = ev.toType(isInput)
      makeList(if (ev.optional) t else makeNonNull(t))
    }

    override def resolve(value: List[A]): Step[R] = ListStep(value.map(ev.resolve))
  }
  implicit def setSchema[A](implicit ev: Schema[R, A]): Schema[R, Set[A]]       = listSchema[A].contramap(_.toList)
  implicit def seqSchema[A](implicit ev: Schema[R, A]): Schema[R, Seq[A]]       = listSchema[A].contramap(_.toList)
  implicit def vectorSchema[A](implicit ev: Schema[R, A]): Schema[R, Vector[A]] = listSchema[A].contramap(_.toList)
  implicit def functionUnitSchema[A](implicit ev: Schema[R, A]): Schema[R, () => A] =
    new Schema[R, () => A] {
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(value: () => A): Step[R]         = FunctionStep(_ => ev.resolve(value()))
    }

  implicit def eitherSchema[RA, RB, A, B](
    implicit evA: Schema[RA, A],
    evB: Schema[RB, B]
  ): Schema[RA with RB, Either[A, B]] = {
    lazy val typeA: __Type       = evA.toType()
    lazy val typeB: __Type       = evB.toType()
    lazy val typeAName: String   = Types.name(typeA)
    lazy val typeBName: String   = Types.name(typeB)
    lazy val name: String        = s"Either${typeAName}Or$typeBName"
    lazy val description: String = s"Either $typeAName or $typeBName"

    objectSchema(
      name,
      Some(description),
      List(
        __Field("left", Some("Left element of the Either"), Nil, () => typeA) -> {
          case Left(value) => evA.resolve(value)
          case Right(_)    => NullStep
        },
        __Field("right", Some("Right element of the Either"), Nil, () => typeB) -> {
          case Left(_)      => NullStep
          case Right(value) => evB.resolve(value)
        }
      )
    )
  }
  implicit def tupleSchema[RA, RB, A, B](
    implicit evA: Schema[RA, A],
    evB: Schema[RB, B]
  ): Schema[RA with RB, (A, B)] = {
    lazy val typeA: __Type     = evA.toType()
    lazy val typeB: __Type     = evB.toType()
    lazy val typeAName: String = Types.name(typeA)
    lazy val typeBName: String = Types.name(typeB)

    objectSchema(
      s"Tuple${typeAName}And$typeBName",
      Some(s"A tuple of $typeAName and $typeBName"),
      List(
        __Field("_1", Some("First element of the tuple"), Nil, () => if (evA.optional) typeA else makeNonNull(typeA)) ->
          ((tuple: (A, B)) => evA.resolve(tuple._1)),
        __Field("_2", Some("Second element of the tuple"), Nil, () => if (evB.optional) typeB else makeNonNull(typeB)) ->
          ((tuple: (A, B)) => evB.resolve(tuple._2))
      )
    )
  }
  implicit def mapSchema[RA, RB, A, B](implicit evA: Schema[RA, A], evB: Schema[RB, B]): Schema[RA with RB, Map[A, B]] =
    new Schema[RA with RB, Map[A, B]] {
      lazy val typeA: __Type       = evA.toType()
      lazy val typeB: __Type       = evB.toType()
      lazy val typeAName: String   = Types.name(typeA)
      lazy val typeBName: String   = Types.name(typeB)
      lazy val name: String        = s"KV$typeAName$typeBName"
      lazy val description: String = s"A key-value pair of $typeAName and $typeBName"

      lazy val kvSchema: Schema[RA with RB, (A, B)] = objectSchema(
        name,
        Some(description),
        List(
          __Field("key", Some("Key"), Nil, () => if (evA.optional) typeA else makeNonNull(typeA))
            -> ((kv: (A, B)) => evA.resolve(kv._1)),
          __Field("value", Some("Value"), Nil, () => if (evB.optional) typeB else makeNonNull(typeB))
            -> ((kv: (A, B)) => evB.resolve(kv._2))
        )
      )

      override def toType(isInput: Boolean = false): __Type = makeList(makeNonNull(kvSchema.toType(isInput)))

      override def resolve(value: Map[A, B]): Step[RA with RB] = ListStep(value.toList.map(kvSchema.resolve))
    }
  implicit def functionSchema[RA, RB, A, B](
    implicit arg1: ArgBuilder[A],
    ev1: Schema[RA, A],
    ev2: Schema[RB, B]
  ): Schema[RA with RB, A => B] =
    new Schema[RA with RB, A => B] {
      override def arguments: List[__InputValue] = {
        val t = ev1.toType(true)
        t.inputFields.getOrElse(t.kind match {
          case __TypeKind.SCALAR | __TypeKind.ENUM | __TypeKind.LIST =>
            // argument was not wrapped in a case class, give it an arbitrary name
            List(__InputValue("value", None, () => if (ev1.optional) t else makeNonNull(t), None))
          case _ => Nil
        })
      }
      override def optional: Boolean                        = ev2.optional
      override def toType(isInput: Boolean = false): __Type = ev2.toType(isInput)

      override def resolve(f: A => B): Step[RA with RB] =
        FunctionStep(args =>
          arg1
            .build(InputValue.ObjectValue(args))
            .fold(error => args.get("value").fold[Either[ExecutionError, A]](Left(error))(arg1.build), Right(_))
            .fold(error => QueryStep(ZQuery.fail(error)), value => ev2.resolve(f(value)))
        )
    }
  implicit def futureSchema[A](implicit ev: Schema[R, A]): Schema[R, Future[A]] =
    effectSchema[R, R, Throwable, A].contramap[Future[A]](future => ZIO.fromFuture(_ => future))
  implicit def infallibleEffectSchema[R1 >: R, R2 >: R, A](implicit ev: Schema[R2, A]): Schema[R, URIO[R1, A]] =
    new Schema[R, URIO[R1, A]] {
      override def optional: Boolean                        = ev.optional
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(value: URIO[R1, A]): Step[R]     = QueryStep(ZQuery.fromEffect(value.map(ev.resolve)))
    }
  implicit def effectSchema[R1 >: R, R2 >: R, E <: Throwable, A](implicit ev: Schema[R2, A]): Schema[R, ZIO[R1, E, A]] =
    new Schema[R, ZIO[R1, E, A]] {
      override def optional: Boolean                        = true
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(value: ZIO[R1, E, A]): Step[R]   = QueryStep(ZQuery.fromEffect(value.map(ev.resolve)))
    }
  implicit def infallibleQuerySchema[R1 >: R, R2 >: R, A](
    implicit ev: Schema[R2, A]
  ): Schema[R, ZQuery[R1, Nothing, A]] =
    new Schema[R, ZQuery[R1, Nothing, A]] {
      override def optional: Boolean                               = ev.optional
      override def toType(isInput: Boolean = false): __Type        = ev.toType(isInput)
      override def resolve(value: ZQuery[R1, Nothing, A]): Step[R] = QueryStep(value.map(ev.resolve))
    }
  implicit def querySchema[R1 >: R, R2 >: R, E <: Throwable, A](
    implicit ev: Schema[R2, A]
  ): Schema[R, ZQuery[R1, E, A]] =
    new Schema[R, ZQuery[R1, E, A]] {
      override def optional: Boolean                         = true
      override def toType(isInput: Boolean): __Type          = ev.toType(isInput)
      override def resolve(value: ZQuery[R1, E, A]): Step[R] = QueryStep(value.map(ev.resolve))
    }
  implicit def streamSchema[R1 >: R, R2 >: R, E <: Throwable, A](
    implicit ev: Schema[R2, A]
  ): Schema[R, ZStream[R1, E, A]] =
    new Schema[R, ZStream[R1, E, A]] {
      override def optional: Boolean                          = ev.optional
      override def toType(isInput: Boolean = false): __Type   = ev.toType(isInput)
      override def resolve(value: ZStream[R1, E, A]): Step[R] = StreamStep(value.map(ev.resolve))
    }

}

trait DerivationSchema[R] {

  /**
   * Default naming logic for input types.
   * This is needed to avoid a name clash between a type used as an input and the same type used as an output.
   * GraphQL needs 2 different types, and they can't have the same name.
   * By default, we add the "Input" suffix after the type name.
   */
  def customizeInputTypeName(name: String): String = s"${name}Input"

  type Typeclass[T] = Schema[R, T]

  def combine[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def toType(isInput: Boolean = false): __Type =
      if (isInput)
        makeInputObject(
          Some(ctx.annotations.collectFirst { case GQLInputName(suffix) => suffix }
            .getOrElse(customizeInputTypeName(getName(ctx)))),
          getDescription(ctx),
          ctx.parameters
            .map(p =>
              __InputValue(
                p.label,
                getDescription(p),
                () =>
                  if (p.typeclass.optional) p.typeclass.toType(isInput) else makeNonNull(p.typeclass.toType(isInput)),
                None,
                Some(p.annotations.collect { case GQLDirective(dir) => dir }.toList).filter(_.nonEmpty)
              )
            )
            .toList,
          Some(ctx.typeName.full)
        )
      else
        makeObject(
          Some(getName(ctx)),
          getDescription(ctx),
          ctx.parameters
            .map(p =>
              __Field(
                p.label,
                getDescription(p),
                p.typeclass.arguments,
                () =>
                  if (p.typeclass.optional) p.typeclass.toType(isInput) else makeNonNull(p.typeclass.toType(isInput)),
                p.annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
                p.annotations.collectFirst { case GQLDeprecated(reason) => reason },
                Option(p.annotations.collect { case GQLDirective(dir) => dir }.toList).filter(_.nonEmpty)
              )
            )
            .toList,
          getDirectives(ctx),
          Some(ctx.typeName.full)
        )

    override def resolve(value: T): Step[R] =
      if (ctx.isObject) PureStep(EnumValue(getName(ctx)))
      else ObjectStep(getName(ctx), ctx.parameters.map(p => p.label -> p.typeclass.resolve(p.dereference(value))).toMap)
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def toType(isInput: Boolean = false): __Type = {
      val subtypes =
        ctx.subtypes
          .map(s => s.typeclass.toType() -> s.annotations)
          .toList
          .sortBy {
            case (tpe, _) => tpe.name.getOrElse("")
          }
      val isEnum = subtypes.forall {
        case (t, _)
            if t.fields(__DeprecatedArgs(Some(true))).forall(_.isEmpty)
              && t.inputFields.forall(_.isEmpty) =>
          true
        case _ => false
      }
      if (isEnum && subtypes.nonEmpty)
        makeEnum(
          Some(getName(ctx)),
          getDescription(ctx),
          subtypes.collect {
            case (__Type(_, Some(name), description, _, _, _, _, _, _, _, _), annotations) =>
              __EnumValue(
                name,
                description,
                annotations.collectFirst { case GQLDeprecated(_) => () }.isDefined,
                annotations.collectFirst { case GQLDeprecated(reason) => reason }
              )
          },
          Some(ctx.typeName.full)
        )
      else {
        ctx.annotations.collectFirst {
          case GQLInterface() => ()
        }.fold(
          makeUnion(
            Some(getName(ctx)),
            getDescription(ctx),
            subtypes.map { case (t, _) => fixEmptyUnionObject(t) },
            Some(ctx.typeName.full)
          )
        ) { _ =>
          val impl = subtypes.map(_._1.copy(interfaces = () => Some(List(toType(isInput)))))
          val commonFields = impl
            .flatMap(_.fields(__DeprecatedArgs(Some(true))))
            .flatten
            .groupBy(_.name)
            .collect {
              case (name, list)
                  if impl.forall(_.fields(__DeprecatedArgs(Some(true))).getOrElse(Nil).exists(_.name == name)) =>
                list.headOption
            }
            .flatten

          makeInterface(Some(getName(ctx)), getDescription(ctx), commonFields.toList, impl, Some(ctx.typeName.full))
        }
      }
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

    override def resolve(value: T): Step[R] =
      ctx.dispatch(value)(subType => subType.typeclass.resolve(subType.cast(value)))
  }

  private def getDirectives(annotations: Seq[Any]): List[Directive] =
    annotations.collect { case GQLDirective(dir) => dir }.toList

  private def getDirectives[Typeclass[_], Type](ctx: ReadOnlyCaseClass[Typeclass, Type]): List[Directive] =
    getDirectives(ctx.annotations)

  private def getName(annotations: Seq[Any], typeName: TypeName): String =
    annotations.collectFirst { case GQLName(name) => name }
      .getOrElse(typeName.short + typeName.typeArguments.map(_.short).mkString)

  private def getName[Typeclass[_], Type](ctx: ReadOnlyCaseClass[Typeclass, Type]): String =
    getName(ctx.annotations, ctx.typeName)

  private def getName[Typeclass[_], Type](ctx: SealedTrait[Typeclass, Type]): String =
    getName(ctx.annotations, ctx.typeName)

  private def getDescription(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case GQLDescription(desc) => desc }

  private def getDescription[Typeclass[_], Type](ctx: ReadOnlyCaseClass[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  private def getDescription[Typeclass[_], Type](ctx: SealedTrait[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  private def getDescription[Typeclass[_], Type](ctx: ReadOnlyParam[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
