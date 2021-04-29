package caliban.schema

import java.time.{ Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZoneOffset, ZonedDateTime }
import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal
import java.util.UUID
import caliban.CalibanError.ExecutionError
import caliban.ResponseValue._
import caliban.Value._
import caliban.execution.Field
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import caliban.schema.Annotations._
import caliban.schema.Step._
import caliban.schema.Types._
import caliban.{ InputValue, ResponseValue }
import magnolia._
import zio.query.ZQuery
import zio.stream.ZStream
import zio.{ Chunk, URIO, ZIO }

import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.language.experimental.macros

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

  private lazy val asType: __Type             = toType()
  private lazy val asInputType: __Type        = toType(isInput = true)
  private lazy val asSubscriptionType: __Type = toType(isSubscription = true)

  /**
   * Generates a GraphQL type object from `T`.
   * Unlike `toType`, this function is optimized and will not re-generate the type at each call.
   * @param isInput indicates if the type is passed as an argument. This is needed because GraphQL differentiates `InputType` from `ObjectType`.
   * @param isSubscription indicates if the type is used in a subscription operation.
   *                       For example, ZStream gives a different GraphQL type depending whether it is used in a subscription or elsewhere.
   */
  final def toType_(isInput: Boolean = false, isSubscription: Boolean = false): __Type =
    if (isInput) asInputType else if (isSubscription) asSubscriptionType else asType

  /**
   * Generates a GraphQL type object from `T`.
   * @param isInput indicates if the type is passed as an argument. This is needed because GraphQL differentiates `InputType` from `ObjectType`.
   * @param isSubscription indicates if the type is used in a subscription operation.
   *                       For example, ZStream gives a different GraphQL type depending whether it is used in a subscription or elsewhere.
   */
  protected[this] def toType(isInput: Boolean = false, isSubscription: Boolean = false): __Type

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
    override def optional: Boolean                                         = self.optional
    override def arguments: List[__InputValue]                             = self.arguments
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = self.toType_(isInput, isSubscription)
    override def resolve(value: A): Step[R]                                = self.resolve(f(value))
  }

  /**
   * Changes the name of the generated graphql type.
   * @param name new name for the type
   * @param inputName new name for the type when it's an input type (by default "Input" is added after the name)
   */
  def rename(name: String, inputName: Option[String] = None): Schema[R, T] = new Schema[R, T] {
    override def optional: Boolean             = self.optional
    override def arguments: List[__InputValue] = self.arguments
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
      val newName = if (isInput) inputName.getOrElse(Schema.customizeInputTypeName(name)) else name
      self.toType_(isInput, isSubscription).copy(name = Some(newName))
    }

    lazy val renameTypename: Boolean = self.toType().kind match {
      case __TypeKind.UNION | __TypeKind.ENUM | __TypeKind.INTERFACE => false
      case _                                                         => true
    }

    override def resolve(value: T): Step[R] =
      self.resolve(value) match {
        case o @ ObjectStep(_, fields)  =>
          if (renameTypename) ObjectStep(name, fields) else o
        case p @ PureStep(EnumValue(_)) =>
          if (renameTypename) PureStep(EnumValue(name)) else p
        case other                      =>
          other
      }
  }
}

object Schema extends GenericSchema[Any]

trait GenericSchema[R] extends DerivationSchema[R] with TemporalSchema {

  /**
   * Creates a scalar schema for a type `A`
   * @param name name of the scalar type
   * @param description description of the scalar type
   * @param makeResponse function from `A` to [[ResponseValue]] that defines how to resolve `A`
   */
  def scalarSchema[A](name: String, description: Option[String], makeResponse: A => ResponseValue): Schema[Any, A] =
    new Schema[Any, A] {
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = makeScalar(name, description)
      override def resolve(value: A): Step[Any]                              = PureStep(makeResponse(value))
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
    fields: (Boolean, Boolean) => List[(__Field, A => Step[R1])],
    directives: List[Directive] = List.empty
  ): Schema[R1, A] =
    new Schema[R1, A] {

      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        if (isInput) {
          makeInputObject(
            Some(customizeInputTypeName(name)),
            description,
            fields(isInput, isSubscription).map { case (f, _) =>
              __InputValue(f.name, f.description, f.`type`, None)
            }
          )
        } else makeObject(Some(name), description, fields(isInput, isSubscription).map(_._1), directives)

      override def resolve(value: A): Step[R1] =
        ObjectStep(name, fields(false, false).map { case (f, plan) => f.name -> plan(value) }.toMap)
    }

  def field[V](
    name: String,
    description: Option[String] = None,
    directives: List[Directive] = List.empty
  ): PartiallyAppliedField[V] =
    PartiallyAppliedField[V](name, description, directives)

  def fieldWithArgs[V, A](
    name: String,
    description: Option[String] = None,
    directives: List[Directive] = Nil
  ): PartiallyAppliedFieldWithArgs[V, A] =
    PartiallyAppliedFieldWithArgs[V, A](name, description, directives)

  /**
   * Creates a new hand-rolled schema. For normal usage use the derived schemas, this is primarily for schemas
   * which can't be resolved by derivation.
   * @param name The name of the type
   * @param description An optional description of the type
   * @param directives The directives to add to the type
   * @param fields The fields to add to this object
   *
   * {{{
   *  case class Group(id: String, users: UQuery[List[User]], parent: UQuery[Option[Group]], organization: UQuery[Organization])
   *  case class Organization(id: String, groups: UQuery[List[Group]])
   *  case class User(id: String, group: UQuery[Group])
   *
   *  implicit val groupSchema: Schema[Any, Group] = obj("Group", Some("A group of users"))(implicit ft =>
   *    List(
   *      field("id")(_.id),
   *      field("users")(_.users),
   *      field("parent")(_.parent),
   *      field("organization")(_.organization)
   *    )
   *  )
   *
   *  implicit val orgSchema: Schema[Any, Organization] = obj("Organization", Some("An organization of groups"))(implicit ft =>
   *    List(
   *      field("id")(_.id),
   *      field("groups")(_.groups)
   *    )
   *  )
   *
   *  implicit val userSchema: Schema[Any, User] = obj("User", Some("A user of the service"))(implicit ft =>
   *    List(
   *      field("id")(_.id),
   *      field("group")(_.group)
   *    )
   *  )
   *
   * }}}
   */
  def obj[R1, V](name: String, description: Option[String] = None, directives: List[Directive] = Nil)(
    fields: FieldAttributes => List[(__Field, V => Step[R1])]
  ): Schema[R1, V] =
    objectSchema(
      name,
      description,
      fields = (isInput, isSubscription) => fields(FieldAttributes(isInput = isInput, isSubscription = isSubscription)),
      directives
    )

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

  implicit def optionSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, Option[A]]                                  = new Schema[R0, Option[A]] {
    override def optional: Boolean                                         = true
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)

    override def resolve(value: Option[A]): Step[R0] =
      value match {
        case Some(value) => ev.resolve(value)
        case None        => NullStep
      }
  }
  implicit def listSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, List[A]]                                      = new Schema[R0, List[A]] {
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
      val t = ev.toType_(isInput, isSubscription)
      makeList(if (ev.optional) t else makeNonNull(t))
    }

    override def resolve(value: List[A]): Step[R0] = ListStep(value.map(ev.resolve))
  }
  implicit def setSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, Set[A]]                                        = listSchema[R0, A].contramap(_.toList)
  implicit def seqSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, Seq[A]]                                        = listSchema[R0, A].contramap(_.toList)
  implicit def vectorSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, Vector[A]]                                  =
    listSchema[R0, A].contramap(_.toList)
  implicit def chunkSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, Chunk[A]]                                    =
    listSchema[R0, A].contramap(_.toList)
  implicit def functionUnitSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, () => A]                              =
    new Schema[R0, () => A] {
      override def optional: Boolean                                         = ev.optional
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: () => A): Step[R0]                         = FunctionStep(_ => ev.resolve(value()))
    }
  implicit def metadataFunctionSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, Field => A]                       =
    new Schema[R0, Field => A] {
      override def arguments: List[__InputValue]                             = ev.arguments
      override def optional: Boolean                                         = ev.optional
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: Field => A): Step[R0]                      = MetadataFunctionStep(field => ev.resolve(value(field)))
    }

  implicit def eitherSchema[RA, RB, A, B](implicit
    evA: Schema[RA, A],
    evB: Schema[RB, B]
  ): Schema[RA with RB, Either[A, B]] = {
    lazy val typeAName: String   = Types.name(evA.toType_())
    lazy val typeBName: String   = Types.name(evB.toType_())
    lazy val name: String        = s"Either${typeAName}Or$typeBName"
    lazy val description: String = s"Either $typeAName or $typeBName"

    implicit val leftSchema: Schema[RA, A]  = new Schema[RA, A] {
      override def optional: Boolean                                         = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = evA.toType_(isInput, isSubscription)
      override def resolve(value: A): Step[RA]                               = evA.resolve(value)
    }
    implicit val rightSchema: Schema[RB, B] = new Schema[RB, B] {
      override def optional: Boolean                                         = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = evB.toType_(isInput, isSubscription)
      override def resolve(value: B): Step[RB]                               = evB.resolve(value)
    }

    obj[RA with RB, Either[A, B]](name, Some(description))(implicit ft =>
      List(
        field[Either[A, B]]("left", Some("Left element of the Either"))
          .either[RA, A](_.map(_ => NullStep))(leftSchema, ft),
        field[Either[A, B]]("right", Some("Right element of the Either"))
          .either[RB, B](_.swap.map(_ => NullStep))(rightSchema, ft)
      )
    )
  }
  implicit def tupleSchema[RA, RB, A, B](implicit
    evA: Schema[RA, A],
    evB: Schema[RB, B]
  ): Schema[RA with RB, (A, B)] = {
    lazy val typeAName: String = Types.name(evA.toType_())
    lazy val typeBName: String = Types.name(evB.toType_())

    obj[RA with RB, (A, B)](
      s"Tuple${typeAName}And$typeBName",
      Some(s"A tuple of $typeAName and $typeBName")
    )(implicit ft =>
      List(
        field[(A, B)]("_1", Some("First element of the tuple"))(_._1),
        field[(A, B)]("_2", Some("Second element of the tuple"))(_._2)
      )
    )
  }
  implicit def mapSchema[RA, RB, A, B](implicit evA: Schema[RA, A], evB: Schema[RB, B]): Schema[RA with RB, Map[A, B]] =
    new Schema[RA with RB, Map[A, B]] {
      lazy val typeAName: String   = Types.name(evA.toType_())
      lazy val typeBName: String   = Types.name(evB.toType_())
      lazy val name: String        = s"KV$typeAName$typeBName"
      lazy val description: String = s"A key-value pair of $typeAName and $typeBName"

      lazy val kvSchema: Schema[RA with RB, (A, B)] = obj[RA with RB, (A, B)](name, Some(description))(implicit ft =>
        List(
          field("key", Some("Key"))(_._1),
          field("value", Some("Value"))(_._2)
        )
      )

      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        makeList(makeNonNull(kvSchema.toType_(isInput, isSubscription)))

      override def resolve(value: Map[A, B]): Step[RA with RB] = ListStep(value.toList.map(kvSchema.resolve))
    }
  implicit def functionSchema[RA, RB, A, B](implicit
    arg1: ArgBuilder[A],
    ev1: Schema[RA, A],
    ev2: Schema[RB, B]
  ): Schema[RA with RB, A => B]                                                                                        =
    new Schema[RA with RB, A => B] {
      override def arguments: List[__InputValue] = {
        val t = ev1.toType_(true)
        t.inputFields.getOrElse(t.kind match {
          case __TypeKind.SCALAR | __TypeKind.ENUM | __TypeKind.LIST =>
            // argument was not wrapped in a case class, give it an arbitrary name
            List(__InputValue("value", None, () => if (ev1.optional) t else makeNonNull(t), None))
          case _                                                     => Nil
        })
      }
      override def optional: Boolean                                         = ev2.optional
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev2.toType_(isInput, isSubscription)

      override def resolve(f: A => B): Step[RA with RB] =
        FunctionStep(args =>
          arg1
            .build(InputValue.ObjectValue(args))
            .fold(error => args.get("value").fold[Either[ExecutionError, A]](Left(error))(arg1.build), Right(_))
            .fold(error => QueryStep(ZQuery.fail(error)), value => ev2.resolve(f(value)))
        )
    }

  implicit def futureSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, Future[A]]                                 =
    effectSchema[R0, R0, R0, Throwable, A].contramap[Future[A]](future => ZIO.fromFuture(_ => future))
  implicit def infallibleEffectSchema[R0, R1 >: R0, R2 >: R0, A](implicit ev: Schema[R2, A]): Schema[R0, URIO[R1, A]] =
    new Schema[R0, URIO[R1, A]] {
      override def optional: Boolean                                         = ev.optional
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: URIO[R1, A]): Step[R0]                     = QueryStep(ZQuery.fromEffect(value.map(ev.resolve)))
    }
  implicit def effectSchema[R0, R1 >: R0, R2 >: R0, E <: Throwable, A](implicit
    ev: Schema[R2, A]
  ): Schema[R0, ZIO[R1, E, A]]                                                                                        =
    new Schema[R0, ZIO[R1, E, A]] {
      override def optional: Boolean                                         = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: ZIO[R1, E, A]): Step[R0]                   = QueryStep(ZQuery.fromEffect(value.map(ev.resolve)))
    }
  implicit def infallibleQuerySchema[R0, R1 >: R0, R2 >: R0, A](implicit
    ev: Schema[R2, A]
  ): Schema[R0, ZQuery[R1, Nothing, A]]                                                                               =
    new Schema[R0, ZQuery[R1, Nothing, A]] {
      override def optional: Boolean                                         = ev.optional
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: ZQuery[R1, Nothing, A]): Step[R0]          = QueryStep(value.map(ev.resolve))
    }
  implicit def querySchema[R0, R1 >: R0, R2 >: R0, E <: Throwable, A](implicit
    ev: Schema[R2, A]
  ): Schema[R0, ZQuery[R1, E, A]]                                                                                     =
    new Schema[R0, ZQuery[R1, E, A]] {
      override def optional: Boolean                                         = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: ZQuery[R1, E, A]): Step[R0]                = QueryStep(value.map(ev.resolve))
    }
  implicit def infallibleStreamSchema[R1, R2 >: R1, A](implicit
    ev: Schema[R2, A]
  ): Schema[R1, ZStream[R1, Nothing, A]]                                                                              =
    new Schema[R1, ZStream[R1, Nothing, A]] {
      override def optional: Boolean                                 = false
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
        val t = ev.toType_(isInput, isSubscription)
        if (isSubscription) t else makeList(if (ev.optional) t else makeNonNull(t))
      }
      override def resolve(value: ZStream[R1, Nothing, A]): Step[R1] = StreamStep(value.map(ev.resolve))
    }
  implicit def streamSchema[R0, R1 >: R0, R2 >: R0, E <: Throwable, A](implicit
    ev: Schema[R2, A]
  ): Schema[R0, ZStream[R1, E, A]]                                                                                    =
    new Schema[R0, ZStream[R1, E, A]] {
      override def optional: Boolean                           = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
        val t = ev.toType_(isInput, isSubscription)
        if (isSubscription) t else makeList(if (ev.optional) t else makeNonNull(t))
      }
      override def resolve(value: ZStream[R1, E, A]): Step[R0] = StreamStep(value.map(ev.resolve))
    }

}

trait DerivationSchema[R] extends LowPriorityDerivedSchema {

  /**
   * Default naming logic for input types.
   * This is needed to avoid a name clash between a type used as an input and the same type used as an output.
   * GraphQL needs 2 different types, and they can't have the same name.
   * By default, we add the "Input" suffix after the type name.
   */
  def customizeInputTypeName(name: String): String = s"${name}Input"

  type Typeclass[T] = Schema[R, T]

  def combine[T](ctx: ReadOnlyCaseClass[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
      if (ctx.isValueClass && ctx.parameters.nonEmpty) ctx.parameters.head.typeclass.toType_(isInput, isSubscription)
      else if (isInput)
        makeInputObject(
          Some(ctx.annotations.collectFirst { case GQLInputName(suffix) => suffix }
            .getOrElse(customizeInputTypeName(getName(ctx)))),
          getDescription(ctx),
          ctx.parameters
            .map(p =>
              __InputValue(
                getName(p),
                getDescription(p),
                () =>
                  if (p.typeclass.optional) p.typeclass.toType_(isInput, isSubscription)
                  else makeNonNull(p.typeclass.toType_(isInput, isSubscription)),
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
                getName(p),
                getDescription(p),
                p.typeclass.arguments,
                () =>
                  if (p.typeclass.optional) p.typeclass.toType_(isInput, isSubscription)
                  else makeNonNull(p.typeclass.toType_(isInput, isSubscription)),
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
      else if (ctx.isValueClass && ctx.parameters.nonEmpty) {
        val head = ctx.parameters.head
        head.typeclass.resolve(head.dereference(value))
      } else {
        val fields = Map.newBuilder[String, Step[R]]
        ctx.parameters.foreach(p => fields += getName(p) -> p.typeclass.resolve(p.dereference(value)))
        ObjectStep(getName(ctx), fields.result())
      }
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new Typeclass[T] {
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
      val subtypes =
        ctx.subtypes
          .map(s => s.typeclass.toType_(isInput = false, isSubscription = false) -> s.annotations)
          .toList
          .sortBy { case (tpe, _) =>
            tpe.name.getOrElse("")
          }
      val isEnum   = subtypes.forall {
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
          subtypes.collect { case (__Type(_, Some(name), description, _, _, _, _, _, _, _, _), annotations) =>
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
        ctx.annotations.collectFirst { case GQLInterface() =>
          ()
        }.fold(
          makeUnion(
            Some(getName(ctx)),
            getDescription(ctx),
            subtypes.map { case (t, _) => fixEmptyUnionObject(t) },
            Some(ctx.typeName.full)
          )
        ) { _ =>
          val impl         = subtypes.map(_._1.copy(interfaces = () => Some(List(toType(isInput, isSubscription)))))
          val commonFields = () =>
            impl
              .flatMap(_.fields(__DeprecatedArgs(Some(true))))
              .flatten
              .groupBy(_.name)
              .collect {
                case (name, list)
                    if impl.forall(_.fields(__DeprecatedArgs(Some(true))).getOrElse(Nil).exists(_.name == name)) &&
                      list.map(t => Types.name(t.`type`())).distinct.length == 1 =>
                  list.headOption
              }
              .flatten
              .toList

          makeInterface(Some(getName(ctx)), getDescription(ctx), commonFields, impl, Some(ctx.typeName.full))
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
        case _         => t
      }

    override def resolve(value: T): Step[R] =
      ctx.dispatch(value)(subType => subType.typeclass.resolve(subType.cast(value)))
  }

  private def getDirectives(annotations: Seq[Any]): List[Directive]                                       =
    annotations.collect { case GQLDirective(dir) => dir }.toList

  private def getDirectives[Typeclass[_], Type](ctx: ReadOnlyCaseClass[Typeclass, Type]): List[Directive] =
    getDirectives(ctx.annotations)

  private def getName(annotations: Seq[Any], typeName: TypeName): String =
    annotations.collectFirst { case GQLName(name) => name }.getOrElse {
      typeName.typeArguments match {
        case Nil  => typeName.short
        case args => typeName.short + args.map(getName(Nil, _)).mkString
      }
    }

  private def getName[Typeclass[_], Type](ctx: ReadOnlyCaseClass[Typeclass, Type]): String =
    getName(ctx.annotations, ctx.typeName)

  private def getName[Typeclass[_], Type](ctx: SealedTrait[Typeclass, Type]): String =
    getName(ctx.annotations, ctx.typeName)

  private def getName[Typeclass[_], Type](ctx: ReadOnlyParam[Typeclass, Type]): String                    =
    ctx.annotations.collectFirst { case GQLName(name) => name }.getOrElse(ctx.label)

  private def getDescription(annotations: Seq[Any]): Option[String]                                       =
    annotations.collectFirst { case GQLDescription(desc) => desc }

  private def getDescription[Typeclass[_], Type](ctx: ReadOnlyCaseClass[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  private def getDescription[Typeclass[_], Type](ctx: SealedTrait[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  private def getDescription[Typeclass[_], Type](ctx: ReadOnlyParam[Typeclass, Type]): Option[String] =
    getDescription(ctx.annotations)

  /**
   * Generates an instance of `Schema` for the given type T.
   * This should be used only if T is a case class or a sealed trait.
   */
  implicit def genMacro[T]: Derived[Typeclass[T]] = macro DerivedMagnolia.derivedMagnolia[Typeclass, T]

  /**
   * Returns an instance of `Schema` for the given type T.
   * For a case class or sealed trait, you can call `genMacro[T].schema` instead to get more details if the
   * schema can't be derived.
   */
  def gen[T](implicit derived: Derived[Schema[R, T]]): Schema[R, T] = derived.schema

}

private[schema] trait LowPriorityDerivedSchema {
  implicit def derivedSchema[R, T](implicit derived: Derived[Schema[R, T]]): Schema[R, T] = derived.schema
}

trait TemporalSchema {

  private[schema] abstract class TemporalSchema[T <: Temporal](
    name: String,
    description: Option[String]
  ) extends Schema[Any, T] {

    protected def format(temporal: T): ResponseValue

    override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
      makeScalar(name, description)

    override def resolve(value: T): Step[Any] =
      PureStep(format(value))
  }

  def temporalSchema[A <: Temporal](name: String, description: Option[String] = None)(
    f: A => ResponseValue
  ): Schema[Any, A] = new TemporalSchema[A](name, description) {
    override protected def format(temporal: A): ResponseValue = f(temporal)
  }

  def temporalSchemaWithFormatter[A <: Temporal](name: String, description: Option[String] = None)(
    formatter: DateTimeFormatter
  ): Schema[Any, A] =
    temporalSchema[A](name, description)(a => StringValue(formatter.format(a)))

  implicit lazy val instantSchema: Schema[Any, Instant] =
    temporalSchema(
      "Instant",
      Some("An instantaneous point on the time-line represented by a standard date time string")
    )(a => StringValue(a.toString))

  lazy val instantEpochSchema: Schema[Any, Instant] =
    temporalSchema(
      "Instant",
      Some("An instantaneous point on the time-line represented by a numerical millisecond since epoch")
    )(a => IntValue.LongNumber(a.toEpochMilli))

  implicit lazy val localDateTimeSchema: Schema[Any, LocalDateTime] =
    localDateTimeSchemaWithFormatter(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

  def localDateTimeSchemaWithFormatter(formatter: DateTimeFormatter): Schema[Any, LocalDateTime] =
    temporalSchemaWithFormatter(
      "LocalDateTime",
      Some(s"A date-time without a time-zone in the ISO-8601 calendar system in the format of $formatter")
    )(formatter)

  implicit lazy val offsetDateTimeSchema: Schema[Any, OffsetDateTime] =
    offsetDateTimeSchemaWithFormatter(DateTimeFormatter.ISO_OFFSET_DATE_TIME)

  def offsetDateTimeSchemaWithFormatter(formatter: DateTimeFormatter): Schema[Any, OffsetDateTime] =
    temporalSchemaWithFormatter(
      "OffsetDateTime",
      Some(s"A date-time with an offset from UTC/Greenwich in the ISO-8601 calendar system using the format $formatter")
    )(formatter)

  implicit lazy val zonedDateTimeSchema: Schema[Any, ZonedDateTime] =
    zonedDateTimeSchemaWithFormatter(DateTimeFormatter.ISO_ZONED_DATE_TIME)

  val localDateTimeEpochSchema: Schema[Any, LocalDateTime] =
    temporalSchema("LocalDateTime")(a => IntValue.LongNumber(a.toInstant(ZoneOffset.UTC).toEpochMilli))

  def zonedDateTimeSchemaWithFormatter(formatter: DateTimeFormatter): Schema[Any, ZonedDateTime] =
    temporalSchemaWithFormatter(
      "ZonedDateTime",
      Some(s"A date-time with a time-zone in the ISO-8601 calendar system using the format $formatter")
    )(formatter)

  implicit lazy val localDateSchema: Schema[Any, LocalDate] =
    localDateSchemaWithFormatter(DateTimeFormatter.ISO_LOCAL_DATE)

  val localDateEpochSchema: Schema[Any, LocalDate] =
    temporalSchema(
      "LocalDate",
      Some("A date without a time-zone in the ISO-8601 calendar system represented as a millisecond value since epoch")
    )(a => IntValue.LongNumber(a.atStartOfDay.toInstant(ZoneOffset.UTC).toEpochMilli))

  def localDateSchemaWithFormatter(formatter: DateTimeFormatter): Schema[Any, LocalDate] =
    temporalSchemaWithFormatter(
      "LocalDate",
      Some(s"A date without a time-zone in the ISO-8601 calendar system using the format $formatter")
    )(formatter)

  implicit lazy val localTimeSchema: Schema[Any, LocalTime] =
    localTimeSchemaWithFormatter(DateTimeFormatter.ISO_LOCAL_TIME)

  def localTimeSchemaWithFormatter(formatter: DateTimeFormatter): Schema[Any, LocalTime] =
    temporalSchemaWithFormatter(
      "LocalTime",
      Some(s"A time without a time-zone in the ISO-8601 calendar system using the format $formatter")
    )(formatter)

}

case class FieldAttributes(isInput: Boolean, isSubscription: Boolean)

case class PartiallyAppliedField[V](
  name: String,
  description: Option[String],
  directives: List[Directive]
) {
  def apply[R, V1](fn: V => V1)(implicit ev: Schema[R, V1], ft: FieldAttributes): (__Field, V => Step[R]) =
    either[R, V1](v => Left(fn(v)))(ev, ft)

  def either[R, V1](
    fn: V => Either[V1, Step[R]]
  )(implicit ev: Schema[R, V1], ft: FieldAttributes): (__Field, V => Step[R]) =
    (
      __Field(
        name,
        description,
        Nil,
        () =>
          if (ev.optional) ev.toType_(ft.isInput, ft.isSubscription)
          else Types.makeNonNull(ev.toType_(ft.isInput, ft.isSubscription)),
        directives = Some(directives).filter(_.nonEmpty)
      ),
      (v: V) => fn(v).fold(ev.resolve, identity)
    )
}

case class PartiallyAppliedFieldWithArgs[V, A](
  name: String,
  description: Option[String],
  directives: List[Directive]
) {
  def apply[R, V1](
    fn: V => (A => V1)
  )(implicit ev1: Schema[R, A => V1], fa: FieldAttributes): (__Field, V => Step[R]) =
    (
      __Field(
        name,
        description,
        ev1.arguments,
        () => ev1.toType_(fa.isInput, fa.isSubscription)
      ),
      (v: V) => ev1.resolve(fn(v))
    )
}
