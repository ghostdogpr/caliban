package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue._
import caliban.Value._
import caliban.execution.Field
import caliban.introspection.adt._
import caliban.parsing.adt.{ Directive, Directives }
import caliban.relay.{ Base64Cursor, Cursor }
import caliban.schema.Step.{ PureStep => _, _ }
import caliban.schema.Types._
import caliban.uploads.Upload
import caliban.{ InputValue, ResponseValue }
import zio.query.ZQuery
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.stream.ZStream
import zio.{ Chunk, Trace, URIO, ZIO }

import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal
import java.util.UUID
import scala.annotation.{ implicitNotFound, nowarn }
import scala.concurrent.Future

/**
 * Typeclass that defines how to map the type `T` to the according GraphQL concepts: how to introspect it and how to resolve it.
 * `R` is the ZIO environment required by the effects in the schema (`Any` if nothing required).
 */
@implicitNotFound(
  """Cannot find a Schema for type ${T}.

Caliban provides instances of Schema for the most common Scala types, and can derive it for your case classes and sealed traits.
Derivation requires that you have a Schema for any other type nested inside ${T}.
If you use a custom type as an argument, you also need to provide an implicit ArgBuilder for that type.
See https://ghostdogpr.github.io/caliban/docs/schema.html for more information.
"""
)
trait Schema[-R, T] { self =>

  private lazy val asType: __Type             = toType(): @nowarn("msg=deprecated")
  private lazy val asInputType: __Type        = toType(isInput = true): @nowarn("msg=deprecated")
  private lazy val asSubscriptionType: __Type = toType(isSubscription = true): @nowarn("msg=deprecated")

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
   *
   * Note that this method is public to allow derivation via the `derives` keyword in Scala 3.
   * To avoid accidental usage (which would be bad for performance), we mark as @deprecated, which will lead to compiler warnings
   * when used
   *
   * @param isInput indicates if the type is passed as an argument. This is needed because GraphQL differentiates `InputType` from `ObjectType`.
   * @param isSubscription indicates if the type is used in a subscription operation.
   *                       For example, ZStream gives a different GraphQL type depending whether it is used in a subscription or elsewhere.
   */
  @deprecated("use toType_ instead")
  def toType(isInput: Boolean = false, isSubscription: Boolean = false): __Type

  /**
   * Resolves `T` by turning a value of type `T` into an execution step that describes how to resolve the value.
   * @param value a value of type `T`
   */
  def resolve(value: T): Step[R]

  /**
   * Defines if the type can resolve to null or not.
   * It is true if the type is nullable or if it can fail.
   */
  @deprecatedOverriding("this method will be made final. Override canFail and nullable instead", "2.7.0")
  def optional: Boolean = canFail || nullable

  /**
   * Defines if the underlying type represents a nullable value. Should be false except for `Option`.
   */
  def nullable: Boolean = false

  /**
   * Defines if the type can fail during resolution.
   */
  def canFail: Boolean = false

  /**
   * Defined the arguments of the given type. Should be empty except for `Function`.
   */
  def arguments: List[__InputValue] = Nil

  // Disables traces for caliban-provided effectful schemas
  private[caliban] implicit def trace: Trace = Trace.empty

  /**
   * Builds a new `Schema` of `A` from an existing `Schema` of `T` and a function from `A` to `T`.
   * @param f a function from `A` to `T`.
   */
  def contramap[A](f: A => T): Schema[R, A] = new Schema[R, A] {
    override def nullable: Boolean                                         = self.nullable
    override def canFail: Boolean                                          = self.canFail
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
    override def nullable: Boolean                                         = self.nullable
    override def canFail: Boolean                                          = self.canFail
    override def arguments: List[__InputValue]                             = self.arguments
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
      val tpe     = self.toType_(isInput, isSubscription)
      val newName = if (isInput) inputName.getOrElse(Schema.customizeInputTypeName(name)) else name
      val newTpe  = tpe.copy(name = Some(newName))

      tpe.kind match {
        case __TypeKind.INTERFACE =>
          val pt = tpe.possibleTypes.map(_.map { t0 =>
            val newInterfaces = t0
              .interfaces()
              .map(_.map(t1 => if (t1.name == tpe.name && t1.name.isDefined) t1.copy(name = Some(newName)) else t1))
            t0.copy(interfaces = () => newInterfaces)
          })
          newTpe.copy(possibleTypes = pt)
        case _                    => newTpe
      }
    }

    private lazy val renameTypename: Boolean = self.toType_().kind match {
      case __TypeKind.UNION | __TypeKind.ENUM | __TypeKind.INTERFACE => false
      case _                                                         => true
    }

    override def resolve(value: T): Step[R] = {
      def loop(step: Step[R]): Step[R] = step match {
        case ObjectStep(_, fields)   => ObjectStep(name, fields)
        case PureStep(EnumValue(_))  => PureStep(EnumValue(name))
        case MetadataFunctionStep(s) => MetadataFunctionStep(field => loop(s(field)))
        case FunctionStep(s)         => FunctionStep(args => loop(s(args)))
        case other                   => other
      }

      val step = self.resolve(value)
      if (renameTypename) loop(step) else step
    }
  }
}

object Schema extends GenericSchema[Any] with SchemaVersionSpecific

trait GenericSchema[R] extends SchemaDerivation[R] with TemporalSchema {

  /**
   * Creates a scalar schema for a type `A`
   * @param name name of the scalar type
   * @param description description of the scalar type
   * @param specifiedBy URL of the scalar specification
   * @param directives the directives to add to the type
   * @param makeResponse function from `A` to [[ResponseValue]] that defines how to resolve `A`
   */
  def scalarSchema[A](
    name: String,
    description: Option[String],
    specifiedBy: Option[String],
    directives: Option[List[Directive]],
    makeResponse: A => ResponseValue
  ): Schema[Any, A] =
    new Schema[Any, A] {
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        makeScalar(name, description, specifiedBy, directives)
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
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
        val _ = resolver
        if (isInput) {
          makeInputObject(
            Some(customizeInputTypeName(name)),
            description,
            fields(isInput, isSubscription).map { case (f, _) =>
              __InputValue(f.name, f.description, f.`type`, None, directives = f.directives)
            },
            directives = Some(directives)
          )
        } else makeObject(Some(name), description, fields(isInput, isSubscription).map(_._1), directives)
      }

      private lazy val resolver =
        ObjectFieldResolver[R1, A](name, fields(false, false).map(f => (f._1.name, f._2)))

      override def resolve(value: A): Step[R1] = resolver.resolve(value)
    }

  /**
   * Creates an enum schema for a type A
   * @param name         name of the scalar type
   * @param description  description of the scalar type
   * @param values       list of possible enum values
   * @param directives   the directives to add to the type
   * @param repr         function that defines how to convert A into a string. WARNING: The resulting string must be contained in the values list
   * @see [[enumValue]]  convenience method for creating enum values required by this method
   */
  def enumSchema[A](
    name: String,
    description: Option[String] = None,
    values: List[__EnumValue],
    directives: List[Directive] = List.empty,
    repr: A => String
  ): Schema[Any, A] = new Schema[Any, A] {
    private val validEnumValues = values.map(_.name).toSet

    override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
      makeEnum(Some(name), description, values, None, if (directives.nonEmpty) Some(directives) else None)

    override def resolve(value: A): Step[Any] = {
      val asString = repr(value)
      if (validEnumValues.contains(asString)) PureStep(EnumValue(asString))
      else Step.fail(s"Invalid enum value '$asString'")
    }
  }

  def enumValue(
    name: String,
    description: Option[String] = None,
    isDeprecated: Boolean = false,
    deprecationReason: Option[String] = None,
    directives: List[Directive] = List.empty
  ): __EnumValue = __EnumValue(
    name,
    description,
    isDeprecated,
    deprecationReason,
    directives = if (directives.nonEmpty) Some(directives) else None
  )

  /**
   * Manually defines a field from a name, a description, some directives and a resolver.
   * If the field is a function that should be called lazily, use `fieldLazy` instead.
   * If the field takes arguments, use `fieldWithArgs` instead.
   */
  def field[V](
    name: String,
    description: Option[String] = None,
    directives: List[Directive] = List.empty
  ): PartiallyAppliedField[V] =
    PartiallyAppliedField[V](name, description, directives)

  /**
   * Manually defines a lazy field from a name, a description, some directives and a resolver.
   */
  def fieldLazy[V](
    name: String,
    description: Option[String] = None,
    directives: List[Directive] = List.empty
  ): PartiallyAppliedFieldLazy[V] =
    PartiallyAppliedFieldLazy[V](name, description, directives)

  /**
   * Manually defines a field with arguments from a name, a description, some directives and a resolver.
   */
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
   *
   * @see `customObj` for an improved variant using context functions (Scala 3 only)
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

  implicit val unitSchema: Schema[Any, Unit]             = scalarSchema("Unit", None, None, None, _ => ObjectValue(Nil))
  implicit val booleanSchema: Schema[Any, Boolean]       = scalarSchema("Boolean", None, None, None, BooleanValue.apply)
  implicit val stringSchema: Schema[Any, String]         = scalarSchema("String", None, None, None, StringValue.apply)
  implicit val uuidSchema: Schema[Any, UUID]             = scalarSchema("ID", None, None, None, uuid => StringValue(uuid.toString))
  implicit val shortSchema: Schema[Any, Short]           = scalarSchema("Short", None, None, None, IntValue(_))
  implicit val intSchema: Schema[Any, Int]               = scalarSchema("Int", None, None, None, IntValue(_))
  implicit val longSchema: Schema[Any, Long]             = scalarSchema("Long", None, None, None, IntValue(_))
  implicit val bigIntSchema: Schema[Any, BigInt]         = scalarSchema("BigInt", None, None, None, IntValue(_))
  implicit val doubleSchema: Schema[Any, Double]         = scalarSchema("Float", None, None, None, FloatValue(_))
  implicit val floatSchema: Schema[Any, Float]           = scalarSchema("Float", None, None, None, FloatValue(_))
  implicit val bigDecimalSchema: Schema[Any, BigDecimal] = scalarSchema("BigDecimal", None, None, None, FloatValue(_))
  implicit val uploadSchema: Schema[Any, Upload]         =
    scalarSchema("Upload", None, None, None, _ => StringValue("<upload>"))

  implicit val base64CursorSchema: Schema[Any, Base64Cursor] =
    Schema.stringSchema.contramap(Cursor[Base64Cursor].encode)

  implicit def optionSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, Option[A]]                                  = new Schema[R0, Option[A]] {
    override def nullable: Boolean                                         = true
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
      (if (ev.nullable || ev.canFail) t else t.nonNull).list
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
      override def nullable: Boolean                                         = ev.nullable
      override def canFail: Boolean                                          = ev.canFail
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: () => A): Step[R0]                         = FunctionStep(_ => ev.resolve(value()))
    }
  implicit def metadataFunctionSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, Field => A]                       =
    new Schema[R0, Field => A] {
      override def arguments: List[__InputValue]                             = ev.arguments
      override def nullable: Boolean                                         = ev.nullable
      override def canFail: Boolean                                          = ev.canFail
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: Field => A): Step[R0]                      = MetadataFunctionStep(field => ev.resolve(value(field)))
    }

  implicit def eitherSchema[RA, RB, A, B](implicit
    evA: Schema[RA, A],
    evB: Schema[RB, B]
  ): Schema[RA with RB, Either[A, B]] = {
    val typeAName: String   = Types.name(evA.toType_())
    val typeBName: String   = Types.name(evB.toType_())
    val name: String        = s"Either${typeAName}Or$typeBName"
    val description: String = s"Either $typeAName or $typeBName"

    implicit val leftSchema: Schema[RA, A]  = new Schema[RA, A] {
      override def nullable: Boolean                                         = true
      override def canFail: Boolean                                          = evA.canFail
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = evA.toType_(isInput, isSubscription)
      override def resolve(value: A): Step[RA]                               = evA.resolve(value)
    }
    implicit val rightSchema: Schema[RB, B] = new Schema[RB, B] {
      override def nullable: Boolean                                         = true
      override def canFail: Boolean                                          = evB.canFail
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
    val typeAName: String = Types.name(evA.toType_())
    val typeBName: String = Types.name(evB.toType_())

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
      private lazy val typeAName: String   = Types.name(evA.toType_())
      private lazy val typeBName: String   = Types.name(evB.toType_())
      private lazy val name: String        = s"KV$typeAName$typeBName"
      private lazy val description: String = s"A key-value pair of $typeAName and $typeBName"

      private lazy val kvSchema: Schema[RA with RB, (A, B)] =
        obj[RA with RB, (A, B)](name, Some(description))(implicit ft =>
          List(
            field("key", Some("Key"))(_._1),
            field("value", Some("Value"))(_._2)
          )
        )

      override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        kvSchema.toType_(isInput, isSubscription).nonNull.list

      override def resolve(value: Map[A, B]): Step[RA with RB] = ListStep(value.toList.map(kvSchema.resolve))
    }
  implicit def functionSchema[RA, RB, A, B](implicit
    arg1: ArgBuilder[A],
    ev1: Schema[RA, A],
    ev2: Schema[RB, B]
  ): Schema[RB, A => B] =
    new Schema[RB, A => B] {
      private lazy val inputType                 = ev1.toType_(true)
      private val unwrappedArgumentName          = "value"
      override def arguments: List[__InputValue] = {
        val input = inputType.allInputFields
        if (input.nonEmpty) input
        else
          handleInput(List.empty[__InputValue])(
            List(
              __InputValue(
                unwrappedArgumentName,
                None,
                () => if (ev1.nullable || ev1.canFail) inputType else inputType.nonNull,
                None
              )
            )
          )
      }

      override def nullable: Boolean                                         = ev2.nullable
      override def canFail: Boolean                                          = ev2.canFail
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev2.toType_(isInput, isSubscription)

      override def resolve(f: A => B): Step[RB]                         =
        FunctionStep { args =>
          val builder = arg1.build(InputValue.ObjectValue(args))
          handleInput(builder)(
            builder.fold(
              error => args.get(unwrappedArgumentName).fold[Either[ExecutionError, A]](Left(error))(arg1.build),
              Right(_)
            )
          )
            .fold(error => Step.fail(error), value => ev2.resolve(f(value)))

        }
      private def handleInput[T](onWrapped: => T)(onUnwrapped: => T): T =
        inputType.kind match {
          case __TypeKind.SCALAR | __TypeKind.ENUM | __TypeKind.LIST =>
            // argument was not wrapped in a case class
            onUnwrapped
          case _                                                     => onWrapped
        }
    }

  implicit def futureSchema[R0, A](implicit ev: Schema[R0, A]): Schema[R0, Future[A]]                                 =
    effectSchema[R0, R0, R0, Throwable, A].contramap[Future[A]](future => ZIO.fromFuture(_ => future)(Trace.empty))
  implicit def infallibleEffectSchema[R0, R1 >: R0, R2 >: R0, A](implicit ev: Schema[R2, A]): Schema[R0, URIO[R1, A]] =
    new Schema[R0, URIO[R1, A]] {
      override def nullable: Boolean                                         = ev.nullable
      override def canFail: Boolean                                          = ev.canFail
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: URIO[R1, A]): Step[R0]                     = QueryStep(ZQuery.fromZIONow(value.map(ev.resolve)))
    }
  implicit def effectSchema[R0, R1 >: R0, R2 >: R0, E <: Throwable, A](implicit
    ev: Schema[R2, A]
  ): Schema[R0, ZIO[R1, E, A]] =
    new Schema[R0, ZIO[R1, E, A]] {
      override def nullable: Boolean                                         = ev.nullable
      override def canFail: Boolean                                          = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: ZIO[R1, E, A]): Step[R0]                   = QueryStep(ZQuery.fromZIONow(value.map(ev.resolve)))
    }
  def customErrorEffectSchema[R0, R1 >: R0, R2 >: R0, E, A](convertError: E => ExecutionError)(implicit
    ev: Schema[R2, A]
  ): Schema[R0, ZIO[R1, E, A]] =
    new Schema[R0, ZIO[R1, E, A]] {
      override def nullable: Boolean                                         = ev.nullable
      override def canFail: Boolean                                          = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: ZIO[R1, E, A]): Step[R0]                   = QueryStep(
        ZQuery.fromZIONow(value.mapBoth(convertError, ev.resolve))
      )
    }
  implicit def infallibleQuerySchema[R0, R1 >: R0, R2 >: R0, A](implicit
    ev: Schema[R2, A]
  ): Schema[R0, ZQuery[R1, Nothing, A]] =
    new Schema[R0, ZQuery[R1, Nothing, A]] {
      override def nullable: Boolean                                         = ev.nullable
      override def canFail: Boolean                                          = ev.canFail
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: ZQuery[R1, Nothing, A]): Step[R0]          = QueryStep(value.map(ev.resolve))
    }
  implicit def querySchema[R0, R1 >: R0, R2 >: R0, E <: Throwable, A](implicit
    ev: Schema[R2, A]
  ): Schema[R0, ZQuery[R1, E, A]] =
    new Schema[R0, ZQuery[R1, E, A]] {
      override def nullable: Boolean                                         = ev.nullable
      override def canFail: Boolean                                          = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: ZQuery[R1, E, A]): Step[R0]                = QueryStep(value.map(ev.resolve))
    }
  def customErrorQuerySchema[R0, R1 >: R0, R2 >: R0, E, A](convertError: E => ExecutionError)(implicit
    ev: Schema[R2, A]
  ): Schema[R0, ZQuery[R1, E, A]] =
    new Schema[R0, ZQuery[R1, E, A]] {
      override def nullable: Boolean                                         = ev.nullable
      override def canFail: Boolean                                          = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = ev.toType_(isInput, isSubscription)
      override def resolve(value: ZQuery[R1, E, A]): Step[R0]                = QueryStep(value.mapBoth(convertError, ev.resolve))
    }
  implicit def infallibleStreamSchema[R1, R2 >: R1, A](implicit
    ev: Schema[R2, A]
  ): Schema[R1, ZStream[R1, Nothing, A]] =
    new Schema[R1, ZStream[R1, Nothing, A]] {
      override def nullable: Boolean                                         = false
      override def canFail: Boolean                                          = ev.canFail
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
        val t = ev.toType_(isInput, isSubscription)
        if (isSubscription) t else (if (ev.nullable || ev.canFail) t else t.nonNull).list
      }
      override def resolve(value: ZStream[R1, Nothing, A]): Step[R1]         = StreamStep(value.map(ev.resolve))
    }
  implicit def streamSchema[R0, R1 >: R0, R2 >: R0, E <: Throwable, A](implicit
    ev: Schema[R2, A]
  ): Schema[R0, ZStream[R1, E, A]] =
    new Schema[R0, ZStream[R1, E, A]] {
      override def nullable: Boolean                                         = ev.nullable
      override def canFail: Boolean                                          = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
        val t = ev.toType_(isInput, isSubscription)
        if (isSubscription) t else (if (ev.nullable || ev.canFail) t else t.nonNull).list
      }
      override def resolve(value: ZStream[R1, E, A]): Step[R0]               = StreamStep(value.map(ev.resolve))
    }
  def customErrorStreamSchema[R0, R1 >: R0, R2 >: R0, E, A](convertError: E => ExecutionError)(implicit
    ev: Schema[R2, A]
  ): Schema[R0, ZStream[R1, E, A]] =
    new Schema[R0, ZStream[R1, E, A]] {
      override def nullable: Boolean                                         = ev.nullable
      override def canFail: Boolean                                          = true
      override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
        val t = ev.toType_(isInput, isSubscription)
        if (isSubscription) t else (if (ev.nullable || ev.canFail) t else t.nonNull).list
      }
      override def resolve(value: ZStream[R1, E, A]): Step[R0]               = StreamStep(value.mapBoth(convertError, ev.resolve))
    }
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

  lazy val sampleDate: ZonedDateTime = ZonedDateTime.ofInstant(Instant.EPOCH, ZoneId.ofOffset("UTC", ZoneOffset.UTC))

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
      Some(
        s"A date-time without a time-zone in the ISO-8601 calendar system in the format of ${formatter.format(sampleDate)}"
      )
    )(formatter)

  implicit lazy val offsetDateTimeSchema: Schema[Any, OffsetDateTime] =
    offsetDateTimeSchemaWithFormatter(DateTimeFormatter.ISO_OFFSET_DATE_TIME)

  def offsetDateTimeSchemaWithFormatter(formatter: DateTimeFormatter): Schema[Any, OffsetDateTime] =
    temporalSchemaWithFormatter(
      "OffsetDateTime",
      Some(
        s"A date-time with an offset from UTC/Greenwich in the ISO-8601 calendar system using the format ${formatter.format(sampleDate)}"
      )
    )(formatter)

  implicit lazy val zonedDateTimeSchema: Schema[Any, ZonedDateTime] =
    zonedDateTimeSchemaWithFormatter(DateTimeFormatter.ISO_ZONED_DATE_TIME)

  val localDateTimeEpochSchema: Schema[Any, LocalDateTime] =
    temporalSchema("LocalDateTime")(a => IntValue.LongNumber(a.toInstant(ZoneOffset.UTC).toEpochMilli))

  def zonedDateTimeSchemaWithFormatter(formatter: DateTimeFormatter): Schema[Any, ZonedDateTime] =
    temporalSchemaWithFormatter(
      "ZonedDateTime",
      Some(
        s"A date-time with a time-zone in the ISO-8601 calendar system using the format ${formatter.format(sampleDate)}"
      )
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
      Some(
        s"A date without a time-zone in the ISO-8601 calendar system using the format ${formatter.format(sampleDate)}"
      )
    )(formatter)

  implicit lazy val localTimeSchema: Schema[Any, LocalTime] =
    localTimeSchemaWithFormatter(DateTimeFormatter.ISO_LOCAL_TIME)

  def localTimeSchemaWithFormatter(formatter: DateTimeFormatter): Schema[Any, LocalTime] =
    temporalSchemaWithFormatter(
      "LocalTime",
      Some(
        s"A time without a time-zone in the ISO-8601 calendar system using the format ${formatter.format(sampleDate)}"
      )
    )(formatter)

}

case class FieldAttributes(isInput: Boolean, isSubscription: Boolean)

abstract class PartiallyAppliedFieldBase[V](
  name: String,
  description: Option[String],
  directives: List[Directive]
) {
  def apply[R, V1](fn: V => V1)(implicit ev: Schema[R, V1], ft: FieldAttributes): (__Field, V => Step[R]) =
    either[R, V1](v => Left(fn(v)))(ev, ft)

  def either[R, V1](
    fn: V => Either[V1, Step[R]]
  )(implicit ev: Schema[R, V1], ft: FieldAttributes): (__Field, V => Step[R])

  protected def makeField[R, V1](implicit ev: Schema[R, V1], ft: FieldAttributes): __Field =
    __Field(
      name,
      description,
      _ => Nil,
      () =>
        if (ev.nullable || ev.canFail) ev.toType_(ft.isInput, ft.isSubscription)
        else ev.toType_(ft.isInput, ft.isSubscription).nonNull,
      isDeprecated = Directives.isDeprecated(directives),
      deprecationReason = Directives.deprecationReason(directives),
      directives = Some(
        directives.filter(_.name != "deprecated")
      ).filter(_.nonEmpty)
    )
}

case class PartiallyAppliedField[V](
  name: String,
  description: Option[String],
  directives: List[Directive]
) extends PartiallyAppliedFieldBase[V](name, description, directives) {
  def either[R, V1](
    fn: V => Either[V1, Step[R]]
  )(implicit ev: Schema[R, V1], ft: FieldAttributes): (__Field, V => Step[R]) =
    (makeField, (v: V) => fn(v).fold(ev.resolve, identity))
}

case class PartiallyAppliedFieldLazy[V](
  name: String,
  description: Option[String],
  directives: List[Directive]
) extends PartiallyAppliedFieldBase[V](name, description, directives) {
  def either[R, V1](
    fn: V => Either[V1, Step[R]]
  )(implicit ev: Schema[R, V1], ft: FieldAttributes): (__Field, V => Step[R]) =
    (makeField, (v: V) => FunctionStep(_ => fn(v).fold(ev.resolve, identity)))
}

case class PartiallyAppliedFieldWithArgs[V, A](
  name: String,
  description: Option[String],
  directives: List[Directive]
) {
  def apply[R, V1](fn: V => (A => V1))(implicit ev1: Schema[R, A => V1], fa: FieldAttributes): (__Field, V => Step[R]) =
    (
      Types.makeField(
        name,
        description,
        ev1.arguments,
        () =>
          if (ev1.nullable || ev1.canFail) ev1.toType_(fa.isInput, fa.isSubscription)
          else ev1.toType_(fa.isInput, fa.isSubscription).nonNull,
        isDeprecated = Directives.isDeprecated(directives),
        deprecationReason = Directives.deprecationReason(directives),
        directives = Some(
          directives.filter(_.name != "deprecated")
        ).filter(_.nonEmpty)
      ),
      (v: V) => ev1.resolve(fn(v))
    )
}
