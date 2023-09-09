package caliban.wrappers

import caliban.Value._
import caliban._
import caliban.execution.{ ExecutionRequest, Field, FieldInfo }
import caliban.introspection.adt.{ __Directive, __DirectiveLocation, __InputValue, __Type }
import caliban.parsing.adt.{ Directive, Document }
import caliban.schema.Annotations.{ GQLDirective, GQLName }
import caliban.schema.{ Schema, Types }
import caliban.wrappers.Wrapper.{ EffectfulWrapper, FieldWrapper, OverallWrapper, ValidationWrapper }
import zio.prelude._
import zio.query.ZQuery
import zio.{ durationInt, Duration, FiberRef, Ref, UIO, Unsafe, ZIO }

import java.util.concurrent.{ ConcurrentHashMap, TimeUnit }

object Caching {
  val DirectiveName     = "cacheControl"
  val MaxAgeName        = "maxAge"
  val ScopeName         = "scope"
  val InheritMaxAgeName = "inheritMaxAge"

  private val cacheOverride: FiberRef[Option[CacheHint]] = Unsafe.unsafe(implicit u => FiberRef.unsafe.make(None))

  private val cacheDirectives = List(
    __Directive(
      name = DirectiveName,
      description = None,
      locations = Set(__DirectiveLocation.FIELD_DEFINITION, __DirectiveLocation.OBJECT),
      args = List(
        __InputValue(
          name = MaxAgeName,
          None,
          `type` = () => Types.int,
          defaultValue = None
        ),
        __InputValue(
          name = ScopeName,
          None,
          `type` = () => CacheScope.schema.toType_(),
          defaultValue = None
        ),
        __InputValue(
          name = InheritMaxAgeName,
          None,
          `type` = () => Types.boolean,
          defaultValue = None
        )
      ),
      isRepeatable = false
    )
  )

  /**
   * Computes the total cache policy for a query and stores it in the extensions of the response.
   * The result can then be used by http adapters to set the appropriate cache headers.
   */
  def extension(settings: CacheSettings = CacheSettings.default): GraphQLAspect[Nothing, Any] = Default(settings)

  /**
   * Assigns a cache hint to a field or a type
   * @param maxAge The maximum duration that this field should be cached for
   * @param scope The scope of that should be applied to the cache
   * @param inheritMaxAge Whether the maxAge should be inherited from the parent type
   */
  case class GQLCacheControl(
    maxAge: Option[Duration] = None,
    scope: Option[CacheScope] = None,
    inheritMaxAge: Boolean = false
  ) extends GQLDirective(
        Directive(
          DirectiveName,
          Map(
            MaxAgeName        -> maxAge.fold[InputValue](NullValue)(d => IntValue(d.toSeconds)),
            ScopeName         -> scope.fold[InputValue](NullValue)(s => EnumValue(s.toString)),
            InheritMaxAgeName -> BooleanValue(inheritMaxAge)
          )
        )
      )

  case class CacheSettings(
    defaultMaxAge: Duration,
    defaultScope: CacheScope
  ) {
    def withMaxAge(maxAge: Duration): CacheSettings = copy(defaultMaxAge = maxAge)

    def withScope(scope: CacheScope): CacheSettings = copy(defaultScope = scope)
  }

  object CacheSettings {
    val default: CacheSettings = CacheSettings(0.seconds, CacheScope.Public)
  }

  /**
   * Overrides the cache hint for a particular field. This can be used to dynamically set the cache hint within
   * a resolver
   */
  def setCacheHint(hint: CacheHint): UIO[Unit] = cacheOverride.set(Some(hint))

  case class CachePolicy(hint: CacheHint) {
    def merge(that: CachePolicy): CachePolicy =
      CachePolicy(mostRestrictive(hint, that.hint))

    def restrict(restricted: Option[CacheHint]): CachePolicy = restricted match {
      case Some(h) => copy(hint = mostRestrictive(hint, h))
      case None    => this
    }

    private def mostRestrictive(
      parent: CacheHint,
      current: CacheHint
    ): CacheHint = {
      val scope         = List(parent.scope, current.scope).flatten.minOption
      val shouldInherit = current.inheritMaxAge
      val maxAge        =
        if (shouldInherit) parent.maxAge
        else
          (current.maxAge, parent.maxAge) match {
            case (Some(a), Some(b)) => Some(a min b)
            case (Some(a), None)    => Some(a)
            case (None, Some(b))    => Some(b)
            case (None, None)       => None
          }
      CacheHint(maxAge, scope, shouldInherit)
    }
  }

  case class CacheHint(
    maxAge: Option[Duration] = None,
    scope: Option[CacheScope] = None,
    inheritMaxAge: Boolean = false
  )

  object CacheHint {
    val default: CacheHint = CacheHint()
  }

  sealed trait CacheScope
  object CacheScope {

    @GQLName("PRIVATE")
    case object Private extends CacheScope {
      override def toString: String = "PRIVATE"
    }

    @GQLName("PUBLIC")
    case object Public extends CacheScope {
      override def toString: String = "PUBLIC"
    }

    implicit val ord: Ord[CacheScope] = Ord.make {
      case (Private, Private) => Ordering.Equals
      case (Public, Public)   => Ordering.Equals
      case (Private, Public)  => Ordering.LessThan
      case (Public, Private)  => Ordering.GreaterThan
    }

    implicit val ordering: scala.math.Ordering[CacheScope] = ord.toScala

    implicit val schema: Schema[Any, CacheScope] = Schema.gen
  }

  private case class Default[-R1](settings: CacheSettings) extends GraphQLAspect[Nothing, R1] {
    private val _typeCache = new ConcurrentHashMap[String, Option[CacheHint]]()

    def apply[R <: R1](gql: GraphQL[R]): GraphQL[R] = {
      val wrapper = EffectfulWrapper(
        Ref
          .make(CachePolicy(CacheHint.default))
          .map(state => staticWrapper(state) |+| fieldWrapper(state) |+| overallWrapper(state))
      )

      gql
        .withAdditionalDirectives(cacheDirectives)
        .withAdditionalTypes(CacheScope.schema.toType_() :: Nil)
        .withWrapper(wrapper)
    }

    private[caliban] def cacheHintFromType(typ: __Type): Option[CacheHint] = {
      val key = typ.name.getOrElse(typ.toString)
      Option(_typeCache.get(key)) match {
        case Some(Some(hint)) => Some(hint)
        case Some(None)       => None
        case None             =>
          typ.directives match {
            case Some(dirs) =>
              val hint = extractCacheDirective(dirs)
              hint.foreach(h => _typeCache.put(key, Some(h)))
              hint
            case None       =>
              _typeCache.put(key, None)
              None
          }
      }
    }

    private def staticWrapper(state: Ref[CachePolicy]): ValidationWrapper[Any] = new ValidationWrapper[Any] {
      override def wrap[R](
        f: Document => ZIO[R, CalibanError.ValidationError, ExecutionRequest]
      ): Document => ZIO[R, CalibanError.ValidationError, ExecutionRequest] = { (d: Document) =>
        f(d).flatMap { request =>
          def loop(policy: CachePolicy, field: Field): CachePolicy = {
            val fieldHint = extractCacheDirective(field.directives)
            val typeHint  = field.parentType.flatMap(cacheHintFromType)
            val newPolicy = policy.restrict(fieldHint orElse typeHint)

            field.fields.foldLeft(newPolicy)(loop)
          }

          val updated = loop(CachePolicy(CacheHint.default), request.field)

          state.update(updated.merge).as(request)
        }
      }
    }

    private def fieldWrapper(state: Ref[CachePolicy]): FieldWrapper[Any] = new FieldWrapper[Any](false) {
      override def wrap[R](
        query: ZQuery[R, CalibanError.ExecutionError, ResponseValue],
        info: FieldInfo
      ): ZQuery[R, CalibanError.ExecutionError, ResponseValue] =
        query.mapZIO { result =>
          cacheOverride.get.flatMap {
            case Some(overrideValue) => state.update(_.restrict(Some(overrideValue))) as result
            case None                => ZIO.succeed(result)
          }
        }
    }

    private def overallWrapper(state: Ref[CachePolicy]): OverallWrapper[Any] = new OverallWrapper[Any] {
      override def wrap[R](
        f: GraphQLRequest => ZIO[R, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R, Nothing, GraphQLResponse[CalibanError]] = { (request: GraphQLRequest) =>
        (f(request) zipWith state.get) { (response, cacheState) =>
          val cacheControl = cacheState.hint
          val maxAge       = cacheControl.maxAge getOrElse settings.defaultMaxAge
          val scope        = cacheControl.scope getOrElse settings.defaultScope
          response.copy(
            extensions = Some(
              ResponseValue.ObjectValue(
                response.extensions.foldLeft[List[(String, ResponseValue)]](
                  List(
                    DirectiveName -> ResponseValue.ObjectValue(
                      List(
                        "version" -> IntValue(2),
                        "policy"  -> ResponseValue.ObjectValue(
                          List(
                            MaxAgeName -> IntValue(maxAge.toSeconds),
                            ScopeName  -> StringValue(scope.toString)
                          )
                        )
                      )
                    )
                  )
                )((s, extensions) => extensions.fields ++ s)
              )
            )
          )

        }
      }
    }

  }

  private def extractCacheDirective(directives: List[Directive]): Option[CacheHint] =
    directives.collectFirst {
      case d if d.name == DirectiveName =>
        val scope = d.arguments.get(ScopeName).collectFirst {
          case StringValue("PRIVATE") | EnumValue("PRIVATE") => CacheScope.Private
          case StringValue("PUBLIC") | EnumValue("PUBLIC")   => CacheScope.Public
        }

        val maxAge = d.arguments.get(MaxAgeName).collectFirst { case i: IntValue =>
          Duration(i.toLong, TimeUnit.SECONDS)
        }

        val inheritMaxAge = d.arguments
          .get(InheritMaxAgeName)
          .collectFirst { case BooleanValue(value) =>
            value
          }
          .getOrElse(false)

        CacheHint(maxAge, scope, inheritMaxAge)
    }

}
