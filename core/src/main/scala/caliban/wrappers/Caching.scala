package caliban.wrappers

import caliban.ResponseValue.ObjectValue
import caliban.Value._
import caliban._
import caliban.execution.{ ExecutionRequest, Field, FieldInfo }
import caliban.introspection.adt._
import caliban.parsing.adt.{ Directive, Document }
import caliban.schema.Annotations.{ GQLDirective, GQLName }
import caliban.schema.Types
import caliban.wrappers.Wrapper.{ EffectfulWrapper, FieldWrapper, OverallWrapper, ValidationWrapper }
import zio.query.ZQuery
import zio.{ duration2DurationOps, durationInt, Duration, Exit, FiberRef, Ref, UIO, Unsafe, ZIO }

import java.util.concurrent.{ ConcurrentHashMap, TimeUnit }
import scala.collection.compat._

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
      args = _ =>
        List(
          __InputValue(
            name = MaxAgeName,
            None,
            `type` = () => Types.int,
            defaultValue = None
          ),
          __InputValue(
            name = ScopeName,
            None,
            `type` = () => CacheScope._type,
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

  def aspect[R](output: Ref[CachePolicy] => Wrapper[R]): GraphQLAspect[Nothing, R] =
    new Default(output)

  /**
   * Computes the total cache policy for a query and stores it in the extensions of the response.
   * The result can then be used by http adapters to set the appropriate cache headers.
   */
  def extension(settings: CacheSettings = CacheSettings.default): GraphQLAspect[Nothing, Any] = {
    def extensionBuilder(state: Ref[CachePolicy]): OverallWrapper[Any] =
      new OverallWrapper[Any] {
        override def wrap[R](
          f: GraphQLRequest => ZIO[R, Nothing, GraphQLResponse[CalibanError]]
        ): GraphQLRequest => ZIO[R, Nothing, GraphQLResponse[CalibanError]] = { (request: GraphQLRequest) =>
          (f(request) zipWith state.get) {
            case (response, cacheState) if response.errors.isEmpty =>
              response.copy(
                extensions = Some(
                  ObjectValue(
                    (DirectiveName -> cacheState.toResponseValue(settings.toHeader)) :: response.extensions.fold(
                      List.empty[(String, ResponseValue)]
                    )(_.fields)
                  )
                )
              )
            case (response, _)                                     => response

          }
        }
      }

    aspect(extensionBuilder)
  }

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
    defaultScope: CacheScope,
    toHeader: CachePolicy => String = _.hint.toHeaderString
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
  def setCacheHint(hint: CacheHint): UIO[Unit] = cacheOverride.update {
    case Some(hint0) => Some(mostRestrictive(hint0, hint))
    case None        => Some(hint)
  }

  /**
   * Disables caching for the current query
   */
  def disableCaching: UIO[Unit] = cacheOverride.set(Some(CacheHint(Some(0.seconds))))

  case class CachePolicy(hint: CacheHint) {
    def merge(that: CachePolicy): CachePolicy =
      CachePolicy(mostRestrictive(hint, that.hint))

    def restrict(restricted: Option[CacheHint]): CachePolicy = restricted match {
      case Some(h) => copy(hint = mostRestrictive(hint, h))
      case None    => this
    }

    def toResponseValue(toHeader: CachePolicy => String) =
      ObjectValue(
        List(
          "version"    -> IntValue(2),
          "httpHeader" -> StringValue(toHeader(this))
        )
      )
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

    implicit val ordering: Ordering[CacheScope] = {
      case (Private, Private) => 0
      case (Public, Public)   => 0
      case (Private, Public)  => -1
      case (Public, Private)  => 1
    }

    val _type = __Type(
      __TypeKind.ENUM,
      Some("CacheScope"),
      Some("The scope of the cache"),
      enumValues = _ =>
        Some(
          List(
            __EnumValue(
              "PRIVATE",
              Some("The cache is private to the user"),
              deprecationReason = None,
              isDeprecated = false,
              directives = None
            ),
            __EnumValue(
              "PUBLIC",
              Some("The cache is public"),
              deprecationReason = None,
              isDeprecated = false,
              directives = None
            )
          )
        )
    )
  }

  case class CacheHint(
    maxAge: Option[Duration] = None,
    scope: Option[CacheScope] = None,
    inheritMaxAge: Boolean = false
  ) { self =>
    def toHeaderString: String = {
      val computedMaxAge = maxAge.fold(0L)(_.toSeconds)
      val cacheable      = computedMaxAge > 0L
      if (!cacheable) "no-store"
      else {
        val b = new StringBuilder()
        b.append("max-age=")
        b.append(computedMaxAge)
        scope.foreach { s =>
          b.append(", ")
          b.append(s.toString.toLowerCase)
        }
        b.toString()
      }
    }
  }

  object CacheHint {
    val default: CacheHint = CacheHint()
  }

  private class Default[-R1](inner: Ref[CachePolicy] => Wrapper[R1]) extends GraphQLAspect[Nothing, R1] {
    private val _typeCache = new ConcurrentHashMap[String, Option[CacheHint]]()

    def apply[R <: R1](gql: GraphQL[R]): GraphQL[R] = {
      val wrapper = EffectfulWrapper(
        cacheOverride.set(None) *> Ref
          .make(CachePolicy(CacheHint.default))
          .map(state => staticWrapper(state).skipForIntrospection |+| fieldWrapper(state) |+| inner(state))
      )

      gql
        .withAdditionalDirectives(cacheDirectives)
        .withAdditionalTypes(CacheScope._type :: Nil)
        .withWrapper(wrapper)
    }

    private[caliban] def cacheHintFromType(typ: __Type): Option[CacheHint] = {
      val key = typ.name.getOrElse(typ.toString)
      _typeCache.computeIfAbsent(key, _ => typ.directives.flatMap(extractCacheDirective))
    }

    private def staticWrapper(state: Ref[CachePolicy]): ValidationWrapper[Any] = new ValidationWrapper[Any] {
      override def wrap[R](
        f: Document => ZIO[R, CalibanError.ValidationError, ExecutionRequest]
      ): Document => ZIO[R, CalibanError.ValidationError, ExecutionRequest] = { (d: Document) =>
        f(d).flatMap { request =>
          def loop(policy: CachePolicy, field: Field, parentHint: Option[CacheHint]): CachePolicy = {
            val fieldHint   = extractCacheDirective(field.directives)
            val isInherited = fieldHint.exists(_.inheritMaxAge)
            def typeHint    = field.parentType.flatMap(cacheHintFromType)
            val actualHint  = (if (isInherited) parentHint else fieldHint) orElse typeHint

            val newPolicy = policy.restrict(actualHint)

            field.fields.foldLeft(newPolicy)(loop(_, _, actualHint))
          }

          val updated = loop(CachePolicy(CacheHint.default), request.field, None)

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
            case None                => Exit.succeed(result)
          }
        }
    }

  }

  private def mostRestrictive(
    parent: CacheHint,
    current: CacheHint
  ): CacheHint = {
    val scope         = List(parent.scope, current.scope).flatten.minOption
    val shouldInherit = current.inheritMaxAge
    val maxAge        =
      if (shouldInherit && parent.maxAge.isDefined) parent.maxAge
      else
        (current.maxAge, parent.maxAge) match {
          case (Some(a), Some(b)) => Some(a min b)
          case (Some(a), None)    => Some(a)
          case (None, Some(b))    => Some(b)
          case (None, None)       => None
        }
    CacheHint(maxAge, scope, shouldInherit)
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
