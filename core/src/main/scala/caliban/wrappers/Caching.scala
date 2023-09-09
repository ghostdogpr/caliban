package caliban.wrappers

import caliban.Value.{ BooleanValue, EnumValue, IntValue, NullValue, StringValue }
import caliban.execution.{ ExecutionRequest, Field, FieldInfo }
import caliban.introspection.adt.{ __Directive, __DirectiveLocation, __InputValue, __Type }
import caliban.parsing.adt.{ Directive, Document }
import caliban.wrappers.Wrapper.{ EffectfulWrapper, FieldWrapper, OverallWrapper, ValidationWrapper }
import caliban._
import caliban.schema.Annotations.{ GQLDirective, GQLName }
import caliban.schema.{ Schema, Types }
import caliban.wrappers.Caching.CacheScope
import zio.prelude._
import zio.query.ZQuery
import zio.{ durationInt, Duration, FiberRef, Ref, UIO, Unsafe, ZIO }

import java.util.concurrent.{ ConcurrentHashMap, TimeUnit }

sealed trait Caching {

  def aspect: GraphQLAspect[Nothing, Any]

}

case class CacheSettings(defaultMaxAge: Option[Duration], defaultScope: Option[CacheScope]) {
  def withMaxAge(maxAge: Duration): CacheSettings = copy(defaultMaxAge = Some(maxAge))
  def withScope(scope: CacheScope): CacheSettings = copy(defaultScope = Some(scope))
}

object Caching {
  private val directiveName = "cacheControl"

  private val cacheDirectives = List(
    __Directive(
      name = directiveName,
      description = None,
      locations = Set(__DirectiveLocation.FIELD_DEFINITION, __DirectiveLocation.OBJECT),
      args = List(
        __InputValue(
          name = "maxAge",
          None,
          `type` = () => Types.int,
          defaultValue = None
        ),
        __InputValue(
          name = "scope",
          None,
          `type` = () => CacheScope.schema.toType_(),
          defaultValue = None
        ),
        __InputValue(
          name = "inheritMaxAge",
          None,
          `type` = () => Types.boolean,
          defaultValue = None
        )
      ),
      isRepeatable = false
    )
  )

  case class GQLCacheControl(
    maxAge: Option[Duration] = None,
    scope: Option[CacheScope] = None,
    inheritMaxAge: Boolean = false
  ) extends GQLDirective(
        Directive(
          directiveName,
          Map(
            "maxAge"        -> maxAge.fold[InputValue](NullValue)(d => IntValue(d.toSeconds)),
            "scope"         -> scope.fold[InputValue](NullValue)(s => EnumValue(s.toString)),
            "inheritMaxAge" -> BooleanValue(inheritMaxAge)
          )
        )
      )

  def apply(settings: CacheSettings = CacheSettings(Some(0.seconds), Some(CacheScope.Public))): Caching = Default(
    settings
  )

  private case class Default(settings: CacheSettings) extends Caching {
    private val _typeCache                           = new ConcurrentHashMap[String, Option[CacheHint]]()
    override def aspect: GraphQLAspect[Nothing, Any] =
      GraphQLAspect.withDirectives(cacheDirectives) @@
        GraphQLAspect.withTypes(CacheScope.schema.toType_() :: Nil) @@
        EffectfulWrapper(
          Ref.make(CacheState()).map(state => staticWrapper(state) |+| fieldWrapper(state) |+| overallWrapper(state))
        )

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

    private def staticWrapper(state: Ref[CacheState]): ValidationWrapper[Any] = new ValidationWrapper[Any] {
      override def wrap[R1](
        f: Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest] = { (d: Document) =>
        f(d).flatMap { request =>
          def loop(policy: CachePolicy, field: Field): CachePolicy = {
            val fieldHint = extractCacheDirective(field.directives)
            val typeHint  = field.parentType.flatMap(cacheHintFromType)
            val newPolicy = policy.restrict(fieldHint orElse typeHint)

            field.fields.foldLeft(newPolicy)(loop)
          }

          val updated = loop(CachePolicy(CacheHint.default), request.field)

          state.update(s => s.copy(policy = updated merge s.policy)).as(request)
        }
      }
    }

    private def fieldWrapper(state: Ref[CacheState]): FieldWrapper[Any] = new FieldWrapper[Any](false) {
      override def wrap[R1](
        query: ZQuery[R1, CalibanError.ExecutionError, ResponseValue],
        info: FieldInfo
      ): ZQuery[R1, CalibanError.ExecutionError, ResponseValue] =
        query.mapZIO { result =>
          cacheOverride.get.flatMap {
            case Some(overrideValue) =>
              state.update(s => s.copy(policy = s.policy.restrict(Some(overrideValue)))) as result
            case None                => ZIO.succeed(result)
          }
        }
    }

    private def overallWrapper(state: Ref[CacheState]): OverallWrapper[Any] = new OverallWrapper[Any] {
      override def wrap[R1](
        f: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] = { (request: GraphQLRequest) =>
        (f(request) zipWith state.get) { (response, cacheState) =>
          val cacheControl = cacheState.policy.hint
          val maxAge       = cacheControl.maxAge orElse settings.defaultMaxAge getOrElse 0.seconds
          val scope        = cacheControl.scope orElse settings.defaultScope getOrElse CacheScope.Public
          response.copy(
            extensions = Some(
              ResponseValue.ObjectValue(
                response.extensions.foldLeft[List[(String, ResponseValue)]](
                  List(
                    "cacheControl" -> ResponseValue.ObjectValue(
                      List(
                        "version" -> IntValue(2),
                        "policy"  -> ResponseValue.ObjectValue(
                          List(
                            "maxAge" -> IntValue(maxAge.toSeconds),
                            "scope"  -> StringValue(scope.toString)
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

  def setCacheHint(hint: CacheHint): UIO[Unit] = cacheOverride.set(Some(hint))

  case class CachePolicy(hint: CacheHint) {
    def merge(that: CachePolicy): CachePolicy =
      CachePolicy(mostRestrictive(hint, that.hint))

    def restrict(restricted: Option[CacheHint]): CachePolicy = restricted match {
      case Some(h) => copy(hint = mostRestrictive(hint, h))
      case None    => this
    }

    private def mostRestrictive(
      parent: Caching.CacheHint,
      current: Caching.CacheHint
    ): Caching.CacheHint = {
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

  private def extractCacheDirective(directives: List[Directive]): Option[CacheHint] =
    directives.collectFirst {
      case d if d.name == directiveName =>
        val scope = d.arguments.get("scope").collectFirst {
          case StringValue("PRIVATE") | EnumValue("PRIVATE") => CacheScope.Private
          case StringValue("PUBLIC") | EnumValue("PUBLIC")   => CacheScope.Public
        }

        val maxAge = d.arguments.get("maxAge").collectFirst { case i: IntValue =>
          Duration(i.toLong, TimeUnit.SECONDS)
        }

        val inheritMaxAge = d.arguments
          .get("inheritMaxAge")
          .collectFirst { case b: BooleanValue =>
            b.value
          }
          .getOrElse(false)

        CacheHint(maxAge, scope, inheritMaxAge)
    }

  private case class CacheState(
    policy: CachePolicy = CachePolicy(CacheHint.default)
  )

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

    implicit val schema: Schema[Any, CacheScope] = Schema.gen
  }

  private val cacheOverride: FiberRef[Option[CacheHint]] = Unsafe.unsafe(implicit u => FiberRef.unsafe.make(None))

}
