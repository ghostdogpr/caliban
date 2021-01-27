package caliban.wrappers

import java.util.concurrent.TimeUnit
import caliban.{ GraphQLRequest, ResponseValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ EnumValue, IntValue, StringValue }
import caliban.parsing.adt.Directive
import caliban.wrappers.Wrapper.{ EffectfulWrapper, FieldWrapper, OverallWrapper }
import zio.Ref
import zio.duration.Duration
import zio.query.ZQuery

/**
 * Returns a wrapper which applies apollo caching response extensions
 */
object ApolloCaching {

  private val directiveName = "cacheControl"

  object CacheControl {

    def apply(scope: ApolloCaching.CacheScope): Directive                   =
      Directive(directiveName, Map("scope" -> EnumValue(scope.toString)))

    def apply(maxAge: Duration): Directive                                  =
      Directive(directiveName, Map("maxAge" -> IntValue(maxAge.toMillis / 1000)))

    def apply(maxAge: Duration, scope: ApolloCaching.CacheScope): Directive =
      Directive(directiveName, Map("maxAge" -> IntValue(maxAge.toMillis / 1000), "scope" -> EnumValue(scope.toString)))

  }

  val apolloCaching: EffectfulWrapper[Any] =
    EffectfulWrapper(
      Ref.make(Caching()).map(ref => apolloCachingOverall(ref) |+| apolloCachingField(ref))
    )

  sealed trait CacheScope

  object CacheScope {

    case object Private extends CacheScope {
      override def toString: String = "PRIVATE"
    }

    case object Public extends CacheScope {
      override def toString: String = "PUBLIC"
    }

  }

  case class CacheHint(
    fieldName: String = "",
    path: List[Either[String, Int]] = Nil,
    maxAge: Duration,
    scope: CacheScope
  ) {

    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "path"   -> ListValue((Left(fieldName) :: path).reverse.map(_.fold(StringValue, IntValue(_)))),
          "maxAge" -> IntValue(maxAge.toMillis / 1000),
          "scope"  -> StringValue(scope match {
            case CacheScope.Private => "PRIVATE"
            case CacheScope.Public  => "PUBLIC"
          })
        )
      )

  }

  case class Caching(
    version: Int = 1,
    hints: List[CacheHint] = List.empty
  ) {

    def toResponseValue: ResponseValue =
      ObjectValue(List("version" -> IntValue(version), "hints" -> ListValue(hints.map(_.toResponseValue))))
  }

  case class CacheDirective(scope: Option[CacheScope] = None, maxAge: Option[Duration] = None)

  private def extractCacheDirective(directives: List[Directive]): Option[CacheDirective] =
    directives.collectFirst {
      case d if d.name == directiveName =>
        val scope = d.arguments.get("scope").collectFirst {
          case StringValue("PRIVATE") | EnumValue("PRIVATE") => CacheScope.Private
          case StringValue("PUBLIC") | EnumValue("PUBLIC")   => CacheScope.Public
        }

        val maxAge = d.arguments.get("maxAge").collectFirst { case i: IntValue =>
          Duration(i.toLong, TimeUnit.SECONDS)
        }

        CacheDirective(scope, maxAge)
    }

  private def apolloCachingOverall(ref: Ref[Caching]): OverallWrapper[Any] =
    OverallWrapper { process => (request: GraphQLRequest) =>
      for {
        result <- process(request)
        cache  <- ref.get
      } yield result.copy(
        extensions = Some(
          ObjectValue(
            ("cacheControl" -> cache.toResponseValue) :: result.extensions.fold(
              List.empty[(String, ResponseValue)]
            )(_.fields)
          )
        )
      )
    }

  private def apolloCachingField(ref: Ref[Caching]): FieldWrapper[Any] =
    FieldWrapper(
      { case (query, fieldInfo) =>
        val cacheDirectives = extractCacheDirective(
          fieldInfo.directives ++ fieldInfo.details.fieldType.ofType.flatMap(_.directives).getOrElse(Nil)
        )

        cacheDirectives.foldLeft(query) { case (q, cacheDirective) =>
          q <* ZQuery.fromEffect(
            ref.update(state =>
              state.copy(
                hints = CacheHint(
                  path = fieldInfo.path,
                  fieldName = fieldInfo.name,
                  maxAge = cacheDirective.maxAge getOrElse Duration.Zero,
                  scope = cacheDirective.scope getOrElse CacheScope.Private
                ) :: state.hints
              )
            )
          )
        }
      },
      wrapPureValues = true
    )

}
