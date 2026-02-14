package caliban.federation.v2x

import caliban.ResponseValue
import caliban.Value.StringValue
import caliban.federation.v2x.FederationDirectivesV2_12.Cacheable
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective
import caliban.schema.Extended

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

trait FederationDirectivesV2_12 extends FederationDirectivesV2_11 with CacheableLowPrio {

  def CacheTag(format: String): Directive = Directive("cacheTag", Map("format" -> StringValue(format)))

  case class GQLCacheTag(format: String) extends GQLDirective(CacheTag(format))

  def cacheField[A](field: A)(tags: List[String]): Extended[A] =
    Extended(field, FederationDirectivesV2_12.buildCacheTags(tags))
}

trait CacheableLowPrio {
  implicit lazy val cacheableInstance: Cacheable = Cacheable.instance
}

object FederationDirectivesV2_12 {

  @implicitNotFound(
    """You are using a version of federation that doesn't support caching

Please make sure to import `caliban.federation.v2_12._` or later.
"""
  )
  sealed trait Cacheable {
    def fromTags(tags: List[String]): ResponseValue.ObjectValue
  }

  object Cacheable {
    lazy val instance: Cacheable = new Cacheable {
      def fromTags(tags: List[String]): ResponseValue.ObjectValue =
        ResponseValue.ObjectValue(
          List(
            "apolloEntityCacheTags" -> ResponseValue.ListValue(
              List(ResponseValue.ListValue(tags.map(StringValue(_))))
            )
          )
        )
    }
  }

  private def buildCacheTags(tags: List[String]): ResponseValue.ObjectValue =
    ResponseValue.ObjectValue(
      List(
        "apolloEntityCacheTags" -> ResponseValue.ListValue(
          List(ResponseValue.ListValue(tags.map(StringValue(_))))
        )
      )
    )
}
