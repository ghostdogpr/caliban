package caliban.federation.v2x

import caliban.Value.StringValue
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectivesV2_12 extends FederationDirectivesV2_11 {

  def CacheTag(format: String): Directive = Directive("cacheTag", Map("format" -> StringValue(format)))

  case class GQLCacheTag(format: String) extends GQLDirective(CacheTag(format))

}
