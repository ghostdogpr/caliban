package caliban.federation.v2x

import caliban.Value.StringValue
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectivesV2_7 extends FederationDirectivesV2_6 {

  def ProgressiveOverride(from: String, label: String): Directive =
    Directive("override", Map("from" -> StringValue(from), "label" -> StringValue(label)))

  case class GQLProgressiveOverride(from: String, label: String) extends GQLDirective(ProgressiveOverride(from, label))
}
