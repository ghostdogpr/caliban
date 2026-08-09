package caliban.federation.v2x

import caliban.Value.StringValue
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectivesV2_7 { self: FederationDirectivesV2_6 =>
  case class GQLProgressiveOverride(from: String, label: String) extends GQLDirective(ProgressiveOverride(from, label))

  object ProgressiveOverride {
    def apply(from: String, label: String): Directive =
      Directive("override", Map("from" -> StringValue(from), "label" -> StringValue(label)))
  }
}
