package caliban.federation.v2x

import caliban.InputValue.ListValue
import caliban.Value.StringValue
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectivesV2_6 extends FederationDirectivesV2_5 {

  def Policy(policies: List[List[String]]) =
    Directive("policy", Map("policies" -> ListValue(policies.map(s => ListValue(s.map(s => StringValue(s)))))))

  case class GQLPolicy(policies: List[List[String]]) extends GQLDirective(Policy(policies))

}
