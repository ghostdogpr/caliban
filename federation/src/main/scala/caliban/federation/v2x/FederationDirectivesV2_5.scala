package caliban.federation.v2x

import caliban.InputValue.ListValue
import caliban.Value.StringValue
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectivesV2_5 extends FederationDirectivesV2_3 {

  val Authenticated                              = Directive("authenticated")
  def RequiresScopes(scopes: List[List[String]]) =
    Directive("requiresScopes", Map("scopes" -> ListValue(scopes.map(s => ListValue(s.map(s => StringValue(s)))))))
  case class GQLAuthenticated() extends GQLDirective(Authenticated)

  case class GQLRequiresScopes(scopes: List[List[String]]) extends GQLDirective(RequiresScopes(scopes))

}
