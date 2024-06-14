package caliban.federation.v2x

import caliban.Value.StringValue
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectivesV2_8 extends FederationDirectivesV2_6 {

  def Context(context: String) = Directive("context", Map("context" -> StringValue(context)))

  case class GQLContext(context: String) extends GQLDirective(Context(context))

  def FromContext(context: String) = Directive("fromContext", Map("context" -> StringValue(context)))

  case class GQLFromContext(context: String) extends GQLDirective(FromContext(context))
}
