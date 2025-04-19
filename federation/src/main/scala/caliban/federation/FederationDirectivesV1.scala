package caliban.federation

import caliban.Value.StringValue
import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectivesV1 {

  case class GQLKey(fields: String) extends GQLDirective(Key(fields))

  object Key {
    def apply(fields: String, name: String = "key"): Directive =
      Directive(name, Map("fields" -> StringValue(fields)))
  }
}
