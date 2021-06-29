package caliban.introspection

import caliban.Value.StringValue
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import caliban.schema.Schema

trait IntrospectionDerivation {
  implicit lazy val directiveSchema: Schema[Any, Directive]     =
    Schema.scalarSchema("Directive", None, d => StringValue(d.name))
  implicit lazy val typeSchema: Schema[Any, __Type]             = Schema.gen
  implicit lazy val __directiveSchema: Schema[Any, __Directive] = Schema.gen
  val introspectionSchema: Schema[Any, __Introspection]         = Schema.gen
}
