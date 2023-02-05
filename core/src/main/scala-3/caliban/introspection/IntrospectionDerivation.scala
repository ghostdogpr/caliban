package caliban.introspection

import caliban.InputValue
import caliban.Value.StringValue
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import caliban.schema.Schema
import caliban.schema.auto._
import caliban.schema.ArgBuilder.auto._

trait IntrospectionDerivation {
  import Schema._

  implicit lazy val inputValueSchema: Schema[Any, InputValue]   = Schema.gen
  implicit lazy val typeSchema: Schema[Any, __Type]             = Schema.gen
  implicit lazy val __directiveSchema: Schema[Any, __Directive] = Schema.gen
  val introspectionSchema: Schema[Any, __Introspection]         = Schema.gen
}
