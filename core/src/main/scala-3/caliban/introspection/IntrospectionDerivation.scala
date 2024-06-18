package caliban.introspection

import caliban.InputValue
import caliban.Value.StringValue
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import caliban.schema.Schema
import caliban.schema.ArgBuilder

trait IntrospectionDerivation {
  private given Schema[Any, __InputValue]     = Schema.derived
  private given Schema[Any, __EnumValue]      = Schema.derived
  private given Schema[Any, __Field]          = Schema.derived
  private given Schema[Any, __Type]           = Schema.derived
  private given Schema[Any, __TypeArgs]       = Schema.derived
  private given Schema[Any, __Schema]         = Schema.derived
  private given Schema[Any, __Directive]      = Schema.derived
  private given Schema[Any, __DeprecatedArgs] = Schema.derived

  // Unions, so we can auto-derive them cheaply
  private given Schema[Any, __TypeKind]          = Schema.Auto.derived
  private given Schema[Any, __DirectiveLocation] = Schema.Auto.derived

  private given ArgBuilder[__TypeArgs]       = ArgBuilder.derived
  private given ArgBuilder[__DeprecatedArgs] = ArgBuilder.derived

  val introspectionSchema: Schema[Any, __Introspection] = Schema.derived
}
