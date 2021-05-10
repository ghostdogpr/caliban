package caliban.introspection

import caliban.introspection.adt.{ __Introspection, __Type }
import caliban.schema.Schema

trait IntrospectionDerivation {
  implicit lazy val typeSchema: Schema[Any, __Type] = Schema.gen[__Type]

  val introspectionSchema: Schema[Any, __Introspection] = Schema.gen[__Introspection]
}
