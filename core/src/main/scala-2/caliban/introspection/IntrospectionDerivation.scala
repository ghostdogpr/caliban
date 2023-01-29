package caliban.introspection

import caliban.introspection.adt.{ __Introspection, __Type }
import caliban.schema.Schema
import caliban.schema.auto._

trait IntrospectionDerivation {
  implicit lazy val typeSchema: Schema[Any, __Type] = Schema.genAll

  val introspectionSchema: Schema[Any, __Introspection] = Schema.genAll
}
