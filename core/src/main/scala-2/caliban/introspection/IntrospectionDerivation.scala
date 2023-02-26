package caliban.introspection

import caliban.introspection.adt.{ __Introspection, __Type }
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema
import caliban.schema.Schema.auto._

trait IntrospectionDerivation {
  implicit lazy val typeSchema: Schema[Any, __Type] = genAll

  val introspectionSchema: Schema[Any, __Introspection] = genAll
}
