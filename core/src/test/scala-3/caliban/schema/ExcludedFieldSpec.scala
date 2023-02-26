package caliban.schema

import caliban.schema.Annotations.GQLExcluded

/** Compile-time "test", which ensures that we don't need to define a schema for fields annotated with @GQLExcluded */
object ExcludedFieldSpec {
  final case class Foo(value: String)
  final case class Bar(value: String, @GQLExcluded foo: Foo, num: Int)

  given Schema[Any, Bar] = Schema.derived
}
