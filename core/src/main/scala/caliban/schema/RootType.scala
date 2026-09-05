package caliban.schema

import caliban.introspection.adt.{ __Directive, __Type }

case class RootType(
  queryType: __Type,
  mutationType: Option[__Type],
  subscriptionType: Option[__Type],
  additionalTypes: List[__Type] = List.empty,
  additionalDirectives: List[__Directive] = List.empty,
  description: Option[String] = None
) {
  private val primitiveTypes: List[__Type] = List(Types.boolean, Types.int, Types.float, Types.string)

  val types: Map[String, __Type] =
    Types
      .collectRootTypes(additionalTypes, Some(queryType), mutationType, subscriptionType, primitiveTypes)
      .map(t => t.name.getOrElse("") -> t)
      .toMap
}
