package caliban.schema

import caliban.introspection.adt.{ __Directive, __Type }
import caliban.schema.Types.collectTypes

case class RootType(
  queryType: __Type,
  mutationType: Option[__Type],
  subscriptionType: Option[__Type],
  additionalDirectives: List[__Directive] = List.empty
) {
  val empty                      = List.empty[__Type]
  val types: Map[String, __Type] =
    (mutationType.toList ++ subscriptionType.toList)
      .foldLeft(collectTypes(queryType)) { case (existingTypes, tpe) => collectTypes(tpe, existingTypes) }
      .map(t => t.name.getOrElse("") -> t)
      .toMap
}
