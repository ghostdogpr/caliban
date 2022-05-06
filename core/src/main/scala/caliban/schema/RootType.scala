package caliban.schema

import caliban.introspection.adt.{ __Directive, __Type }
import caliban.schema.Types.collectTypes
import caliban.introspection.adt.__TypeKind

case class RootType(
  queryType: __Type,
  mutationType: Option[__Type],
  subscriptionType: Option[__Type],
  additionalTypes: List[__Type] = List.empty,
  additionalDirectives: List[__Directive] = List.empty
) {
  val types: Map[String, __Type] = {
    val init = additionalTypes.foldLeft(List.empty[__Type]) { case (acc, t) => collectTypes(t, acc) }
    (init ++
      collectTypes(queryType, init) ++
      mutationType.fold(List.empty[__Type])(collectTypes(_, init)) ++
      subscriptionType.fold(List.empty[__Type])(collectTypes(_, init)))
      .groupBy(t => (t.name, t.kind, t.origin))
      .flatMap(_._2.headOption)
      .map(t => t.name.getOrElse("") -> t)
      .toMap
  }
}
