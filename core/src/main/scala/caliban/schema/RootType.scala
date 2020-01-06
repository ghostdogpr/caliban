package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.Types.collectTypes

case class RootType(queryType: __Type, mutationType: Option[__Type], subscriptionType: Option[__Type]) {
  val empty = Map.empty[String, __Type]
  val types: Map[String, __Type] =
    collectTypes(queryType) ++
      mutationType.fold(empty)(collectTypes(_)) ++
      subscriptionType.fold(empty)(collectTypes(_))
}
