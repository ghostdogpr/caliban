package caliban.schema

import caliban.schema.Types.{ __Type, collectTypes }

case class RootType(queryType: __Type, mutationType: Option[__Type], subscriptionType: Option[__Type]) {
  val types: Map[String, __Type] =
    collectTypes(queryType) ++
      mutationType.map(collectTypes(_)).getOrElse(Map()) ++
      subscriptionType.map(collectTypes(_)).getOrElse(Map())
}
