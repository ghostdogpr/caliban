package caliban

import caliban.schema.Types.{ collectTypes, Type }

case class RootType(queryType: Type, mutationType: Option[Type], subscriptionType: Option[Type]) {
  val types: Map[String, Type] =
    collectTypes(queryType) ++
      mutationType.map(collectTypes(_)).getOrElse(Map()) ++
      subscriptionType.map(collectTypes(_)).getOrElse(Map())
}
