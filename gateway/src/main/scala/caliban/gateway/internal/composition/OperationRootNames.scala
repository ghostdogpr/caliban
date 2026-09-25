package caliban.gateway.internal.composition

import caliban.schema.RootType

private[gateway] final case class OperationRootNames private (entries: List[(String, String)]) {
  private val composedBySource = entries.map(_.swap).toMap
  private val sourceByComposed = entries.toMap

  val sourceNames: Set[String] = composedBySource.keySet

  def source(composed: String): Option[String] = sourceByComposed.get(composed)

  def composed(source: String): String = composedBySource.getOrElse(source, source)

  def mapSource(f: String => String): OperationRootNames = OperationRootNames(entries.map { case (operation, source) =>
    operation -> f(source)
  })
}

private[gateway] object OperationRootNames {
  def apply(rootType: RootType): OperationRootNames =
    OperationRootNames(
      List(
        rootType.queryType.name.map("Query" -> _),
        rootType.mutationType.flatMap(_.name).map("Mutation" -> _),
        rootType.subscriptionType.flatMap(_.name).map("Subscription" -> _)
      ).flatten
    )
}
