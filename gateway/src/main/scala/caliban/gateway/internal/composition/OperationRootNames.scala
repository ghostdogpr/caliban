package caliban.gateway.internal.composition

import caliban.schema.RootType

private[gateway] final case class OperationRootNames private (entries: List[(String, String)]) {
  private val composedBySource = entries.groupBy(_._2).map { case (source, values) => source -> values.map(_._1) }
  private val sourceByComposed = entries.toMap

  val sourceNames: Set[String] = entries.iterator.map(_._2).toSet

  def source(composed: String): Option[String] = sourceByComposed.get(composed)

  def composed(source: String): String = composedAll(source).headOption.getOrElse(source)

  def composedAll(source: String): List[String] = composedBySource.getOrElse(source, Nil)

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
