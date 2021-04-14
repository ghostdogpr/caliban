package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.Types.collectTypes

case class RootSchemaBuilder[-R](
  query: Option[Operation[R]],
  mutation: Option[Operation[R]],
  subscription: Option[Operation[R]],
  additionalTypes: List[__Type] = Nil
) {
  def |+|[R1 <: R](that: RootSchemaBuilder[R1]): RootSchemaBuilder[R1] =
    RootSchemaBuilder(
      (query ++ that.query).reduceOption(_ |+| _),
      (mutation ++ that.mutation).reduceOption(_ |+| _),
      (subscription ++ that.subscription).reduceOption(_ |+| _)
    )

  def types: List[__Type] = {
    val empty = additionalTypes
    (query.map(_.opType).fold(empty)(collectTypes(_)) ++
      mutation.map(_.opType).fold(empty)(collectTypes(_)) ++
      subscription.map(_.opType).fold(empty)(collectTypes(_)))
      .groupBy(t => (t.name, t.kind, t.origin))
      .flatMap(_._2.headOption)
      .toList
  }
}
