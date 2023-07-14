package caliban.schema

import caliban.introspection.adt.__Type
import caliban.parsing.adt.Directive
import caliban.schema.Types.collectTypes

case class RootSchemaBuilder[-R](
  query: Option[Operation[R]],
  mutation: Option[Operation[R]],
  subscription: Option[Operation[R]],
  additionalTypes: List[__Type] = Nil,
  schemaDirectives: List[Directive] = Nil
) {
  def |+|[R1 <: R](that: RootSchemaBuilder[R1]): RootSchemaBuilder[R1] =
    RootSchemaBuilder(
      (query ++ that.query).reduceOption(_ |+| _),
      (mutation ++ that.mutation).reduceOption(_ |+| _),
      (subscription ++ that.subscription).reduceOption(_ |+| _),
      additionalTypes ++ that.additionalTypes,
      schemaDirectives ++ that.schemaDirectives
    )

  def types: List[__Type] = {
    val init = additionalTypes.foldLeft(List.empty[__Type]) { case (acc, t) => collectTypes(t, acc) }
    (init ++
      query.map(_.opType).fold(List.empty[__Type])(collectTypes(_, init)) ++
      mutation.map(_.opType).fold(List.empty[__Type])(collectTypes(_, init)) ++
      subscription.map(_.opType).fold(List.empty[__Type])(collectTypes(_, init)))
      .groupBy(t => (t.name, t.kind, t.origin))
      .flatMap(_._2.headOption)
      .toList
  }

  def transformTypes(f: __Type => __Type): RootSchemaBuilder[R] =
    copy(
      query = query.map(query => query.copy(opType = query.opType.transform(f))),
      mutation = mutation.map(mutation => mutation.copy(opType = mutation.opType.transform(f))),
      subscription = subscription.map(subscription => subscription.copy(opType = subscription.opType.transform(f))),
      additionalTypes = additionalTypes.map(_.transform(f))
    )
}
