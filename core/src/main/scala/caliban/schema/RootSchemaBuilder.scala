package caliban.schema

import caliban.introspection.adt.{ __Type, TypeVisitor }
import caliban.parsing.adt.Directive

case class RootSchemaBuilder[-R](
  query: Option[Operation[R]],
  mutation: Option[Operation[R]],
  subscription: Option[Operation[R]],
  additionalTypes: List[__Type] = Nil,
  schemaDirectives: List[Directive] = Nil,
  schemaDescription: Option[String] = None
) {
  def |+|[R1 <: R](that: RootSchemaBuilder[R1]): RootSchemaBuilder[R1] =
    RootSchemaBuilder(
      (query ++ that.query).reduceOption(_ |+| _),
      (mutation ++ that.mutation).reduceOption(_ |+| _),
      (subscription ++ that.subscription).reduceOption(_ |+| _),
      (additionalTypes ++ that.additionalTypes),
      schemaDirectives ++ that.schemaDirectives,
      schemaDescription orElse that.schemaDescription
    )

  def types: List[__Type] =
    Types.collectRootTypes(
      additionalTypes,
      query.map(_.opType),
      mutation.map(_.opType),
      subscription.map(_.opType)
    )

  def visit(visitor: TypeVisitor): RootSchemaBuilder[R] =
    copy(
      query = query.map(query => query.copy(opType = visitor.visit(query.opType))),
      mutation = mutation.map(mutation => mutation.copy(opType = visitor.visit(mutation.opType))),
      subscription = subscription.map(subscription => subscription.copy(opType = visitor.visit(subscription.opType))),
      additionalTypes = additionalTypes.map(visitor.visit)
    )
}
