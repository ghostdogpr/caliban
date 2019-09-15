package caliban.schema

import caliban.schema.RootSchema.Operation

case class RootSchema[Query, Mutation, Subscription](
  query: Operation[Query],
  mutation: Option[Operation[Mutation]],
  subscription: Option[Operation[Subscription]]
)

object RootSchema {

  case class Operation[T](schema: Schema[T], resolver: T)

}
