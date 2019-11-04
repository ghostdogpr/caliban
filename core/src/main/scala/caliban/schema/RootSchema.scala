package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.RootSchema.Operation

case class RootSchema[-R, Query, Mutation, Subscription](
  query: Operation[R, Query],
  mutation: Option[Operation[R, Mutation]],
  subscription: Option[Operation[R, Subscription]]
)

object RootSchema {

  case class Operation[-R, T](opType: __Type, plan: Step[R])

}
