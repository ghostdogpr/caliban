package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.RootSchema.Operation

case class RootSchema[-R](
  query: Operation[R],
  mutation: Option[Operation[R]],
  subscription: Option[Operation[R]]
)

object RootSchema {

  case class Operation[-R](opType: __Type, plan: Step[R])

}
