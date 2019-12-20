package caliban.schema

import caliban.introspection.adt.__Type
import caliban.schema.RootSchema.Operation

case class RootSchema[-R](
  query: Operation[R],
  mutation: Option[Operation[R]],
  subscription: Option[Operation[R]]
) {
  def |+|[R1 <: R](that: RootSchema[R1]): RootSchema[R1] =
    RootSchema(
      query |+| that.query,
      (mutation ++ that.mutation).reduceOption(_ |+| _),
      (subscription ++ that.subscription).reduceOption(_ |+| _)
    )
}

object RootSchema {

  case class Operation[-R](opType: __Type, plan: Step[R]) {
    def |+|[R1 <: R](that: Operation[R1]): Operation[R1] =
      Operation(opType |+| that.opType, Step.mergeRootSteps(plan, that.plan))
  }

}
