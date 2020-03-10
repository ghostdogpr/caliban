package caliban.schema

import caliban.introspection.adt.__Type

case class Operation[-R](opType: __Type, plan: Step[R]) {
  def |+|[R1 <: R](that: Operation[R1]): Operation[R1] =
    Operation(opType |+| that.opType, Step.mergeRootSteps(plan, that.plan))
}
