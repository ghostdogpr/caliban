package caliban.execution

import caliban.parsing.adt.OperationType

case class ExecutionRequest(
  field: Field,
  operationType: OperationType
)
