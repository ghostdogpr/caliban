package caliban.execution

import caliban.parsing.adt.OperationType

final case class ExecutionRequest(
  field: Field,
  operationType: OperationType
)
