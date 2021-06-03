package caliban.execution

import caliban.parsing.adt.{ OperationType, VariableDefinition }

case class ExecutionRequest(
  field: Field,
  operationType: OperationType
)
