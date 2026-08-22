package caliban.execution

import caliban.parsing.adt.OperationType
import caliban.parsing.adt.OperationType.Query

case class ExecutionRequest(
  field: Field,
  operationType: OperationType,
  operationName: Option[String]
) {
  private[caliban] def hasIntrospection: Boolean =
    operationType == Query && field.fields.exists(isIntrospectionField)

  private[caliban] def isIntrospection: Boolean =
    operationType == Query && field.fields.nonEmpty && field.fields.forall(isIntrospectionField)
}
