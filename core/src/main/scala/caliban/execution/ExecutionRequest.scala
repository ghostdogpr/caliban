package caliban.execution

import caliban.parsing.adt.OperationType
import caliban.parsing.adt.OperationType.Query

case class ExecutionRequest(
  field: Field,
  operationType: OperationType,
  operationName: Option[String]
) {
  private[caliban] def hasIntrospection: Boolean =
    operationType == Query && field.fields.exists(ExecutionRequest.isIntrospectionField)

  private[caliban] def isIntrospection: Boolean =
    operationType == Query && field.fields.nonEmpty && field.fields.forall(ExecutionRequest.isIntrospectionField)
}

object ExecutionRequest {
  private[caliban] def isIntrospectionField(field: Field): Boolean =
    field.name == "__schema" || field.name == "__type"

  private[caliban] def isMetaField(field: Field): Boolean =
    isIntrospectionField(field) || field.name == "__typename"
}
