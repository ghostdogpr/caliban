package caliban.execution

import caliban.parsing.adt.OperationType
import caliban.parsing.adt.OperationType.Query

case class ExecutionRequest(
  field: Field,
  operationType: OperationType,
  operationName: Option[String]
) {
  private[caliban] def hasIntrospection: Boolean =
    operationType == Query && field.fields.exists(field => field.name == "__schema" || field.name == "__type")

  private[caliban] def isIntrospection: Boolean =
    operationType == Query && field.fields.nonEmpty && field.fields.forall(field =>
      field.name == "__schema" || field.name == "__type"
    )
}
