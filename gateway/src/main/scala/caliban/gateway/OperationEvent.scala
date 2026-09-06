package caliban.gateway

import caliban.CalibanError
import caliban.execution.ExecutionRequest
import caliban.parsing.adt.{ Document, OperationType }

/**
 * Everything observable about one operation, delivered to the `observeOperation` phase once per request.
 *
 * `document` and `executionRequest` are absent whenever the request never reached execution: a preparation failure, a
 * timeout, a shutdown, or an interruption. That traffic is still reported rather than dropped. A handler that needs
 * timing brackets it itself, keeping the start in its own `Ctx0`.
 */
final case class OperationEvent(
  document: Option[Document],
  executionRequest: Option[ExecutionRequest],
  operationType: Option[OperationType],
  errors: List[CalibanError],
  outcome: PhaseHooks.Outcome
)
