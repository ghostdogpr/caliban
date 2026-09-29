package caliban.gateway

import caliban.CalibanError
import caliban.execution.ExecutionRequest
import caliban.parsing.adt.{ Document, OperationType }

/**
 * Everything observable about one operation, delivered to the `operation` phase once per request.
 *
 * `prepared` is absent whenever the request never reached execution: a preparation failure, a
 * timeout, a shutdown, or an interruption. That traffic is still reported rather than dropped. A handler that needs
 * timing brackets it itself, keeping the start in its own `Ctx0`.
 */
final case class OperationEvent(
  prepared: Option[OperationEvent.Prepared],
  errors: List[CalibanError],
  outcome: PhaseHooks.Outcome
) {
  def operationType: Option[OperationType] = prepared.map(_.executionRequest.operationType)

  def result: PhaseHooks.Result = PhaseHooks.Result(outcome, operationType, errors.size)
}

object OperationEvent {
  final case class Prepared(document: Document, executionRequest: ExecutionRequest)
}
