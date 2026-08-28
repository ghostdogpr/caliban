package caliban.gateway

import zio.{ Trace, UIO }

import java.time.Instant

/**
 * A stable interpreter whose acquired schemas are refreshed within its owning scope.
 * `status` aggregates work and admission limits across active and retiring generations, with the active generation's
 * operation cache. Aggregate counts saturate at `Int.MaxValue`. Limits remain per interpreter: summed capacity is not
 * transferable between generations.
 */
trait ReloadableGatewayInterpreter[-R] extends GatewayInterpreter[R] {

  /**
   * Returns the refresh phase, generation status, and bounded diagnostics for the latest failed refresh.
   */
  def reloadStatus(implicit trace: Trace): UIO[ReloadableGatewayInterpreter.Status]

  def generationSubscriptions(implicit trace: Trace): UIO[List[ReloadableGatewayInterpreter.GenerationSubscriptions]]
}

object ReloadableGatewayInterpreter {
  final case class GenerationSubscriptions(id: Long, retiring: Boolean, status: GatewayInterpreter.SubscriptionStatus)
  sealed trait Phase
  object Phase {
    case object Idle     extends Phase
    case object Checking extends Phase
    case object Building extends Phase
    case object Draining extends Phase
    case object Closing  extends Phase
    case object Closed   extends Phase
  }

  sealed trait FailureStage
  object FailureStage {
    case object Acquisition  extends FailureStage
    case object Construction extends FailureStage
    case object Internal     extends FailureStage
  }

  /**
   * Bounded diagnostics. Remote messages, schemas, response bodies and exception causes are never retained.
   */
  final case class Failure(stage: FailureStage, reason: String, subgraphs: List[String], at: Instant)

  final case class Generation(id: Long, activatedAt: Instant, status: GatewayInterpreter.Status)

  final case class Status(
    phase: Phase,
    active: Generation,
    retiring: Option[Generation],
    lastAttemptAt: Option[Instant],
    lastSuccessfulCheckAt: Instant,
    lastFailure: Option[Failure],
    retirementStartedAt: Option[Instant],
    retirementOverdue: Boolean
  )
}
