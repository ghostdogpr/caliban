package caliban.gateway.internal

import caliban.gateway.GatewayRuntime.AdmissionStatus
import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayWrapper.{ AdmissionKind, Event, Result }
import zio.{ Scope, Semaphore, Trace, UIO, URIO, ZIO }

private[gateway] final class ExecutionGate private (
  limit: Int,
  semaphore: Semaphore,
  kind: AdmissionKind
) {

  def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
    semaphore.withPermit(effect)

  def observed[R, E, A](wrapper: GatewayWrapper[R])(effect: ZIO[R, E, A])(implicit
    trace: Trace
  ): ZIO[R, E, A] =
    if (!wrapper.enabled) apply(effect)
    else {
      val waiting: URIO[Scope with R, Unit] =
        wrapper.wrap(Event.AdmissionWait(kind))(semaphore.withPermitScoped)(Result.classifyExit)

      ZIO.scoped[R] {
        waiting *> wrapper.wrap(Event.Admission(kind))(effect)(Result.classifyExit)
      }
    }

  def status(implicit trace: Trace): UIO[AdmissionStatus] =
    semaphore.available.zipWith(semaphore.awaiting) { (available, waiting) =>
      AdmissionStatus(
        limit,
        active = limit - available.toInt,
        waiting = waiting.toInt
      )
    }
}

private[gateway] object ExecutionGate {
  def make(limit: Int, kind: AdmissionKind)(implicit trace: Trace): UIO[ExecutionGate] =
    Semaphore.make(limit.toLong).map(new ExecutionGate(limit, _, kind))
}
