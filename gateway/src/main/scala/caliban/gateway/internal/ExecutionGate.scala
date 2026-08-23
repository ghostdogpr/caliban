package caliban.gateway.internal

import caliban.gateway.GatewayRuntime.AdmissionStatus
import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayWrapper.{ AdmissionKind, Event, Result }
import zio.{ Semaphore, Trace, UIO, ZIO }

private[gateway] final class ExecutionGate private (
  limit: Int,
  semaphore: Semaphore,
  kind: Option[AdmissionKind]
) {

  def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
    semaphore.withPermit(effect)

  def observed[R, E, A](wrapper: GatewayWrapper[R])(effect: ZIO[R, E, A])(implicit
    trace: Trace
  ): ZIO[R, E, A] =
    if (!wrapper.enabled) apply(effect)
    else
      kind match {
        case Some(value) =>
          ZIO.scoped[R] {
            wrapper.wrap(Event.AdmissionWait(value))(semaphore.withPermitScoped)(
              Result.classifyExit
            ) *>
              wrapper.wrap(Event.Admission(value))(effect)(
                Result.classifyExit
              )
          }
        case None        => apply(effect)
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
  def make(limit: Int)(implicit trace: Trace): UIO[ExecutionGate] =
    Semaphore.make(limit.toLong).map(new ExecutionGate(limit, _, None))

  def make(limit: Int, kind: AdmissionKind)(implicit trace: Trace): UIO[ExecutionGate] =
    Semaphore.make(limit.toLong).map(new ExecutionGate(limit, _, Some(kind)))
}
