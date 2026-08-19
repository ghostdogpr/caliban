package caliban.gateway.internal

import caliban.gateway.GatewayRuntime.AdmissionStatus
import zio.{ Semaphore, Trace, UIO, ZIO }

private[gateway] final class ExecutionGate private (
  limit: Int,
  semaphore: Semaphore
) {

  def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
    semaphore.withPermit(effect)

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
    Semaphore.make(limit.toLong).map(new ExecutionGate(limit, _))
}
