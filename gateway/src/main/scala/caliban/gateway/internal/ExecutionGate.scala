package caliban.gateway.internal

import caliban.gateway.GatewayRuntime.AdmissionStatus
import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayWrapper.{ AdmissionKind, Event, Result }
import zio.{ Exit, Scope, Semaphore, Trace, UIO, URIO, ZEnvironment, ZIO }

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
    else
      ZIO.scoped[R] {
        ZIO.acquireRelease(Scope.make)(_.close(Exit.unit)).flatMap { permitScope =>
          val waiting: UIO[Unit] = semaphore.withPermitScoped.provideEnvironment(ZEnvironment(permitScope))
          wrapper.wrap[Any, Nothing, Unit](Event.AdmissionWait(kind))(waiting)(Result.classifyExit) *>
            wrapper.wrap[R, E, A](Event.Admission(kind))(effect)(Result.classifyExit)
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
