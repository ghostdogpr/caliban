package caliban.gateway.internal

import caliban.gateway.GatewayInterpreter.AdmissionStatus
import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayWrapper.{ AdmissionKind, Event, Result }
import zio.{ Exit, Scope, Semaphore, Trace, UIO, URIO, ZEnvironment, ZIO }

private[gateway] final class AdmissionGate private (
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
          val waiting = semaphore.withPermitScoped.provideEnvironment(ZEnvironment(permitScope))
          wrapper.wrap[Scope with R, Nothing, Unit](Event.AdmissionWait(kind))(waiting)(Result.classifyExit) *>
            wrapper.wrap(Event.Admission(kind))(effect)(Result.classifyExit)
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

private[gateway] object AdmissionGate {
  def make(limit: Int, kind: AdmissionKind)(implicit trace: Trace): UIO[AdmissionGate] =
    Semaphore.make(limit.toLong).map(new AdmissionGate(limit, _, kind))
}
