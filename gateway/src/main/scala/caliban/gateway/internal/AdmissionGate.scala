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
    observedAs(kind, wrapper)(effect)

  def observedAs[R, E, A](work: AdmissionKind, wrapper: GatewayWrapper[R])(
    effect: ZIO[R, E, A]
  )(implicit trace: Trace): ZIO[R, E, A] =
    if (!wrapper.enabled) apply(effect)
    else
      // Only the permit is scoped here. Replacing a caller's Scope would finalize
      // subscription sources and layers as soon as their setup effect returned.
      ZIO.acquireReleaseWith(Scope.make)(_.close(Exit.unit)) { permitScope =>
        val waiting = semaphore.withPermitScoped.provideEnvironment(ZEnvironment(permitScope))
        wrapper.wrap[R, Nothing, Unit](Event.AdmissionWait(work))(waiting)(Result.classifyExit) *>
          wrapper.wrap(Event.Admission(work))(effect)(Result.classifyExit)
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
