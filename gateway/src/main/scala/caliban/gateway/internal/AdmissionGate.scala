package caliban.gateway.internal

import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayWrapper.{ AdmissionKind, Event, Result }
import zio.{ Semaphore, Trace, UIO, ZIO }

private[gateway] final class AdmissionGate private (
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
    else semaphore.withPermit(wrapper.wrap(Event.Admission(work))(effect)(Result.classifyExit))

}

private[gateway] object AdmissionGate {
  def make(limit: Int, kind: AdmissionKind)(implicit trace: Trace): UIO[AdmissionGate] =
    Semaphore.make(limit.toLong).map(new AdmissionGate(_, kind))
}
