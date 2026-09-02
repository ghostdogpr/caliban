package caliban.gateway.internal

import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayWrapper.{ AdmissionKind, Event, Result }
import zio.{ Scope, Semaphore, Trace, UIO, ZIO }

private[gateway] final class AdmissionGate[-R] private (
  semaphore: Semaphore,
  kind: AdmissionKind,
  wrapper: GatewayWrapper[R]
) {

  def apply[R0, E, A](effect: ZIO[R0, E, A])(implicit trace: Trace): ZIO[R0, E, A] =
    semaphore.withPermit(effect)

  def acquire(implicit trace: Trace): ZIO[Scope, Nothing, Unit] =
    semaphore.withPermitScoped

  def observed[R1 <: R, E, A](effect: ZIO[R1, E, A])(implicit
    trace: Trace
  ): ZIO[R1, E, A] =
    observedAs(kind)(effect)

  def observe[R1 <: R, E, A](effect: ZIO[R1, E, A])(implicit
    trace: Trace
  ): ZIO[R1, E, A] =
    observeAs(kind)(effect)

  def observedAs[R1 <: R, E, A](work: AdmissionKind)(
    effect: ZIO[R1, E, A]
  )(implicit trace: Trace): ZIO[R1, E, A] =
    apply(observeAs(work)(effect))

  private def observeAs[R1 <: R, E, A](work: AdmissionKind)(
    effect: ZIO[R1, E, A]
  )(implicit trace: Trace): ZIO[R1, E, A] =
    if (!wrapper.enabled) effect else wrapper.wrap(Event.Admission(work))(effect)(Result.classifyExit)

}

private[gateway] object AdmissionGate {
  def make[R](limit: Int, kind: AdmissionKind, wrapper: GatewayWrapper[R])(implicit
    trace: Trace
  ): UIO[AdmissionGate[R]] =
    Semaphore.make(limit.toLong).map(new AdmissionGate(_, kind, wrapper))
}
