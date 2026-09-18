package caliban.gateway.internal

import caliban.gateway.PhaseHooks
import caliban.gateway.PhaseHooks.{ AdmissionKind, Event, Result }
import zio.{ Scope, Semaphore, Trace, UIO, ZIO }

private[gateway] final class AdmissionGate[-R] private (
  semaphore: Semaphore,
  kind: AdmissionKind,
  hooks: PhaseHooks[R]
) {

  def withPermit[R0, E, A](effect: ZIO[R0, E, A])(implicit trace: Trace): ZIO[R0, E, A] =
    semaphore.withPermit(effect)

  def acquireScoped(implicit trace: Trace): ZIO[Scope, Nothing, Unit] =
    semaphore.withPermitScoped

  def admit[R1 <: R, E, A](effect: ZIO[R1, E, A])(implicit trace: Trace): ZIO[R1, E, A] =
    admitAs(kind)(effect)

  def observe[R1 <: R, E, A](effect: ZIO[R1, E, A])(implicit trace: Trace): ZIO[R1, E, A] =
    observeAs(kind)(effect)

  def admitAs[R1 <: R, E, A](work: AdmissionKind)(effect: ZIO[R1, E, A])(implicit trace: Trace): ZIO[R1, E, A] =
    withPermit(observeAs(work)(effect))

  private def observeAs[R1 <: R, E, A](work: AdmissionKind)(
    effect: ZIO[R1, E, A]
  )(implicit trace: Trace): ZIO[R1, E, A] =
    hooks.admission.run(Event.Admission(work))(effect)(Result.classifyExit)

}

private[gateway] object AdmissionGate {
  def make[R](limit: Int, kind: AdmissionKind, hooks: PhaseHooks[R])(implicit trace: Trace): UIO[AdmissionGate[R]] =
    Semaphore.make(limit.toLong).map(new AdmissionGate(_, kind, hooks))
}
