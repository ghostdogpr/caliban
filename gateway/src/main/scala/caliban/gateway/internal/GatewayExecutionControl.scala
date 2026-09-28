package caliban.gateway.internal

import zio.stm.{ TRef, USTM }
import zio.{ Clock, Duration, Promise, Scope, Trace, UIO, ZIO }

private[gateway] final class GatewayExecutionControl private (
  requestTimeout: Duration,
  drainTimeout: Duration,
  state: TRef[GatewayExecutionControl.State],
  forceStop: Promise[Nothing, Unit]
) {
  final class Lease private[GatewayExecutionControl] (startedAt: Long) {
    def runWithin[R0, E, A](effect: ZIO[R0, E, A])(implicit trace: Trace): ZIO[R0, E, Option[A]] =
      Clock.nanoTime.zip(forceStop.isDone).flatMap { case (now, stopped) =>
        val remaining = requestTimeout.toNanos - (now - startedAt)
        if (stopped) ZIO.interrupt
        else if (remaining <= 0L) ZIO.none
        else {
          val stop: UIO[UIO[Option[A]]] =
            Clock.sleep(Duration.fromNanos(remaining)).as(ZIO.none).raceFirst(forceStop.await.as(ZIO.interrupt))

          effect
            .map(Some(_))
            .raceWith[R0, Nothing, E, UIO[Option[A]], Option[A]](stop)(
              (exit, stopFiber) => stopFiber.interrupt *> (exit: ZIO[R0, E, Option[A]]),
              (exit, workFiber) => workFiber.interrupt.uninterruptible *> exit.flatten
            )
        }
      }

    def release(implicit trace: Trace): UIO[Unit] =
      state.update(current => current.copy(leases = current.leases - 1)).commit
  }

  // Reservation is coordinated with generation selection by the reload supervisor.
  def reserve(startedAt: Long): USTM[Option[Lease]] =
    state.modify { current =>
      if (current.drainStartedAt.isEmpty)
        Some(new Lease(startedAt)) -> current.copy(leases = current.leases + 1)
      else None                    -> current
    }

  private def drained(implicit trace: Trace): UIO[Unit] =
    state.get.retryUntil(_.leases == 0).unit.commit

  private def closeRequests(implicit trace: Trace): UIO[Unit] =
    (for {
      startedAt <- Clock.nanoTime
      _         <- state.update(_.copy(drainStartedAt = Some(startedAt))).commit
      _         <- drained.interruptible.timeout(drainTimeout).someOrElseZIO(forceStop.succeed(()) *> drained)
    } yield ()).uninterruptible

}

private[gateway] object GatewayExecutionControl {
  def make(requestTimeout: Duration, drainTimeout: Duration)(implicit
    trace: Trace
  ): ZIO[Scope, Nothing, GatewayExecutionControl] =
    for {
      state     <- TRef.make(State(0, None)).commit
      forceStop <- Promise.make[Nothing, Unit]
      control    = new GatewayExecutionControl(requestTimeout, drainTimeout, state, forceStop)
      _         <- ZIO.addFinalizer(control.closeRequests)
    } yield control

  private final case class State(leases: Int, drainStartedAt: Option[Long])
}
