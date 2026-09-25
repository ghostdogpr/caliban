package caliban.gateway.internal

import caliban.gateway.{ GatewaySubscriptionConfig, PhaseHooks }
import zio.{ Clock, Duration, Exit, Promise, Ref, Scope, Trace, UIO, ZIO }

private[gateway] final class GatewayExecutionControl[-R] private (
  val subscriptions: SubscriptionControl[R],
  requestTimeout: Duration,
  drainTimeout: Duration,
  state: Ref[GatewayExecutionControl.State],
  drained: Promise[Nothing, Unit],
  forceStop: Promise[Nothing, Unit]
) {
  import GatewayExecutionControl._

  def withLease[R0, E, A](onRejected: => ZIO[R0, E, A])(body: Lease => ZIO[R0, E, A])(implicit
    trace: Trace
  ): ZIO[R0, E, A] =
    ZIO.acquireReleaseWith(reserve)(ZIO.foreachDiscard(_)(_ => release))(_.fold(onRejected)(body))

  def runWithin[R0, E, A](lease: Lease)(effect: ZIO[R0, E, A])(implicit trace: Trace): ZIO[R0, E, Option[A]] =
    Clock.nanoTime.zip(forceStop.isDone).flatMap { case (now, stopped) =>
      val remaining = requestTimeout.toNanos - (now - lease.startedAt)
      if (stopped) ZIO.interrupt
      else if (remaining <= 0L) ZIO.none
      else {
        val stop =
          Clock.sleep(Duration.fromNanos(remaining)).as(Stop.Deadline).raceFirst(forceStop.await.as(Stop.Drain))

        effect
          .map(Some(_))
          .raceWith[R0, Nothing, E, Stop, Option[A]](stop)(
            (exit, stopFiber) => stopFiber.interrupt *> (exit: ZIO[R0, E, Option[A]]),
            (exit, workFiber) =>
              workFiber.interrupt.uninterruptible *> (exit match {
                case Exit.Success(Stop.Deadline) => ZIO.none
                case Exit.Success(Stop.Drain)    => ZIO.interrupt
                case Exit.Failure(cause)         => ZIO.failCause(cause)
              })
          )
      }
    }

  // Reservation is coordinated with generation selection by the reload supervisor.
  def reserve(implicit trace: Trace): UIO[Option[Lease]] =
    Clock.nanoTime.flatMap { startedAt =>
      state.modify { current =>
        if (current.drainStartedAt.isEmpty)
          Some(new Lease(startedAt)) -> current.copy(leases = current.leases + 1)
        else None                    -> current
      }
    }

  def release(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      val next   = current.copy(leases = current.leases - 1)
      val signal = next.drainStartedAt.nonEmpty && next.leases == 0
      signal -> next
    }.flatMap(signal => drained.succeed(()).unit.when(signal).unit)

  private def close(implicit trace: Trace): UIO[Unit] =
    // Shut down requests and subscriptions in parallel so each starts its drain timeout immediately.
    closeRequests.zipPar(subscriptions.close).unit.uninterruptible

  private def closeRequests(implicit trace: Trace): UIO[Unit] =
    (for {
      startedAt <- Clock.nanoTime
      empty     <- state.modify { current =>
                     val next = current.copy(drainStartedAt = Some(startedAt))
                     (next.leases == 0) -> next
                   }
      _         <- drained.succeed(()).unit.when(empty)
      done      <- drained.await.interruptible.timeout(drainTimeout).map(_.isDefined)
      _         <- (forceStop.succeed(()).unit *> drained.await).unless(done)
    } yield ()).uninterruptible

}

private[gateway] object GatewayExecutionControl {
  def make[R](
    subscriptionConfig: GatewaySubscriptionConfig,
    hooks: PhaseHooks[R],
    requestTimeout: Duration,
    drainTimeout: Duration
  )(implicit trace: Trace): ZIO[Scope, Nothing, GatewayExecutionControl[R]] =
    for {
      subscriptions <- SubscriptionControl.make(subscriptionConfig, hooks)
      state         <- Ref.make(State(0, None))
      drained       <- Promise.make[Nothing, Unit]
      forceStop     <- Promise.make[Nothing, Unit]
      control        =
        new GatewayExecutionControl(
          subscriptions,
          requestTimeout,
          drainTimeout,
          state,
          drained,
          forceStop
        )
      _             <- ZIO.addFinalizer(control.close)
    } yield control

  final class Lease(val startedAt: Long)

  private final case class State(leases: Int, drainStartedAt: Option[Long])

  private sealed trait Stop
  private object Stop {
    case object Deadline extends Stop
    case object Drain    extends Stop
  }
}
