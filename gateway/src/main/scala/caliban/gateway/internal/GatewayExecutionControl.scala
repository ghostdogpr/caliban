package caliban.gateway.internal

import caliban.gateway.PhaseHooks.{ Event, Result }
import caliban.gateway.{ GatewaySubscriptionConfig, PhaseHooks }
import zio.{ Clock, Duration, Exit, Promise, Ref, Scope, Trace, UIO, URIO, ZIO }

private[gateway] final class GatewayExecutionControl[-R] private (
  hooks: PhaseHooks[R],
  val subscriptions: SubscriptionControl[R],
  requestTimeout: Duration,
  drainTimeout: Duration,
  state: Ref[GatewayExecutionControl.State],
  drained: Promise[Nothing, Unit],
  forceStop: Promise[Nothing, Unit]
) {
  import GatewayExecutionControl._

  def runRequest[R0, E, A](effect: ZIO[R0, E, A], reservation: Option[Lease] = None)(
    onTimeout: => ZIO[R0, E, A]
  )(onRejected: => ZIO[R0, E, A])(implicit trace: Trace): ZIO[R0, E, A] =
    withLease(reservation)(onRejected) { lease =>
      run(lease, effect).flatMap(_.fold(onTimeout)(ZIO.succeed(_)))
    }

  def runObservedRequest[R1 <: R, B, A](event: Event.Execution, reservation: Option[Lease] = None)(
    prepare: URIO[R1, B]
  )(isFinite: B => Boolean)(execute: B => URIO[R1, A])(
    onTimeout: => URIO[R1, A]
  )(onRejected: => URIO[R1, A])(result: Exit[Nothing, A] => Result)(implicit trace: Trace): URIO[R1, A] = {
    def observe(effect: URIO[R1, A]): URIO[R1, A] = hooks.execution.run(event)(effect)(result)
    withLease(reservation)(observe(onRejected)) { lease =>
      // Classify the resolved operation before opening finite-request metrics/spans, while one
      // deadline and drain lease cover preparation and execution together.
      run(lease, prepare).flatMap {
        case None           => observe(onTimeout)
        case Some(prepared) =>
          val response = run(lease, execute(prepared)).flatMap(_.fold(onTimeout)(ZIO.succeed(_)))
          if (isFinite(prepared)) observe(response) else response
      }
    }
  }

  // Reservation is coordinated with generation selection by the reload supervisor.
  def reserve(implicit trace: Trace): UIO[Option[Lease]] =
    Clock.nanoTime.flatMap { startedAt =>
      val lease = new Lease(startedAt)
      state.modify { current =>
        if (current.drainStartedAt.isEmpty)
          Some(lease) -> current.copy(leases = current.leases + lease)
        else None     -> current
      }
    }

  // Must stay idempotent: request execution and the reload supervisor both end the same lease, including on cancellation.
  def release(lease: Lease)(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      val next   = current.copy(leases = current.leases - lease)
      val signal = next.drainStartedAt.nonEmpty && next.leases.isEmpty
      signal -> next
    }.flatMap(signal => drained.succeed(()).unit.when(signal).unit)

  private def withLease[R, E, A](
    reservation: Option[Lease]
  )(onRejected: => ZIO[R, E, A])(body: Lease => ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
    ZIO.uninterruptibleMask { restore =>
      reservation.fold(reserve)(ZIO.some(_)).flatMap {
        case Some(lease) => restore(body(lease)).ensuring(release(lease))
        case None        => restore(onRejected)
      }
    }

  private def run[R, E, A](lease: Lease, effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, Option[A]] =
    currentStop(lease).flatMap {
      case Some(Stop.Deadline) => ZIO.none
      case Some(Stop.Drain)    => ZIO.interrupt
      case None                => race(lease, effect)
    }

  private def race[R, E, A](lease: Lease, effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, Option[A]] = {
    val stop: ZIO[R, E, Stop]      = awaitStop(lease)
    val work: ZIO[R, E, Option[A]] = effect.map(Some(_))

    work.raceWith(stop)(
      (exit, stopFiber) => stopFiber.interrupt *> (exit: ZIO[Any, E, Option[A]]),
      (exit, workFiber) =>
        exit match {
          case Exit.Success(Stop.Deadline) =>
            workFiber.interrupt.uninterruptible *>
              effectiveStop(Stop.Deadline).flatMap {
                case Stop.Deadline => ZIO.none
                case Stop.Drain    => ZIO.interrupt
              }
          case Exit.Success(Stop.Drain)    => workFiber.interrupt.uninterruptible *> ZIO.interrupt
          case Exit.Failure(cause)         => workFiber.interrupt.uninterruptible *> ZIO.failCause(cause)
        }
    )
  }

  private def awaitStop(lease: Lease)(implicit trace: Trace): UIO[Stop] =
    remainingNanos(lease).flatMap { value =>
      val deadline =
        if (value <= 0L) ZIO.succeed(Stop.Deadline) else Clock.sleep(Duration.fromNanos(value)).as(Stop.Deadline)
      deadline.raceFirst(forceStop.await.as(Stop.Drain)).flatMap(effectiveStop)
    }

  private def currentStop(lease: Lease)(implicit trace: Trace): UIO[Option[Stop]] =
    drainExpired.flatMap {
      case true  => ZIO.some(Stop.Drain)
      case false => remainingNanos(lease).map(value => if (value <= 0L) Some(Stop.Deadline) else None)
    }

  private def effectiveStop(stop: Stop)(implicit trace: Trace): UIO[Stop] =
    drainExpired.map(if (_) Stop.Drain else stop)

  private def remainingNanos(lease: Lease)(implicit trace: Trace): UIO[Long] =
    Clock.nanoTime.map(now => requestTimeout.toNanos - (now - lease.startedAt))

  private def drainExpired(implicit trace: Trace): UIO[Boolean] =
    Clock.nanoTime.zipWith(state.get) { (now, current) =>
      current.drainStartedAt.exists(startedAt => now - startedAt >= drainTimeout.toNanos)
    }

  private def close(implicit trace: Trace): UIO[Unit] =
    // Shut down requests and subscriptions in parallel so each starts its drain timeout immediately.
    closeRequests.zipPar(subscriptions.close).unit.uninterruptible

  private def closeRequests(implicit trace: Trace): UIO[Unit] =
    (for {
      startedAt <- Clock.nanoTime
      empty     <- state.modify { current =>
                     val next = current.copy(drainStartedAt = Some(startedAt))
                     next.leases.isEmpty -> next
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
      state         <- Ref.make(State(Set.empty, None))
      drained       <- Promise.make[Nothing, Unit]
      forceStop     <- Promise.make[Nothing, Unit]
      control        =
        new GatewayExecutionControl(
          hooks,
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

  private final case class State(leases: Set[Lease], drainStartedAt: Option[Long])

  private sealed trait Stop
  private object Stop {
    case object Deadline extends Stop
    case object Drain    extends Stop
  }
}
