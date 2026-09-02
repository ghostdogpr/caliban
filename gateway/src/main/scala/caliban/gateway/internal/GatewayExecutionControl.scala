package caliban.gateway.internal

import caliban.gateway.{ GatewaySubscriptionConfig, GatewayWrapper }
import caliban.gateway.GatewayWrapper.{ AdmissionKind, Event, Result }
import zio.{ Clock, Duration, Exit, Promise, Ref, Scope, Trace, UIO, URIO, ZIO }

private[gateway] final class GatewayExecutionControl[-R] private (
  requests: AdmissionGate[R],
  wrapper: GatewayWrapper[R],
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
  )(
    onRejected: => ZIO[R0, E, A]
  )(implicit trace: Trace): ZIO[R0, E, A] =
    leased(reservation)(onRejected) { lease =>
      run(lease, requests(effect)).flatMap(_.fold(onTimeout)(ZIO.succeed(_)))
    }

  def runObservedRequest[R1 <: R, B, A](event: Event.Request, reservation: Option[Lease] = None)(
    prepare: URIO[R1, B]
  )(
    isFinite: B => Boolean
  )(
    execute: B => URIO[R1, A]
  )(
    onTimeout: => URIO[R1, A]
  )(
    onRejected: => URIO[R1, A]
  )(result: Exit[Nothing, A] => Result)(implicit trace: Trace): URIO[R1, A] = {
    def observe(effect: URIO[R1, A]): URIO[R1, A] = wrapper.wrap(event)(effect)(result)
    leased(reservation)(observe(onRejected)) { lease =>
      // Classify the resolved operation before opening finite-request metrics/spans, while one
      // admission permit, deadline, and drain lease cover preparation and execution together.
      ZIO.scoped[R1] {
        run(lease, requests.acquire).flatMap {
          case None    => observe(onTimeout)
          case Some(_) =>
            run(lease, prepare).flatMap {
              case None           => observe(onTimeout)
              case Some(prepared) =>
                val finite   = isFinite(prepared)
                val work     = if (finite) requests.observe(execute(prepared)) else execute(prepared)
                val response = run(lease, work).flatMap(_.fold(onTimeout)(ZIO.succeed(_)))
                if (finite) observe(response) else response
            }
        }
      }
    }
  }

  // Reservation is coordinated with generation selection by the reload supervisor.
  def reserve(implicit trace: Trace): UIO[Option[Lease]] =
    Clock.nanoTime.flatMap { startedAt =>
      val lease = new Lease(startedAt)
      state.modify { current =>
        if (current.drainStartedAt.isEmpty)
          Some(lease) -> current.copy(requests = current.requests + lease)
        else None     -> current
      }
    }

  private def leased[R, E, A](reservation: Option[Lease])(
    onRejected: => ZIO[R, E, A]
  )(
    body: Lease => ZIO[R, E, A]
  )(implicit trace: Trace): ZIO[R, E, A] =
    ZIO.uninterruptibleMask { restore =>
      reservation.fold(reserve)(ZIO.some(_)).flatMap {
        case Some(lease) => restore(body(lease)).ensuring(end(lease))
        case None        => restore(onRejected)
      }
    }

  private def run[R, E, A](lease: Lease, effect: ZIO[R, E, A])(implicit
    trace: Trace
  ): ZIO[R, E, Option[A]] =
    currentStop(lease).flatMap {
      case Some(Stop.Deadline) => ZIO.none
      case Some(Stop.Drain)    => ZIO.interrupt
      case None                => race(lease, effect)
    }

  private def race[R, E, A](lease: Lease, effect: ZIO[R, E, A])(implicit
    trace: Trace
  ): ZIO[R, E, Option[A]] = {
    val stop: ZIO[R, E, Stop]      = stopAt(lease)
    val work: ZIO[R, E, Option[A]] = effect.map(Some(_))

    work.raceWith(stop)(
      (exit, stopFiber) => stopFiber.interrupt *> (exit: ZIO[Any, E, Option[A]]),
      (exit, workFiber) =>
        exit match {
          case Exit.Success(stop)  =>
            stop match {
              case Stop.Deadline =>
                workFiber.interrupt.uninterruptible *>
                  effectiveStop(Stop.Deadline).flatMap {
                    case Stop.Deadline => ZIO.none
                    case Stop.Drain    => ZIO.interrupt
                  }
              case Stop.Drain    =>
                workFiber.interrupt.uninterruptible *> ZIO.interrupt
            }
          case Exit.Failure(cause) => workFiber.interrupt.uninterruptible *> ZIO.failCause(cause)
        }
    )
  }

  private def stopAt(lease: Lease)(implicit trace: Trace): UIO[Stop] =
    remaining(lease).flatMap { value =>
      val deadline =
        if (value <= 0L) ZIO.succeed(Stop.Deadline) else Clock.sleep(Duration.fromNanos(value)).as(Stop.Deadline)
      deadline.raceFirst(forceStop.await.as(Stop.Drain)).flatMap(effectiveStop)
    }

  private def currentStop(lease: Lease)(implicit trace: Trace): UIO[Option[Stop]] =
    drainExpired.flatMap {
      case true  => ZIO.some(Stop.Drain)
      case false => remaining(lease).map(value => if (value <= 0L) Some(Stop.Deadline) else None)
    }

  private def effectiveStop(stop: Stop)(implicit trace: Trace): UIO[Stop] =
    drainExpired.map(if (_) Stop.Drain else stop)

  private def remaining(lease: Lease)(implicit trace: Trace): UIO[Long] =
    Clock.nanoTime.map(now => requestTimeout.toNanos - (now - lease.startedAt))

  private def drainExpired(implicit trace: Trace): UIO[Boolean] =
    Clock.nanoTime.zipWith(state.get) { (now, current) =>
      current.drainStartedAt.exists(startedAt => now - startedAt >= drainTimeout.toNanos)
    }

  // Must stay idempotent: request execution and the reload supervisor both end the same lease, including on cancellation.
  def release(lease: Lease)(implicit trace: Trace): UIO[Unit] = end(lease)

  private def end(lease: Lease)(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      val next   = current.copy(requests = current.requests - lease)
      val signal = next.drainStartedAt.nonEmpty && next.requests.isEmpty
      signal -> next
    }
      .flatMap(signal => drained.succeed(()).unit.when(signal).unit)

  private def close(implicit trace: Trace): UIO[Unit] =
    // Neither lifetime may postpone the other's admission closure or cancellation deadline.
    closeRequests.zipPar(subscriptions.close).unit.uninterruptible

  private def closeRequests(implicit trace: Trace): UIO[Unit] =
    (for {
      startedAt <- Clock.nanoTime
      empty     <- state.modify { current =>
                     val next = current.copy(
                       drainStartedAt = Some(startedAt)
                     )
                     next.requests.isEmpty -> next
                   }
      _         <- drained.succeed(()).unit.when(empty)
      done      <- drained.await.interruptible.timeout(drainTimeout).map(_.isDefined)
      _         <- (forceStop.succeed(()).unit *> drained.await).unless(done)
    } yield ()).uninterruptible

}

private[gateway] object GatewayExecutionControl {
  def make[R](
    requestLimit: Int,
    subscriptionConfig: GatewaySubscriptionConfig,
    wrapper: GatewayWrapper[R],
    requestTimeout: Duration,
    drainTimeout: Duration
  )(implicit trace: Trace): ZIO[Scope, Nothing, GatewayExecutionControl[R]] =
    for {
      requests      <- AdmissionGate.make(requestLimit, AdmissionKind.Request, wrapper)
      subscriptions <- SubscriptionControl.make(subscriptionConfig, requests, wrapper)
      state         <- Ref.make(State(Set.empty, None))
      drained       <- Promise.make[Nothing, Unit]
      forceStop     <- Promise.make[Nothing, Unit]
      control        =
        new GatewayExecutionControl(
          requests,
          wrapper,
          subscriptions,
          requestTimeout,
          drainTimeout,
          state,
          drained,
          forceStop
        )
      _             <- ZIO.addFinalizer(control.close)
    } yield control

  private[gateway] final class Lease(val startedAt: Long)

  private final case class State(
    requests: Set[Lease],
    drainStartedAt: Option[Long]
  )

  private sealed trait Stop
  private object Stop {
    case object Deadline extends Stop
    case object Drain    extends Stop
  }
}
