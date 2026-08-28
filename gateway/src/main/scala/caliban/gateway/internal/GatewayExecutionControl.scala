package caliban.gateway.internal

import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayWrapper.{ AdmissionKind, Event, Result }
import caliban.gateway.internal.execution.SubgraphExecutor
import zio.{ Clock, Duration, Exit, Promise, Ref, Scope, Trace, UIO, URIO, ZIO }

private[gateway] final class GatewayExecutionControl private (
  requests: AdmissionGate,
  subgraphs: Map[String, AdmissionGate],
  requestTimeout: Duration,
  drainTimeout: Duration,
  state: Ref[GatewayExecutionControl.State],
  drained: Promise[Nothing, Unit],
  forceStop: Promise[Nothing, Unit],
  subscriptionDrain: Ref[UIO[Unit]]
) {
  import GatewayExecutionControl._

  def onSubscriptionClose(effect: UIO[Unit])(implicit trace: Trace): UIO[Unit] = subscriptionDrain.set(effect)

  def subscriptionWork[R, E, A](kind: AdmissionKind, wrapper: GatewayWrapper[R])(effect: ZIO[R, E, A])(implicit
    trace: Trace
  ): ZIO[R, E, A] =
    requests.observedAs(kind, wrapper)(effect)

  def runRequest[R, E, A](effect: ZIO[R, E, A], reservation: Option[Lease] = None)(
    onTimeout: => ZIO[R, E, A]
  )(
    onRejected: => ZIO[R, E, A]
  )(implicit trace: Trace): ZIO[R, E, A] =
    ZIO.uninterruptibleMask { restore =>
      reservation.fold(reserve)(ZIO.some(_)).flatMap {
        case Some(lease) =>
          restore(run(lease, requests(effect)).flatMap(_.fold(onTimeout)(ZIO.succeed(_))))
            .ensuring(end(lease.token))
        case None        => restore(onRejected)
      }
    }

  def runObservedRequest[R, B, A](wrapper: GatewayWrapper[R], event: Event.Request, reservation: Option[Lease] = None)(
    prepare: URIO[R, B]
  )(
    isFinite: B => Boolean
  )(
    execute: B => URIO[R, A]
  )(
    onTimeout: => URIO[R, A]
  )(
    onRejected: => URIO[R, A]
  )(result: Exit[Nothing, A] => Result)(implicit trace: Trace): URIO[R, A] =
    ZIO.uninterruptibleMask { restore =>
      def observe(effect: URIO[R, A]): URIO[R, A] = wrapper.wrap(event)(effect)(result)
      reservation.fold(reserve)(ZIO.some(_)).flatMap {
        case None        => restore(observe(onRejected))
        case Some(lease) =>
          // Preparation has its own routing observation. Classify the resolved operation before
          // opening finite-request metrics/spans, but retain one deadline and drain lease for both phases.
          restore(run(lease, requests(prepare)).flatMap {
            case None           => observe(onTimeout)
            case Some(prepared) =>
              val finite   = isFinite(prepared)
              val work     = if (finite) requests.observed(wrapper)(execute(prepared)) else requests(execute(prepared))
              val response = run(lease, work).flatMap(_.fold(onTimeout)(ZIO.succeed(_)))
              if (finite) observe(response) else response
          }).ensuring(end(lease.token))
      }
    }

  def admitExecutor[R](name: String, executor: SubgraphExecutor[R], wrapper: GatewayWrapper[R]): SubgraphExecutor[R] =
    subgraphs.get(name).fold(executor)(executor.admittedBy(_, wrapper))

  // Reservation is coordinated with generation selection by the reload supervisor.
  def reserve(implicit trace: Trace): UIO[Option[Lease]] =
    Clock.nanoTime.flatMap { startedAt =>
      val lease = Lease(new Token, startedAt)
      state.modify { current =>
        if (current.drainStartedAt.isEmpty)
          Some(lease) -> current.copy(requests = current.requests + lease.token)
        else None     -> current
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
            effectiveStop(stop).flatMap {
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

  // Must stay idempotent: request execution and the reload supervisor both end the same token, including on cancellation.
  def release(lease: Lease)(implicit trace: Trace): UIO[Unit] = end(lease.token)

  private def end(token: Token)(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      val next   = current.copy(requests = current.requests - token)
      val signal = next.drainStartedAt.nonEmpty && next.requests.isEmpty
      signal -> next
    }
      .flatMap(signal => drained.succeed(()).unit.when(signal).unit)

  private def close(implicit trace: Trace): UIO[Unit] =
    // Neither lifetime may postpone the other's admission closure or cancellation deadline.
    closeRequests.zipPar(subscriptionDrain.get.flatten).unit.uninterruptible

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
  def make(
    requestLimit: Int,
    subgraphLimits: Map[String, Int],
    requestTimeout: Duration,
    drainTimeout: Duration
  )(implicit trace: Trace): ZIO[Scope, Nothing, GatewayExecutionControl] =
    for {
      requests          <- AdmissionGate.make(requestLimit, AdmissionKind.Request)
      subgraphs         <- ZIO.foreach(subgraphLimits) { case (name, limit) =>
                             AdmissionGate.make(limit, AdmissionKind.Subgraph).map(name -> _)
                           }
      state             <- Ref.make(State(Set.empty, None))
      drained           <- Promise.make[Nothing, Unit]
      forceStop         <- Promise.make[Nothing, Unit]
      subscriptionDrain <- Ref.make[UIO[Unit]](ZIO.unit)
      control            =
        new GatewayExecutionControl(
          requests,
          subgraphs,
          requestTimeout,
          drainTimeout,
          state,
          drained,
          forceStop,
          subscriptionDrain
        )
      _                 <- ZIO.addFinalizer(control.close)
    } yield control

  private[gateway] final class Token

  private[gateway] final case class Lease(token: Token, startedAt: Long)

  private final case class State(
    requests: Set[Token],
    drainStartedAt: Option[Long]
  )

  private sealed trait Stop
  private object Stop {
    case object Deadline extends Stop
    case object Drain    extends Stop
  }
}
