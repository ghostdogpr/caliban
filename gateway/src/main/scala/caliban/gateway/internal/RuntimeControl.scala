package caliban.gateway.internal

import caliban.gateway.GatewayRuntime
import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayRuntime.{ LifecycleStatus, OperationCacheStatus, Status }
import caliban.gateway.GatewayWrapper.{ AdmissionKind, Event, Result }
import zio.{ Clock, Duration, Exit, Promise, Ref, Scope, Trace, UIO, URIO, ZIO }

private[gateway] final class RuntimeControl private (
  requests: ExecutionGate,
  sources: Map[String, ExecutionGate],
  requestTimeout: Duration,
  drainTimeout: Duration,
  state: Ref[RuntimeControl.State],
  drained: Promise[Nothing, Unit],
  forceStop: Promise[Nothing, Unit]
) {
  import RuntimeControl._

  def runRequest[R, E, A](effect: ZIO[R, E, A])(
    onTimeout: => ZIO[R, E, A]
  )(
    onRejected: => ZIO[R, E, A]
  )(implicit trace: Trace): ZIO[R, E, A] =
    withLease(requests(effect), ZIO.unit)(onTimeout)(onRejected)(identity)

  def runObservedRequest[R, E, A](wrapper: GatewayWrapper[R], event: Event.Request)(effect: ZIO[R, E, A])(
    onTimeout: URIO[R, A]
  )(
    onRejected: URIO[R, A]
  )(result: Exit[E, A] => Result)(implicit trace: Trace): ZIO[R, E, A] =
    withLease(
      requests.observed(wrapper)(effect),
      wrapper.wrap(Event.RequestOverdue)(ZIO.unit)(Result.classifyExit)
    )(onTimeout)(onRejected)(value => wrapper.wrap(event)(value)(result))

  private def withLease[R, E, A](
    effect: ZIO[R, E, A],
    onOverdue: URIO[R, Unit]
  )(
    onTimeout: => ZIO[R, E, A]
  )(
    onRejected: => ZIO[R, E, A]
  )(
    observe: ZIO[R, E, A] => ZIO[R, E, A]
  )(implicit trace: Trace): ZIO[R, E, A] =
    ZIO.uninterruptibleMask { restore =>
      begin.flatMap {
        case Some(lease) =>
          restore(observe(run(lease, effect, onOverdue).flatMap(_.fold(onTimeout)(ZIO.succeed(_)))))
            .ensuring(end(lease.token))
        case None        => observe(onRejected)
      }
    }

  def source[R](name: String, source: GraphQLSource[R], wrapper: GatewayWrapper[R]): GraphQLSource[R] =
    sources.get(name).fold(source)(source.admittedBy(_, wrapper))

  def status(cache: OperationCacheStatus)(implicit trace: Trace): UIO[Status] =
    state.get.flatMap { current =>
      requests.status.zipWith(
        ZIO.foreach(sources) { case (name, gate) => gate.status.map(name -> _) }
      ) { (requestStatus, sourceStatus) =>
        GatewayRuntime.Status(
          LifecycleStatus(
            current.lifecycle,
            current.requests.size,
            current.requests.valuesIterator.count(_ == RequestState.Overdue)
          ),
          requestStatus,
          sourceStatus,
          cache
        )
      }
    }

  private def begin(implicit trace: Trace): UIO[Option[Lease]] =
    Clock.nanoTime.flatMap { startedAt =>
      val lease = Lease(new Token, startedAt)
      state.modify { current =>
        current.lifecycle match {
          case GatewayRuntime.LifecycleState.Running =>
            Some(lease) -> current.copy(requests = current.requests.updated(lease.token, RequestState.Active))
          case _                                     => None -> current
        }
      }
    }

  private def run[R, E, A](lease: Lease, effect: ZIO[R, E, A], onOverdue: URIO[R, Unit])(implicit
    trace: Trace
  ): ZIO[R, E, Option[A]] =
    currentStop(lease).flatMap {
      case Some(Stop.Deadline) => markOverdue(lease.token, onOverdue) *> ZIO.none
      case Some(Stop.Drain)    => markOverdue(lease.token, onOverdue) *> ZIO.interrupt
      case None                => race(lease, effect, onOverdue)
    }

  private def race[R, E, A](lease: Lease, effect: ZIO[R, E, A], onOverdue: URIO[R, Unit])(implicit
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
                (markOverdue(lease.token, onOverdue) *> workFiber.interrupt).uninterruptible *>
                  effectiveStop(Stop.Deadline).flatMap {
                    case Stop.Deadline => ZIO.none
                    case Stop.Drain    => ZIO.interrupt
                  }
              case Stop.Drain    =>
                (markOverdue(lease.token, onOverdue) *> workFiber.interrupt).uninterruptible *> ZIO.interrupt
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

  private def markOverdue[R](token: Token, onOverdue: URIO[R, Unit])(implicit trace: Trace): URIO[R, Unit] =
    state.modify { current =>
      current.requests.get(token) match {
        case Some(RequestState.Active) =>
          true -> current.copy(requests = current.requests.updated(token, RequestState.Overdue))
        case _                         => false -> current
      }
    }.flatMap(onOverdue.when(_).unit)

  private def end(token: Token)(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      val next   = current.copy(requests = current.requests - token)
      val signal = next.lifecycle == GatewayRuntime.LifecycleState.Draining && next.requests.isEmpty
      signal -> next
    }
      .flatMap(signal => drained.succeed(()).unit.when(signal).unit)

  private def close(implicit trace: Trace): UIO[Unit] =
    (for {
      startedAt <- Clock.nanoTime
      empty     <- state.modify { current =>
                     val next = current.copy(
                       lifecycle = GatewayRuntime.LifecycleState.Draining,
                       drainStartedAt = Some(startedAt)
                     )
                     next.requests.isEmpty -> next
                   }
      _         <- drained.succeed(()).unit.when(empty)
      done      <- drained.await.interruptible.timeout(drainTimeout).map(_.isDefined)
      _         <- (forceStop.succeed(()).unit *> drained.await).unless(done)
      _         <- state.update(_.copy(lifecycle = GatewayRuntime.LifecycleState.Closed))
    } yield ()).uninterruptible

}

private[gateway] object RuntimeControl {
  def make(
    requestLimit: Int,
    sourceLimits: Map[String, Int],
    requestTimeout: Duration,
    drainTimeout: Duration
  )(implicit trace: Trace): ZIO[Scope, Nothing, RuntimeControl] =
    for {
      requests  <- ExecutionGate.make(requestLimit, AdmissionKind.Request)
      sources   <- ZIO.foreach(sourceLimits) { case (name, limit) =>
                     ExecutionGate.make(limit, AdmissionKind.Source).map(name -> _)
                   }
      state     <- Ref.make(State(GatewayRuntime.LifecycleState.Running, Map.empty, None))
      drained   <- Promise.make[Nothing, Unit]
      forceStop <- Promise.make[Nothing, Unit]
      control    = new RuntimeControl(requests, sources, requestTimeout, drainTimeout, state, drained, forceStop)
      _         <- ZIO.addFinalizer(control.close)
    } yield control

  private final class Token

  private final case class Lease(token: Token, startedAt: Long)

  private final case class State(
    lifecycle: GatewayRuntime.LifecycleState,
    requests: Map[Token, RequestState],
    drainStartedAt: Option[Long]
  )

  private sealed trait RequestState
  private object RequestState {
    case object Active  extends RequestState
    case object Overdue extends RequestState
  }

  private sealed trait Stop
  private object Stop {
    case object Deadline extends Stop
    case object Drain    extends Stop
  }
}
