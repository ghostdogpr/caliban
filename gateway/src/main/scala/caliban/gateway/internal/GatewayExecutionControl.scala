package caliban.gateway.internal

import zio.stm.{ TRef, USTM }
import zio.{ Clock, Duration, Fiber, IO, Scope, Trace, UIO, ZIO }

import java.util.concurrent.ConcurrentHashMap

private[gateway] final class GatewayExecutionControl private (
  requestTimeout: Duration,
  drainTimeout: Duration,
  state: TRef[GatewayExecutionControl.State]
) {
  private val running           = ConcurrentHashMap.newKeySet[Fiber.Runtime[_, _]]()
  @volatile private var stopped = false

  final class Lease private[GatewayExecutionControl] (startedAt: Long) {
    def runWithin[R0, E, A](effect: ZIO[R0, E, A])(onTimeout: => IO[E, A])(implicit trace: Trace): ZIO[R0, E, A] =
      Clock.nanoTime.flatMap { now =>
        val timedOut = ZIO.suspendSucceed(if (stopped) ZIO.interrupt else onTimeout)
        ZIO.uninterruptibleMask { restore =>
          restore(effect).fork.flatMap { work =>
            ZIO.succeed {
              running.add(work)
              if (stopped) Deadline.expire(work)
            } *> restore(
              Deadline.await(work, Duration.fromNanos(requestTimeout.toNanos - (now - startedAt)), timedOut)
            ).ensuring(ZIO.succeed(running.remove(work)))
          }
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
      _         <- drained.interruptible.timeout(drainTimeout).someOrElseZIO(forceStop *> drained)
    } yield ()).uninterruptible

  private def forceStop: UIO[Unit] =
    ZIO.succeed {
      stopped = true
      running.forEach(Deadline.expire(_))
    }

}

private[gateway] object GatewayExecutionControl {
  def make(requestTimeout: Duration, drainTimeout: Duration)(implicit
    trace: Trace
  ): ZIO[Scope, Nothing, GatewayExecutionControl] =
    for {
      state  <- TRef.make(State(0, None)).commit
      control = new GatewayExecutionControl(requestTimeout, drainTimeout, state)
      _      <- ZIO.addFinalizer(control.closeRequests)
    } yield control

  private final case class State(leases: Int, drainStartedAt: Option[Long])
}
