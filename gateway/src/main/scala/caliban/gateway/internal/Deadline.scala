package caliban.gateway.internal

import zio.{ Cause, Clock, Duration, Exit, Fiber, FiberId, IO, Trace, Unsafe, ZIO }

/**
 * Bounds a fiber with a clock timer. `ZIO#timeout` races the effect against a sleeping fiber, so it forks two fibers
 * per call; a timer that interrupts the effect's own fiber forks one.
 */
private[gateway] object Deadline {

  /**
   * Runs the effect on a child fiber, running `onExpiry` instead when the duration elapses first.
   */
  def run[R, E, A](duration: Duration, onExpiry: IO[E, Nothing])(effect: ZIO[R, E, A])(implicit
    trace: Trace
  ): ZIO[R, E, A] =
    ZIO.uninterruptibleMask(restore => restore(effect).fork.flatMap(fiber => restore(await(fiber, duration, onExpiry))))

  /**
   * Awaits the fiber, expiring it when the duration elapses first and interrupting it as the caller when the wait is
   * interrupted. An expired fiber yields `onExpiry`; any other exit is the fiber's own, with its FiberRefs inherited.
   */
  def await[E, A](fiber: Fiber.Runtime[E, A], duration: Duration, onExpiry: IO[E, A])(implicit
    trace: Trace
  ): IO[E, A] =
    Clock.scheduler.flatMap { scheduler =>
      val cancel = Unsafe.unsafe(implicit unsafe => scheduler.schedule(() => expire(fiber), duration))
      fiber.await.onInterrupt(fiber.interrupt).ensuring(ZIO.succeed(cancel())).flatMap {
        case Exit.Failure(cause) if expired(cause) => onExpiry
        case exit                                  => fiber.inheritAll *> exit
      }
    }

  def expire(fiber: Fiber.Runtime[_, _]): Unit =
    Unsafe.unsafe(implicit unsafe => fiber.unsafe.interrupt(Cause.interrupt(Expiry)))

  def expired(cause: Cause[Any]): Boolean = cause.interruptors.contains(Expiry)

  // Interrupts work cut by a deadline or by a forced stop.
  private val Expiry: FiberId = FiberId(0, 0, Trace.empty)
}
