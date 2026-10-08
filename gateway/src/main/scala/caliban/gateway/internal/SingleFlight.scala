package caliban.gateway.internal

import zio.{ FiberId, Promise, Trace, Unsafe, ZIO }

import java.util.concurrent.ConcurrentHashMap

/**
 * Shares one in-flight computation between concurrent callers with the same key. The first caller runs it on its own
 * fiber and the others wait for its result; when that caller is interrupted, a waiting caller runs it again.
 */
private[gateway] final class SingleFlight[K, E, A] {
  private val running = new ConcurrentHashMap[K, Promise[E, A]]

  def apply[R](key: K)(effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
    ZIO.uninterruptibleMask { restore =>
      ZIO.suspendSucceed {
        val fresh  = Unsafe.unsafe(implicit unsafe => Promise.unsafe.make[E, A](FiberId.None))
        val leader = running.putIfAbsent(key, fresh)
        if (leader eq null)
          restore(effect).onExit(exit => ZIO.succeed(running.remove(key, fresh)) *> fresh.done(exit))
        else
          restore(leader.await.catchSomeCause { case cause if cause.isInterruptedOnly => apply(key)(effect) })
      }
    }
}
