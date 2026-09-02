package caliban

import zio.{ FiberRef, Scope, UIO, Unsafe, ZIO }

private[caliban] object IncomingRequestHeaders {
  private final class LazyHeaders(load: => List[(String, String)]) {
    lazy val headers: List[(String, String)] = load
  }

  private val current: FiberRef[LazyHeaders] =
    Unsafe.unsafe(implicit unsafe => FiberRef.unsafe.make(new LazyHeaders(Nil)))

  def get: UIO[List[(String, String)]] = current.get.map(_.headers)

  def locallyScoped(headers: => List[(String, String)]): ZIO[Scope, Nothing, Unit] =
    current.locallyScoped(new LazyHeaders(headers))

  def locally[R, E, A](headers: => List[(String, String)])(effect: ZIO[R, E, A]): ZIO[R, E, A] =
    current.locally(new LazyHeaders(headers))(effect)
}
