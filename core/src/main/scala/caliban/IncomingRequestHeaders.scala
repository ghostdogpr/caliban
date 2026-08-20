package caliban

import zio.{ FiberRef, UIO, Unsafe, ZIO }

private[caliban] object IncomingRequestHeaders {
  private val current: FiberRef[List[(String, String)]] =
    Unsafe.unsafe(implicit unsafe => FiberRef.unsafe.make(Nil))

  def get: UIO[List[(String, String)]] = current.get

  def locally[R, E, A](headers: List[(String, String)])(effect: ZIO[R, E, A]): ZIO[R, E, A] =
    current.locally(headers)(effect)
}
