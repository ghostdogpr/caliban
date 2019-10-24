package epstein

import zio.Ref

/**
 * A `BlockedRequest[A]` keeps track of a request of type `A` along with a
 * `Ref` containing the result of the request, existentially hiding the result
 * type. This is used internally by the library to support data sources that
 * return different result types for different requests while guaranteeing that
 * results will be of the type requested.
 */
private[epstein] sealed trait BlockedRequest[+A] {
  type Value

  def request: Request[Value]

  def result: Ref[Option[Value]]
}

private[epstein] object BlockedRequest {

  def apply[A, B](request0: A, result0: Ref[Option[B]])(
    implicit ev: A <:< Request[B]
  ): BlockedRequest[A] =
    new BlockedRequest[A] {
      type Value = B

      val request = request0

      val result = result0
    }
}
