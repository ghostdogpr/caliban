package zquery

import zio.Ref

/**
 * A `BlockedRequest[A]` keeps track of a request of type `A` along with a
 * `Ref` containing the result of the request, existentially hiding the result
 * type. This is used internally by the library to support data sources that
 * return different result types for different requests while guaranteeing that
 * results will be of the type requested.
 */
private[zquery] sealed trait BlockedRequest[+A] {
  type Failure
  type Success

  def request: Request[Failure, Success]

  def result: Ref[Option[Either[Failure, Success]]]
}

private[zquery] object BlockedRequest {

  def apply[E, A, B](request0: A, result0: Ref[Option[Either[E, B]]])(
    implicit ev: A <:< Request[E, B]
  ): BlockedRequest[A] =
    new BlockedRequest[A] {
      type Failure = E
      type Success = B

      val request = request0

      val result = result0
    }
}
