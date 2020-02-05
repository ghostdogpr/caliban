package zquery

import zio.{ IO, Ref, UIO }

/**
 * A `Cache` maintains an internal state with a mapping from requests to `Ref`s
 * that will contain the result of those requests when they are executed. This
 * is used internally by the library to provide deduplication and caching of
 * requests.
 */
final class Cache private (private val state: Ref[Map[Any, Any]]) {

  /**
   * Inserts a request and a `Ref` that will contain the result of the request
   * when it is executed into the cache.
   */
  def insert[E, A](request: Request[E, A], result: Ref[Option[Either[E, A]]]): UIO[Unit] =
    state.update(_ + (request -> result)).unit

  /**
   * Looks up a request in the cache, failing with the unit value if the
   * request is not in the cache, succeeding with `Ref(None)` if the request is
   * in the cache but has not been executed yet, or `Ref(Some(value))` if the
   * request has been executed.
   */
  def lookup[E, A](request: Request[E, A]): IO[Unit, Ref[Option[Either[E, A]]]] =
    state.get.map(_.get(request).asInstanceOf[Option[Ref[Option[Either[E, A]]]]]).get
}

object Cache {

  /**
   * Constructs an empty cache.
   */
  val empty: UIO[Cache] =
    Ref.make(Map.empty[Any, Any]).map(new Cache(_))
}
