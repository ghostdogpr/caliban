package zquery

import zio.{ Ref, UIO }

/**
 * A `Cache` maintains an internal state with a mapping from requests to `Ref`s
 * that will contain the result of those requests when they are executed. This
 * is used internally by the library to provide deduplication and caching of
 * requests.
 */
class Cache private (private val state: Ref[Map[Any, Any]]) {

  /**
   * Inserts a request and a `Ref` that will contain the result of the request
   * when it is executed into the cache.
   */
  final def insert[A](request: Request[A], result: Ref[Option[A]]): UIO[Unit] =
    state.update(_ + (request -> result)).unit

  /**
   * Looks up a request in the cache, returning `None` if the request is not in
   * the cache, `Some(Ref(None))` if the request is in the cache but has not
   * been executed yet, or `Some(Ref(Some(value)))` if the request has been
   * executed.
   */
  final def lookup[A](request: Request[A]): UIO[Option[Ref[Option[A]]]] =
    state.get.map(_.get(request).asInstanceOf[Option[Ref[Option[A]]]])
}

object Cache {

  /**
   * Constructs an empty cache.
   */
  val empty: UIO[Cache] =
    Ref.make(Map.empty[Any, Any]).map(new Cache(_))
}
