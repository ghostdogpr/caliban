package epstein

/**
 * A `CompletedRequestMap` is a universally quantified mapping from requests
 * of type `Request[A]` to results of type `A` for all types `A`. The
 * guarantee is that for any request of type `Request[A]`, if there is a
 * corresponding value in the map, that value is of type `A`. This is used by
 * the library to support data sources that return different result types for
 * different requests while guaranteeing that results will be of the type
 * requested.
 */
final class CompletedRequestMap private (private val map: Map[Any, Any]) { self =>

  final def ++(that: CompletedRequestMap): CompletedRequestMap =
    new CompletedRequestMap(self.map ++ that.map)

  /**
   * Appends the specified result to the completed requests map.
   */
  final def insert[A](request: Request[A])(result: A): CompletedRequestMap =
    new CompletedRequestMap(self.map + (request -> result))

  /**
   * Retrieves the result of the specified request if it exists.
   */
  final def lookup[A](request: Request[A]): Option[A] =
    map.get(request).asInstanceOf[Option[A]]
}

object CompletedRequestMap {

  /**
   * An empty completed requests map.
   */
  val empty: CompletedRequestMap =
    new CompletedRequestMap(Map.empty)
}
