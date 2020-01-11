package zquery

/**
 * A `CompletedRequestMap` is a universally quantified mapping from requests
 * of type `Request[E, A]` to results of type `Either[E, A[` for all types `E`
 * and `A`. The guarantee is that for any request of type `Request[E, A]`, if
 * there is a corresponding value in the map, that value is of type
 * `Either[E, A]`. This is used by the library to support data sources that
 * return different result types for different requests while guaranteeing that
 * results will be of the type requested.
 */
final class CompletedRequestMap private (private val map: Map[Any, Either[Any, Any]]) { self =>

  def ++(that: CompletedRequestMap): CompletedRequestMap =
    new CompletedRequestMap(self.map ++ that.map)

  /**
   * Appends the specified result to the completed requests map.
   */
  def insert[E, A](request: Request[E, A])(result: Either[E, A]): CompletedRequestMap =
    new CompletedRequestMap(self.map + (request -> result))

  /**
   * Retrieves the result of the specified request if it exists.
   */
  def lookup[E, A](request: Request[E, A]): Option[Either[E, A]] =
    map.get(request).asInstanceOf[Option[Either[E, A]]]
}

object CompletedRequestMap {

  /**
   * An empty completed requests map.
   */
  val empty: CompletedRequestMap =
    new CompletedRequestMap(Map.empty)
}
