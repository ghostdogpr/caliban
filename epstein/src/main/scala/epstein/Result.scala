package epstein

/**
 * A `Result[R, E, A]` is the result of running one step of a `ZQuery`. A
 * result may either by done with a value `A` or blocked on a set of requests
 * to data sources that require an environment `R`, may fail with an `E`, and
 * may succeed with an `A`.
 */
private[epstein] sealed trait Result[-R, +E, +A]

private[epstein] object Result {

  /**
   * Constructs a result that is blocked on the specified requests with the
   * specified continuation.
   */
  final def blocked[R, E, A](blockedRequests: BlockedRequestMap[R, E], continue: ZQuery[R, E, A]): Result[R, E, A] =
    Blocked(blockedRequests, continue)

  /**
   * Constructs a result that is done with the specified value.
   */
  final def done[A](value: A): Result[Any, Nothing, A] =
    Done(value)

  final case class Blocked[-R, +E, +A](blockedRequests: BlockedRequestMap[R, E], continue: ZQuery[R, E, A])
      extends Result[R, E, A]

  final case class Done[A](value: A) extends Result[Any, Nothing, A]
}
