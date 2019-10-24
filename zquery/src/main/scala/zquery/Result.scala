package zquery

import zquery.Result._

/**
 * A `Result[R, E, A]` is the result of running one step of a `ZQuery`. A
 * result may either by done with a value `A` or blocked on a set of requests
 * to data sources that require an environment `R`, may fail with an `E`, and
 * may succeed with an `A`.
 */
private[zquery] sealed trait Result[-R, +E, +A] {

  /**
   * Maps the specified function over the successful value of this result.
   */
  final def map[B](f: A => B): Result[R, E, B] = this match {
    case Blocked(br, c) => blocked(br, c.map(f))
    case Done(a)        => done(f(a))
  }

  /**
   * Maps the specified function over the failed value of this result.
   */
  final def mapError[E1](f: E => E1): Result[R, E1, A] = this match {
    case Blocked(br, c) => blocked(br.mapDataSources(DataSourceFunction.mapError(f)), c.mapError(f))
    case Done(a)        => done(a)
  }

  /**
   * Provides this result with its required environment.
   */
  final def provide(r: R): Result[Any, E, A] =
    provideSome(_ => r)

  /**
   * Provides this result with part of its required environment.
   */
  final def provideSome[R0](f: R0 => R): Result[R0, E, A] = this match {
    case Blocked(br, c) => blocked(br.mapDataSources(DataSourceFunction.provideSome(f)), c.provideSome(f))
    case Done(a)        => done(a)
  }
}

private[zquery] object Result {

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
