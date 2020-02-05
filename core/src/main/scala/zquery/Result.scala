package zquery

import zio.{ CanFail, Cause, NeedsEnv }
import zquery.Result._

/**
 * A `Result[R, E, A]` is the result of running one step of a `ZQuery`. A
 * result may either by done with a value `A`, blocked on a set of requests
 * to data sources that require an environment `R`, or failed with an `E`.
 */
private[zquery] sealed trait Result[-R, +E, +A] {

  /**
   * Folds over the successful or failed result.
   */
  final def fold[B](failure: E => B, success: A => B)(implicit ev: CanFail[E]): Result[R, Nothing, B] = this match {
    case Blocked(br, c) => blocked(br, c.fold(failure, success))
    case Done(a)        => done(success(a))
    case Fail(e)        => e.failureOrCause.fold(e => done(failure(e)), c => fail(c))
  }

  /**
   * Maps the specified function over the successful value of this result.
   */
  final def map[B](f: A => B): Result[R, E, B] = this match {
    case Blocked(br, c) => blocked(br, c.map(f))
    case Done(a)        => done(f(a))
    case Fail(e)        => fail(e)
  }

  /**
   * Maps the specified function over the failed value of this result.
   */
  final def mapError[E1](f: E => E1)(implicit ev: CanFail[E]): Result[R, E1, A] = this match {
    case Blocked(br, c) => blocked(br, c.mapError(f))
    case Done(a)        => done(a)
    case Fail(e)        => fail(e.map(f))
  }

  /**
   * Provides this result with its required environment.
   */
  final def provide(r: Described[R])(implicit ev: NeedsEnv[R]): Result[Any, E, A] =
    provideSome(Described(_ => r.value, s"_ => ${r.description}"))

  /**
   * Provides this result with part of its required environment.
   */
  final def provideSome[R0](f: Described[R0 => R])(implicit ev: NeedsEnv[R]): Result[R0, E, A] = this match {
    case Blocked(br, c) => blocked(br.mapDataSources(DataSourceFunction.provideSome(f)), c.provideSome(f))
    case Done(a)        => done(a)
    case Fail(e)        => fail(e)
  }
}

private[zquery] object Result {

  /**
   * Constructs a result that is blocked on the specified requests with the
   * specified continuation.
   */
  def blocked[R, E, A](blockedRequests: BlockedRequestMap[R], continue: ZQuery[R, E, A]): Result[R, E, A] =
    Blocked(blockedRequests, continue)

  /**
   * Constructs a result that is done with the specified value.
   */
  def done[A](value: A): Result[Any, Nothing, A] =
    Done(value)

  /**
   * Constructs a result that is failed with the specified `Cause`.
   */
  def fail[E](cause: Cause[E]): Result[Any, E, Nothing] =
    Fail(cause)

  /**
   * Lifts an `Either` into a result.
   */
  def fromEither[E, A](either: Either[E, A]): Result[Any, E, A] =
    either.fold(e => Result.fail(Cause.fail(e)), a => Result.done(a))

  /**
   * Lifts an `Option[Either[E, A]]` into a result.
   */
  def fromOptionEither[E, A](oeea: Option[Either[E, A]]): Result[Any, E, Option[A]] =
    oeea match {
      case None           => Result.done(None)
      case Some(Left(e))  => Result.fail(Cause.fail(e))
      case Some(Right(a)) => Result.done(Some(a))
    }

  final case class Blocked[-R, +E, +A](blockedRequests: BlockedRequestMap[R], continue: ZQuery[R, E, A])
      extends Result[R, E, A]

  final case class Done[+A](value: A) extends Result[Any, Nothing, A]

  final case class Fail[+E](cause: Cause[E]) extends Result[Any, E, Nothing]
}
