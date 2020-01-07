package zquery

import zio.{ CanFail, Cause, NeedsEnv, Ref, ZIO }

/**
 * A `ZQuery[R, E, A]` is a purely functional description of an effectual query
 * that may contain requests from one or more data sources, requires an
 * environment `R`, may fail with an `E`, and may succeed with an `A`. All
 * requests that do not need to be performed sequentially, as expressed by
 * `flatMap` or combinators derived from it, will automatically be batched,
 * allowing for aggressive data source specific optimizations. Requests will
 * also automatically be deduplicated and cached.
 *
 * This allows for writing queries in a high level, compositional style, with
 * confidence that they will automatically be optimized. For example, consider
 * the following query from a user service.
 *
 * {{{
 * val getAllUserIds: ZQuery[Any, Nothing, List[Int]] = ???
 * def getUserNameById(id: Int): ZQuery[Any, Nothing, String] = ???
 *
 * for {
 *   userIds   <- getAllUserIds
 *   userNames <- ZQuery.foreachPar(userIds)(getUserNameById)
 * } yield userNames
 * }}}
 *
 * This would normally require N + 1 queries, one for `getAllUserIds` and one
 * for each call to `getUserNameById`. In contrast, `ZQuery` will automatically
 * optimize this to two queries, one for `userIds` and one for `userNames`,
 * assuming an implementation of the user service that supports batching.
 *
 * Based on "There is no Fork: an Abstraction for Efficient, Concurrent, and
 * Concise Data Access" by Simon Marlow, Louis Brandy, Jonathan Coens, and Jon
 * Purdy. [[http://simonmar.github.io/bib/papers/haxl-icfp14.pdf]]
 */
sealed trait ZQuery[-R, +E, +A] { self =>

  /**
   * Executes one step of this query.
   */
  protected def step(cache: Cache): ZIO[R, Nothing, Result[R, E, A]]

  /**
   * A symbolic alias for `zipParRight`.
   */
  final def &>[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, B] =
    zipParRight(that)

  /**
   * A symbolic alias for `zipRight`.
   */
  final def *>[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, B] =
    zipRight(that)

  /**
   * A symbolic alias for `zipParLeft`.
   */
  final def <&[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, A] =
    zipParLeft(that)

  /**
   * A symbolic alias for `zipPar`.
   */
  final def <&>[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, (A, B)] =
    zipPar(that)

  /**
   * A symbolic alias for `zipLeft`.
   */
  final def <*[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, A] =
    zipLeft(that)

  /**
   * A symbolic alias for `zip`.
   */
  final def <*>[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, (A, B)] =
    zip(that)

  /**
   * A symbolic alias for `flatMap`.
   */
  final def >>=[R1 <: R, E1 >: E, B](f: A => ZQuery[R1, E1, B]): ZQuery[R1, E1, B] =
    flatMap(f)

  /**
   * Returns a query whose failure and success have been lifted into an
   * `Either`. The resulting query cannot fail, because the failure case has
   * been exposed as part of the `Either` success case.
   */
  final def either(implicit ev: CanFail[E]): ZQuery[R, Nothing, Either[E, A]] =
    fold(Left(_), Right(_))

  /**
   * Returns a query that models execution of this query, followed by passing
   * its result to the specified function that returns a query. Requests
   * composed with `flatMap` or combinators derived from it will be executed
   * sequentially and will not be batched, though deduplication and caching of
   * requests will still be applied.
   */
  final def flatMap[R1 <: R, E1 >: E, B](f: A => ZQuery[R1, E1, B]): ZQuery[R1, E1, B] =
    new ZQuery[R1, E1, B] {
      def step(cache: Cache): ZIO[R1, Nothing, Result[R1, E1, B]] =
        self.step(cache).flatMap {
          case Result.Blocked(br, c) => ZIO.succeed(Result.blocked(br, c.flatMap(f)))
          case Result.Done(a)        => f(a).step(cache)
          case Result.Fail(e)        => ZIO.succeed(Result.fail(e))
        }
    }

  /**
   * Folds over the failed or successful result of this query to yield a query
   * that does not fail, but succeeds with the value returned by the left or
   * right function passed to `fold`.
   */
  final def fold[B](failure: E => B, success: A => B)(implicit ev: CanFail[E]): ZQuery[R, Nothing, B] =
    new ZQuery[R, Nothing, B] {
      def step(cache: Cache): ZIO[R, Nothing, Result[R, Nothing, B]] =
        self.step(cache).map(_.fold(failure, success))
    }

  /**
   * Maps the specified function over the successful result of this query.
   */
  final def map[B](f: A => B): ZQuery[R, E, B] =
    new ZQuery[R, E, B] {
      def step(cache: Cache): ZIO[R, Nothing, Result[R, E, B]] =
        self.step(cache).map(_.map(f))
    }

  /**
   * Maps the specified function over the failed result of this query.
   */
  final def mapError[E1](f: E => E1)(implicit ev: CanFail[E]): ZQuery[R, E1, A] =
    new ZQuery[R, E1, A] {
      def step(cache: Cache): ZIO[R, Nothing, Result[R, E1, A]] =
        self.step(cache).map(_.mapError(f))
    }

  /**
   * Provides this query with its required environment.
   */
  final def provide(r: Described[R])(implicit ev: NeedsEnv[R]): ZQuery[Any, E, A] =
    provideSome(Described(_ => r.value, s"_ => ${r.description}"))

  /**
   * Provides this query with part of its required environment.
   */
  final def provideSome[R0](f: Described[R0 => R])(implicit ev: NeedsEnv[R]): ZQuery[R0, E, A] =
    new ZQuery[R0, E, A] {
      def step(cache: Cache): ZIO[R0, Nothing, Result[R0, E, A]] =
        self.step(cache).provideSome(f.value).map(_.provideSome(f))
    }

  /**
   * Returns an effect that models executing this query.
   */
  final val run: ZIO[R, E, A] =
    runLog.map(_._2)

  /**
   * Returns an effect that models executing this query with the specified
   * cache. This can be useful for deterministically "replaying" a query
   * without executing any new requests.
   */
  final def runCache(cache: Cache): ZIO[R, E, A] =
    step(cache).flatMap {
      case Result.Blocked(br, c) => br.run *> c.runCache(cache)
      case Result.Done(a)        => ZIO.succeed(a)
      case Result.Fail(e)        => ZIO.halt(e)
    }

  /**
   * Returns an effect that models executing this query, returning the query
   * result along with the cache containing a complete log of all requests
   * executed and their results. This can be useful for logging or analysis of
   * query execution.
   */
  final def runLog: ZIO[R, E, (Cache, A)] =
    for {
      cache <- Cache.empty
      a     <- runCache(cache)
    } yield (cache, a)

  /**
   * Returns a query that models the execution of this query and the specified
   * query sequentially, combining their results into a tuple.
   */
  final def zip[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, (A, B)] =
    zipWith(that)((_, _))

  /**
   * Returns a query that models the execution of this query and the specified
   * query sequentially, returning the result of this query.
   */
  final def zipLeft[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, A] =
    zipWith(that)((a, _) => a)

  /**
   * Returns a query that models the execution of this query and the specified
   * query sequentially, returning the result of the specified query.
   */
  final def zipRight[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, B] =
    zipWith(that)((_, b) => b)

  /**
   * Returns a query that models the execution of this query and the specified
   * query in parallel, combining their results into a tuple.
   */
  final def zipPar[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, (A, B)] =
    zipWithPar(that)((_, _))

  /**
   * Returns a query that models the execution of this query and the specified
   * query in parallel, returning the result of this query.
   */
  final def zipParLeft[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, A] =
    zipWithPar(that)((a, _) => a)

  /**
   * Returns a query that models the execution of this query and the specified
   * query in parallel, returning the result of the specified query.
   */
  final def zipParRight[R1 <: R, E1 >: E, B](that: ZQuery[R1, E1, B]): ZQuery[R1, E1, B] =
    zipWithPar(that)((_, b) => b)

  /**
   * Returns a query that models the execution of this query and the specified
   * query sequentially, combining their results with the specified function.
   */
  final def zipWith[R1 <: R, E1 >: E, B, C](that: ZQuery[R1, E1, B])(f: (A, B) => C): ZQuery[R1, E1, C] =
    flatMap(a => that.map(b => f(a, b)))

  /**
   * Returns a query that models the execution of this query and the specified
   * query in parallel, combining their results with the specified function.
   * Requests composed with `zipWithPar` or combinators derived from it will
   * be batched and deduplication and caching of requests will be applied.
   */
  final def zipWithPar[R1 <: R, E1 >: E, B, C](that: ZQuery[R1, E1, B])(f: (A, B) => C): ZQuery[R1, E1, C] =
    new ZQuery[R1, E1, C] {
      def step(cache: Cache): ZIO[R1, Nothing, Result[R1, E1, C]] =
        self.step(cache).zip(that.step(cache)).map {
          case (Result.Blocked(br1, c1), Result.Blocked(br2, c2)) => Result.blocked(br1 ++ br2, c1.zipWithPar(c2)(f))
          case (Result.Blocked(br, c), Result.Done(_))            => Result.blocked(br, c.zipWithPar(that)(f))
          case (Result.Done(_), Result.Blocked(br, c))            => Result.blocked(br, self.zipWithPar(c)(f))
          case (Result.Done(a), Result.Done(b))                   => Result.done(f(a, b))
          case (Result.Fail(e1), Result.Fail(e2))                 => Result.fail(Cause.Both(e1, e2))
          case (Result.Fail(e), _)                                => Result.fail(e)
          case (_, Result.Fail(e))                                => Result.fail(e)
        }
    }
}

object ZQuery {

  /**
   * Collects a collection of queries into a query returning a collection of
   * their results. Requests will be executed sequentially and will not be
   * batched.
   */
  def collectAll[R, E, A](as: Iterable[ZQuery[R, E, A]]): ZQuery[R, E, List[A]] =
    foreach(as)(identity)

  /**
   * Collects a collection of queries into a query returning a collection of
   * their results. All requests will be batched.
   */
  def collectAllPar[R, E, A](as: Iterable[ZQuery[R, E, A]]): ZQuery[R, E, List[A]] =
    foreachPar(as)(identity)

  /**
   * Constructs a query that fails with the specified error.
   */
  def fail[E](error: E): ZQuery[Any, E, Nothing] =
    ZQuery(ZIO.succeed(Result.fail(Cause.fail(error))))

  /**
   * Performs a query for each element in a collection, collecting the results
   * into a query returning a collection of their results. Requests will be
   * executed sequentially and will not be batched.
   */
  def foreach[R, E, A, B](as: Iterable[A])(f: A => ZQuery[R, E, B]): ZQuery[R, E, List[B]] =
    as.foldRight[ZQuery[R, E, List[B]]](ZQuery.succeed(Nil))((a, bs) => f(a).zipWith(bs)(_ :: _))

  /**
   * Performs a query for each element in a collection, collecting the results
   * into a query returning a collection of their results. All requests will be
   * batched.
   */
  def foreachPar[R, E, A, B](as: Iterable[A])(f: A => ZQuery[R, E, B]): ZQuery[R, E, List[B]] =
    as.foldRight[ZQuery[R, E, List[B]]](ZQuery.succeed(Nil))((a, bs) => f(a).zipWithPar(bs)(_ :: _))

  /**
   * Constructs a query from an effect.
   */
  def fromEffect[R, E, A](effect: ZIO[R, E, A]): ZQuery[R, E, A] =
    ZQuery(effect.foldCause(Result.fail, Result.done))

  /**
   * Constructs a query from a request, requiring an environment containing a
   * data source able to execute the request. This is useful to express the
   * dependency on a data source in a more idiomatic style and to defer
   * committing to a particular implementation of the data source too early,
   * allowing, for example, for live and test implementations.
   */
  def fromRequest[R, E, A, B](
    request: A
  )(implicit ev: A <:< Request[E, B]): ZQuery[R with DataSource[R, A], E, B] =
    ZQuery.fromEffect(ZIO.environment[DataSource[R, A]]).flatMap(r => fromRequestWith(request)(r.dataSource))

  /**
   * Constructs a query from a request and a data source. Queries must be
   * constructed with `fromRequestWith` or combinators derived from it for
   * optimizations to be applied.
   */
  def fromRequestWith[R, E, A, B](
    request: A
  )(dataSource: DataSource.Service[R, A])(implicit ev: A <:< Request[E, B]): ZQuery[R, E, B] =
    new ZQuery[R, E, B] {
      def step(cache: Cache): ZIO[R, Nothing, Result[R, E, B]] =
        cache
          .lookup(request)
          .foldM(
            _ =>
              for {
                ref <- Ref.make(Option.empty[Either[E, B]])
                _   <- cache.insert(request, ref)
              } yield Result.blocked(
                BlockedRequestMap(dataSource, BlockedRequest(request, ref)),
                ZQuery {
                  ref.get.flatMap {
                    case None    => ZIO.die(QueryFailure(dataSource, request))
                    case Some(b) => ZIO.succeed(Result.fromEither(b))
                  }
                }
              ),
            ref =>
              ref.get.map {
                case Some(b) => Result.fromEither(b)
                case None =>
                  Result.blocked(
                    BlockedRequestMap.empty,
                    ZQuery {
                      ref.get.flatMap {
                        case None    => ZIO.die(QueryFailure(dataSource, request))
                        case Some(b) => ZIO.succeed(Result.fromEither(b))
                      }
                    }
                  )
              }
          )
    }

  /**
   * Performs a query for each element in a collection, collecting the results
   * into a collection of failed results and a collection of successful
   * results. Requests will be executed sequentially and will not be batched.
   */
  def partitionM[R, E, A, B](
    as: Iterable[A]
  )(f: A => ZQuery[R, E, B])(implicit ev: CanFail[E]): ZQuery[R, Nothing, (List[E], List[B])] =
    ZQuery.foreach(as)(f(_).either).map(partitionMap(_)(identity))

  /**
   * Performs a query for each element in a collection, collecting the results
   * into a collection of failed results and a collection of successful
   * results. All requests will be batched.
   */
  def partitionMPar[R, E, A, B](
    as: Iterable[A]
  )(f: A => ZQuery[R, E, B])(implicit ev: CanFail[E]): ZQuery[R, Nothing, (List[E], List[B])] =
    ZQuery.foreachPar(as)(f(_).either).map(partitionMap(_)(identity))

  /**
   *  Constructs a query that succeeds with the specified value.
   */
  def succeed[A](value: A): ZQuery[Any, Nothing, A] =
    ZQuery(ZIO.succeed(Result.done(value)))

  /**
   * Constructs a query from an effect that returns a result.
   */
  private def apply[R, E, A](step0: ZIO[R, Nothing, Result[R, E, A]]): ZQuery[R, E, A] =
    new ZQuery[R, E, A] {
      def step(cache: Cache): ZIO[R, Nothing, Result[R, E, A]] =
        step0
    }

  /**
   * Partitions the elements of a collection using the specified function.
   */
  private def partitionMap[E, A, B](
    as: Iterable[A]
  )(f: A => Either[E, B])(implicit ev: CanFail[E]): (List[E], List[B]) =
    as.foldRight((List.empty[E], List.empty[B])) {
      case (a, (es, bs)) =>
        f(a).fold(
          e => (e :: es, bs),
          b => (es, b :: bs)
        )
    }
}
