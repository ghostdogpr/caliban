package zquery

import zio.{ NeedsEnv, ZIO }

/**
 * A `DataSource[R, A]` is capable of executing requests of type `A` that
 * require an environment `R`.
 *
 * Data sources must implement the method `run` which takes a collection of
 * requests and returns an effect with a `CompletedRequestMap` containing a
 * mapping from requests to results. Because `run` is parameterized on a
 * collection of requests rather than a single request, data sources have the
 * ability to introspect on all the requests being executed in parallel and
 * optimize the query.
 *
 * Data sources will typically be parameterized on a subtype of `Request[A]`,
 * though that is not strictly necessarily as long as the data source can map
 * the request type to a `Request[A]`. Data sources can then pattern match on
 * the collection of requests to determine the information requested, execute
 * the query, and place the results into the `CompletedRequestsMap` using
 * [[CompletedRequestMap.empty]] and [[CompletedRequestMap.insert]]. Data
 * sources must provide requests for all results received or fail with an `E`.
 * Failure to do so will cause a query to die with a `QueryFailure` when run.
 */
trait DataSource[-R, -A] { self =>

  /**
   * The data source's identifier.
   */
  val identifier: String

  /**
   * Execute a collection of requests. Data sources are guaranteed that the
   * collection will contain at least one request and that all requests will be
   * unique when they are called by `ZQuery`.
   */
  def run(requests: Iterable[A]): ZIO[R, Nothing, CompletedRequestMap]

  /**
   * Returns a new data source that executes requests of type `B` using the
   * specified function to transform `B` requests into requests that this data
   * source can execute.
   */
  final def contramap[B](f: Described[B => A]): DataSource[R, B] =
    new DataSource[R, B] {
      val identifier = s"${self.identifier}.contramap(${f.description})"
      def run(requests: Iterable[B]): ZIO[R, Nothing, CompletedRequestMap] =
        self.run(requests.map(f.value))
    }

  /**
   * Returns a new data source that executes requests of type `B` using the
   * specified effectual function to transform `B` requests into requests that
   * this data source can execute.
   */
  final def contramapM[R1 <: R, B](f: Described[B => ZIO[R1, Nothing, A]]): DataSource[R1, B] =
    new DataSource[R1, B] {
      val identifier = s"${self.identifier}.contramapM(${f.description})"
      def run(requests: Iterable[B]): ZIO[R1, Nothing, CompletedRequestMap] =
        ZIO.foreach(requests)(f.value).flatMap(self.run)
    }

  /**
   * Returns a new data source that executes requests of type `C` using the
   * specified function to transform `C` requests into requests that either
   * this data source or that data source can execute.
   */
  final def eitherWith[R1 <: R, B, C](
    that: DataSource[R1, B]
  )(f: Described[C => Either[A, B]]): DataSource[R1, C] =
    new DataSource[R1, C] {
      val identifier = s"${self.identifier}.eitherWith(${that.identifier})(${f.description})"
      def run(requests: Iterable[C]): ZIO[R1, Nothing, CompletedRequestMap] = {
        val (as, bs) = requests.foldLeft((List.empty[A], List.empty[B])) {
          case ((as, bs), c) =>
            f.value(c) match {
              case Left(a)  => (a :: as, bs)
              case Right(b) => (as, b :: bs)
            }
        }
        self.run(as).zipWithPar(that.run(bs))(_ ++ _)
      }
    }

  override final def equals(that: Any): Boolean = that match {
    case that: DataSource[_, _] => this.identifier == that.identifier
  }

  override final def hashCode: Int =
    identifier.hashCode

  /**
   * Provides this data source with its required environment.
   */
  final def provide(r: Described[R])(implicit ev: NeedsEnv[R]): DataSource[Any, A] =
    provideSome(Described(_ => r.value, s"_ => ${r.description}"))

  /**
   * Provides this data source with part of its required environment.
   */
  final def provideSome[R0](f: Described[R0 => R])(implicit ev: NeedsEnv[R]): DataSource[R0, A] =
    new DataSource[R0, A] {
      val identifier = s"${self.identifier}.provideSome(${f.description})"
      def run(requests: Iterable[A]): ZIO[R0, Nothing, CompletedRequestMap] =
        self.run(requests).provideSome(f.value)
    }

  override final def toString: String =
    identifier
}

object DataSource {

  /**
   * Constructs a data source from a function taking a collection of requests
   * and returning a `CompletedRequestMap`.
   */
  def apply[R, A](name: String)(f: Iterable[A] => ZIO[R, Nothing, CompletedRequestMap]): DataSource[R, A] =
    new DataSource[R, A] {
      val identifier = name
      def run(requests: Iterable[A]): ZIO[R, Nothing, CompletedRequestMap] =
        f(requests)
    }

  /**
   * Constructs a data source from a pure function.
   */
  def fromFunction[A, B](
    name: String
  )(f: A => B)(implicit ev: A <:< Request[Nothing, B]): DataSource[Any, A] =
    new DataSource[Any, A] {
      val identifier = name
      def run(requests: Iterable[A]): ZIO[Any, Nothing, CompletedRequestMap] =
        ZIO.succeed(requests.foldLeft(CompletedRequestMap.empty)((map, k) => map.insert(k)(Right(f(k)))))
    }

  /**
   * Constructs a data source from a pure function that takes a list of
   * requests and returns a list of results of the same size. Each item in the
   * result list must correspond to the item at the same index in the request
   * list.
   */
  def fromFunctionBatched[A, B](
    name: String
  )(f: Iterable[A] => Iterable[B])(implicit ev: A <:< Request[Nothing, B]): DataSource[Any, A] =
    fromFunctionBatchedM(name)(f andThen ZIO.succeed)

  /**
   * Constructs a data source from an effectual function that takes a list of
   * requests and returns a list of results of the same size. Each item in the
   * result list must correspond to the item at the same index in the request
   * list.
   */
  def fromFunctionBatchedM[R, E, A, B](
    name: String
  )(f: Iterable[A] => ZIO[R, E, Iterable[B]])(implicit ev: A <:< Request[E, B]): DataSource[R, A] =
    new DataSource[R, A] {
      val identifier = name
      def run(requests: Iterable[A]): ZIO[R, Nothing, CompletedRequestMap] =
        f(requests)
          .fold(
            failure => requests.map((_, Left(failure))),
            results => requests.zip(results.map(Right(_)))
          )
          .map(_.foldLeft(CompletedRequestMap.empty) {
            case (map, (k, v)) => map.insert(k)(v)
          })
    }

  /**
   * Constructs a data source from a function that takes a list of requests and
   * returns a list of results of the same size. Uses the specified function to
   * associate each result with the corresponding effect, allowing the function
   * to return the list of results in a different order than the list of
   * requests.
   */
  def fromFunctionBatchedWith[A, B](
    name: String
  )(f: Iterable[A] => Iterable[B], g: B => Request[Nothing, B]): DataSource[Any, A] =
    fromFunctionBatchedWithM(name)(f andThen ZIO.succeed, g)

  /**
   * Constructs a data source from an effectual function that takes a list of
   * requests and returns a list of results of the same size. Uses the
   * specified function to associate each result with the corresponding effect,
   * allowing the function to return the list of results in a different order
   * than the list of requests.
   */
  def fromFunctionBatchedWithM[R, E, A, B](
    name: String
  )(f: Iterable[A] => ZIO[R, Nothing, Iterable[B]], g: B => Request[E, B]): DataSource[R, A] =
    new DataSource[R, A] {
      val identifier = name
      def run(requests: Iterable[A]): ZIO[R, Nothing, CompletedRequestMap] =
        f(requests).map { results =>
          results.map(b => (g(b), b)).foldLeft(CompletedRequestMap.empty) {
            case (map, (k, v)) => map.insert(k)(Right(v))
          }
        }
    }

  /**
   * Constructs a data source from an effectual function.
   */
  def fromFunctionM[R, E, A, B](
    name: String
  )(f: A => ZIO[R, E, B])(implicit ev: A <:< Request[E, B]): DataSource[R, A] =
    new DataSource[R, A] {
      val identifier = name
      def run(requests: Iterable[A]): ZIO[R, Nothing, CompletedRequestMap] =
        ZIO
          .foreachPar(requests)(k => ZIO.succeed(k).zip(f(k).either))
          .map(_.foldLeft(CompletedRequestMap.empty) { case (map, (k, v)) => map.insert(k)(v) })
    }
}
