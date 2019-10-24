package zquery

import zio.ZIO

/**
 * A `DataSource[R, E, A]` is capable of executing requests of type `A` that
 * require an environment `R` and may fail with an `E`.
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
 * Failure to do so will result in a query dieing with a `QueryFailure`.
 *
 * Data sources are guaranteed that all requests will be unique when they are
 * called by `ZQuery`.
 */
trait DataSource[-R, +E, -A] {
  def dataSource: DataSource.Service[R, E, A]
}

object DataSource {

  trait Service[-R, +E, -A] { self =>

    def run(requests: Iterable[A]): ZIO[R, E, CompletedRequestMap]

    /**
     * Returns a new data source that executes requests of type `B` using the
     * specified function to transform `B` requests into requests that this
     * data source can execute.
     */
    final def contramap[B](f: B => A): DataSource.Service[R, E, B] =
      new DataSource.Service[R, E, B] {
        def run(requests: Iterable[B]): ZIO[R, E, CompletedRequestMap] =
          self.run(requests.map(f))
      }

    /**
     * Returns a new data source that executes requests of type `B` using the
     * specified effectual function to transform `B` requests into requests
     * that this data source can execute.
     */
    final def contramapM[R1 <: R, E1 >: E, B](f: B => ZIO[R1, E1, A]): DataSource.Service[R1, E1, B] =
      new DataSource.Service[R1, E1, B] {
        def run(requests: Iterable[B]): ZIO[R1, E1, CompletedRequestMap] =
          ZIO.foreach(requests)(f).flatMap(self.run)
      }

    /**
     * Returns a new data source that executes requests of type `C` using the
     * specified function to transform `C` requests into requests that either
     * this data source or that data source can execute.
     */
    final def combineWith[R1 <: R, E1 >: E, B, C](
      that: DataSource.Service[R1, E1, B]
    )(f: C => Either[A, B]): DataSource.Service[R1, E1, C] =
      new DataSource.Service[R1, E1, C] {
        def run(requests: Iterable[C]): ZIO[R1, E1, CompletedRequestMap] = {
          val (as, bs) = requests.foldLeft((List.empty[A], List.empty[B])) {
            case ((as, bs), c) =>
              f(c) match {
                case Left(a)  => (a :: as, bs)
                case Right(b) => (as, b :: bs)
              }
          }
          self.run(as).zipWithPar(that.run(bs))(_ ++ _)
        }
      }

    /**
     * Returns a new data source with failures mapped using the specified
     * function.
     */
    final def mapError[E1](f: E => E1): DataSource.Service[R, E1, A] =
      new DataSource.Service[R, E1, A] {
        def run(requests: Iterable[A]): ZIO[R, E1, CompletedRequestMap] =
          self.run(requests).mapError(f)
      }

    /**
     * Provides this data source with its required environment.
     */
    final def provide(r: R): DataSource.Service[Any, E, A] =
      provideSome(_ => r)

    /**
     * Provides this data source with part of its required environment.
     */
    final def provideSome[R0](f: R0 => R): DataSource.Service[R0, E, A] =
      new DataSource.Service[R0, E, A] {
        def run(requests: Iterable[A]): ZIO[R0, E, CompletedRequestMap] =
          self.run(requests).provideSome(f)
      }
  }
}
