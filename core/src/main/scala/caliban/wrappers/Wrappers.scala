package caliban.wrappers

import caliban.CalibanError.ExecutionError
import caliban.Value.NullValue
import caliban.wrappers.Wrapper.OverallWrapper
import caliban.{ GraphQL, GraphQLResponse }
import zio.clock.Clock
import zio.console.{ putStrLn, Console }
import zio.duration.Duration
import zio.{ URIO, ZIO }

object Wrappers {

  /**
   * Attaches to the given GraphQL API definition a wrapper that prints slow queries.
   * @param duration threshold above which queries are considered slow
   * @param api a GraphQL API definition
   */
  def printSlowQueries[R <: Console with Clock](duration: Duration)(api: GraphQL[R]): GraphQL[R] =
    api.withWrapper(printSlowQueriesWrapper(duration))

  /**
   * Returns a wrapper that prints slow queries
   * @param duration threshold above which queries are considered slow
   */
  def printSlowQueriesWrapper(duration: Duration): OverallWrapper[Console with Clock] =
    onSlowQueriesWrapper(duration) { case (time, query) => putStrLn(s"Slow query took ${time.render}:\n$query") }

  /**
   * Attaches to the given GraphQL API definition a wrapper that runs a given function in case of slow queries.
   * @param duration threshold above which queries are considered slow
   * @param api a GraphQL API definition
   */
  def onSlowQueries[R <: Clock](
    duration: Duration
  )(f: (Duration, String) => URIO[R, Any])(api: GraphQL[R]): GraphQL[R] =
    api.withWrapper(onSlowQueriesWrapper(duration)(f))

  /**
   * Returns a wrapper that runs a given function in case of slow queries
   * @param duration threshold above which queries are considered slow
   */
  def onSlowQueriesWrapper[R](duration: Duration)(f: (Duration, String) => URIO[R, Any]): OverallWrapper[R with Clock] =
    OverallWrapper {
      case (io, query) =>
        io.timed.flatMap {
          case (time, res) =>
            ZIO.when(time > duration)(f(time, query)).as(res)
        }
    }

  /**
   * Attaches to the given GraphQL API definition a wrapper that times out queries taking more than a specified time.
   * @param duration threshold above which queries should be timed out
   * @param api a GraphQL API definition
   */
  def timeout[R <: Clock](duration: Duration)(api: GraphQL[R]): GraphQL[R] =
    api.withWrapper(timeoutWrapper(duration))

  /**
   * Returns a wrapper that times out queries taking more than a specified time.
   * @param duration threshold above which queries should be timed out
   */
  def timeoutWrapper(duration: Duration): OverallWrapper[Clock] =
    OverallWrapper {
      case (io, query) =>
        io.timeout(duration)
          .map(
            _.getOrElse(
              GraphQLResponse(
                NullValue,
                List(ExecutionError(s"Query was interrupted after timeout of ${duration.render}:\n$query"))
              )
            )
          )
    }
}
