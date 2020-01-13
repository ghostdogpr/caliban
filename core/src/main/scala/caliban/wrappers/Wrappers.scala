package caliban.wrappers

import caliban.CalibanError.ExecutionError
import caliban.Value.NullValue
import caliban.wrappers.Wrapper.OverallWrapper
import caliban.{ GraphQL, GraphQLResponse }
import zio.ZIO
import zio.clock.Clock
import zio.console.{ putStrLn, Console }
import zio.duration.Duration

object Wrappers {

  /**
   * Attaches to the given GraphQL API definition a wrapper that logs slow queries.
   * @param duration threshold above which queries are considered slow
   * @param api a GraphQL API definition
   */
  def logSlowQueries[R <: Console with Clock](duration: Duration)(api: GraphQL[R]): GraphQL[R] =
    api.withWrapper(logSlowQueriesWrapper(duration))

  /**
   * Returns a wrapper that logs slow queries
   * @param duration threshold above which queries are considered slow
   */
  def logSlowQueriesWrapper(duration: Duration): OverallWrapper[Console with Clock] =
    OverallWrapper {
      case (io, query) =>
        io.timed.flatMap {
          case (time, res) =>
            ZIO.when(time > duration)(putStrLn(s"Slow query took ${time.render}:\n$query")).as(res)
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
