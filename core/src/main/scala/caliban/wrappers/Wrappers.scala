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

  def logSlowQueries[R <: Console with Clock](maxDuration: Duration)(api: GraphQL[R]): GraphQL[R] =
    api.withWrapper(logSlowQueriesWrapper(maxDuration))

  def logSlowQueriesWrapper(maxDuration: Duration): OverallWrapper[Console with Clock] =
    OverallWrapper {
      case (io, query) =>
        io.timed.flatMap {
          case (time, res) =>
            ZIO.when(time > maxDuration)(putStrLn(s"Slow query took ${time.render}:\n$query")).as(res)
        }
    }

  def timeout[R <: Clock](maxDuration: Duration)(api: GraphQL[R]): GraphQL[R] =
    api.withWrapper(timeoutWrapper(maxDuration))

  def timeoutWrapper(maxDuration: Duration): OverallWrapper[Clock] =
    OverallWrapper {
      case (io, query) =>
        io.timeout(maxDuration)
          .map(
            _.getOrElse(
              GraphQLResponse(
                NullValue,
                List(ExecutionError(s"Query was interrupted after timeout of ${maxDuration.render}:\n$query"))
              )
            )
          )
    }
}
