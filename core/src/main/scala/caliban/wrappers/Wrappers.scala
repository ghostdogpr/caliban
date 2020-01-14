package caliban.wrappers

import caliban.CalibanError.{ ExecutionError, ValidationError }
import caliban.GraphQLResponse
import caliban.Value.NullValue
import caliban.execution.Field
import caliban.wrappers.Wrapper.{ OverallWrapper, ValidationWrapper }
import zio.clock.Clock
import zio.console.{ putStrLn, Console }
import zio.duration.Duration
import zio.{ IO, URIO, ZIO }

object Wrappers {

  /**
   * Returns a wrapper that prints slow queries
   * @param duration threshold above which queries are considered slow
   */
  def printSlowQueries(duration: Duration): OverallWrapper[Console with Clock] =
    onSlowQueries(duration) { case (time, query) => putStrLn(s"Slow query took ${time.render}:\n$query") }

  /**
   * Returns a wrapper that runs a given function in case of slow queries
   * @param duration threshold above which queries are considered slow
   */
  def onSlowQueries[R](duration: Duration)(f: (Duration, String) => URIO[R, Any]): OverallWrapper[R with Clock] =
    OverallWrapper {
      case (io, query) =>
        io.timed.flatMap {
          case (time, res) =>
            ZIO.when(time > duration)(f(time, query)).as(res)
        }
    }

  /**
   * Returns a wrapper that times out queries taking more than a specified time.
   * @param duration threshold above which queries should be timed out
   */
  def timeout(duration: Duration): OverallWrapper[Clock] =
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

  /**
   * Returns a wrapper that checks that the query's depth is under a given max
   * @param maxDepth the max allowed depth
   */
  def maxDepth(maxDepth: Int): ValidationWrapper[Any] =
    ValidationWrapper {
      case (io, _) =>
        io.flatMap { req =>
          val depth = calculateDepth(req.field)
          if (depth > maxDepth) IO.fail(ValidationError(s"Query is too deep: $depth. Max depth: $maxDepth.", ""))
          else IO.succeed(req)
        }
    }

  private def calculateDepth(field: Field): Int = {
    val children      = field.fields ++ field.conditionalFields.values.flatten
    val childrenDepth = if (children.isEmpty) 0 else children.map(calculateDepth).max
    childrenDepth + (if (field.name.nonEmpty) 1 else 0)
  }

  /**
   * Returns a wrapper that checks that the query has a limited number of fields
   * @param maxFields the max allowed number of fields
   */
  def maxFields(maxFields: Int): ValidationWrapper[Any] =
    ValidationWrapper {
      case (io, _) =>
        io.flatMap { req =>
          val fields = countFields(req.field)
          if (fields > maxFields)
            IO.fail(ValidationError(s"Query has too many fields: $fields. Max fields: $maxFields.", ""))
          else IO.succeed(req)
        }
    }

  private def countFields(field: Field): Int =
    innerFields(field.fields) + (if (field.conditionalFields.isEmpty) 0
                                 else field.conditionalFields.values.map(innerFields).max)

  private def innerFields(fields: List[Field]): Int = fields.length + fields.map(countFields).sum

}
