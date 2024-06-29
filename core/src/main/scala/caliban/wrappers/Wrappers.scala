package caliban.wrappers

import caliban.CalibanError.{ ExecutionError, ValidationError }
import caliban.Value.NullValue
import caliban.execution.{ ExecutionRequest, Field, FieldInfo }
import caliban.parsing.adt.{ Directive, Document }
import caliban.wrappers.Wrapper.{ FieldWrapper, OverallWrapper, ValidationWrapper }
import caliban.{ CalibanError, Configurator, GraphQLRequest, GraphQLResponse, ResponseValue }
import zio.Console.{ printLine, printLineError }
import zio._
import zio.metrics.MetricKeyType.Histogram
import zio.metrics.MetricLabel
import zio.query.ZQuery
import java.time.{ Clock => JClock }

import scala.annotation.tailrec

object Wrappers {

  /**
   * Returns a wrapper that prints errors to the console
   */
  lazy val printErrors: OverallWrapper[Any] =
    new OverallWrapper[Any] {
      def wrap[R1](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        request =>
          process(request).tap { response =>
            val errors = response.errors
            if (errors.nonEmpty) printLineError(errors.flatMap(prettyStackStrace).mkString("", "\n", "\n")).orDie
            else Exit.unit
          }
    }

  private def prettyStackStrace(t: Throwable): Chunk[String] = {
    @tailrec def go(acc: Chunk[String], t: Throwable): Chunk[String] =
      if (t == null) acc
      else go(acc ++ (t.toString +: Chunk.fromArray(t.getStackTrace).map("\tat " + _.toString)), t.getCause)
    go(Chunk(""), t)
  }

  /**
   * Returns a wrapper that prints slow queries
   * @param duration threshold above which queries are considered slow
   */
  def printSlowQueries(duration: Duration): OverallWrapper[Any] =
    onSlowQueries(duration) { case (time, query) => printLine(s"Slow query took ${time.render}:\n$query").orDie }

  /**
   * Returns a wrapper that logs slow queries
   * @param duration threshold above which queries are considered slow
   */
  def logSlowQueries(duration: Duration): OverallWrapper[Any] =
    onSlowQueries(duration) { case (time, query) =>
      ZIO.logAnnotate("query", query) {
        ZIO.logWarning(s"Slow query took ${time.render}")
      }
    }

  /**
   * Returns a wrapper that runs a given function in case of slow queries
   * @param duration threshold above which queries are considered slow
   */
  def onSlowQueries[R](duration: Duration)(f: (Duration, String) => URIO[R, Any]): OverallWrapper[R] =
    new OverallWrapper[R] {
      def wrap[R1 <: R](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          process(request).timed.flatMap { case (time, res) =>
            if (time > duration) f(time, request.query.getOrElse("")).as(res)
            else Exit.succeed(res)
          }
    }

  /**
   * Returns a wrapper that times out queries taking more than a specified time.
   * @param duration threshold above which queries should be timed out
   */
  def timeout(duration: Duration): OverallWrapper[Any] =
    new OverallWrapper[Any] {
      def wrap[R1](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          process(request).timeoutTo(
            GraphQLResponse(
              NullValue,
              List(
                ExecutionError(
                  s"Query was interrupted after timeout of ${duration.render}:\n${request.query.getOrElse("")}"
                )
              )
            )
          )(identity)(duration)

    }

  /**
   * Returns a wrapper that checks that the query's depth is under a given max
   * @param maxDepth the max allowed depth
   */
  def maxDepth(maxDepth: Int): ValidationWrapper[Any] =
    new ValidationWrapper[Any] {
      def wrap[R1 <: Any](
        process: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
          process(doc).tap { req =>
            ZIO.unlessZIODiscard(Configurator.skipValidation) {
              val depth = calculateDepth(req.field)
              if (depth > maxDepth) ZIO.fail(ValidationError(s"Query is too deep: $depth. Max depth: $maxDepth.", ""))
              else Exit.unit
            }
          }
    }

  private def calculateDepth(field: Field): Int = {
    // Faster because it doesn't allocate a new list on each iteration but not stack-safe
    def loopUnsafe(field: Field, currentDepth: Int): Int = {
      var children = field.fields
      var max      = currentDepth
      while (children ne Nil) {
        val d = loopUnsafe(children.head, currentDepth + 1)
        if (d > max) max = d
        children = children.tail
      }
      max
    }

    @tailrec
    def loopSafe(fields: List[Field], currentDepth: Int): Int =
      if (fields.isEmpty) currentDepth
      else loopSafe(fields.flatMap(_.fields), currentDepth + 1)

    try loopUnsafe(field, 0)
    catch {
      case _: StackOverflowError => loopSafe(field.fields, 0)
    }
  }

  /**
   * Returns a wrapper that checks that the query has a limited number of fields
   * @param maxFields the max allowed number of fields
   */
  def maxFields(maxFields: Int): ValidationWrapper[Any] =
    new ValidationWrapper[Any] {
      def wrap[R1 <: Any](
        process: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
          process(doc).tap { req =>
            ZIO.unlessZIODiscard(Configurator.skipValidation) {
              val fields = countFields(req.field)
              if (fields > maxFields)
                ZIO.fail(ValidationError(s"Query has too many fields: $fields. Max fields: $maxFields.", ""))
              else Exit.unit
            }
          }
    }

  /**
   * Returns a wrapper that adds field metrics to the query
   *
   * @param totalLabel the name of the total fields metric
   * @param durationLabel the name of the duration metric
   * @param buckets the buckets to use for the duration metric
   * @param extraLabels extra labels to add to the metrics
   */
  def metrics(
    totalLabel: String = "graphql_fields_total",
    durationLabel: String = "graphql_fields_duration_seconds",
    buckets: Histogram.Boundaries = FieldMetrics.defaultBuckets,
    extraLabels: Set[MetricLabel] = Set.empty
  )(implicit clock: Clock = Clock.ClockLive): Wrapper[Any] =
    FieldMetrics.wrapper(totalLabel, durationLabel, buckets, extraLabels)

  private def countFields(rootField: Field): Int = {
    // Faster because it doesn't allocate a new list on each iteration but not stack-safe
    def loopUnsafe(field: Field): Int = {
      val iter  = field.fields.iterator
      var count = 0
      while (iter.hasNext) {
        val f = iter.next()
        count += loopUnsafe(f) + 1
      }
      count
    }

    @tailrec
    def loopSafe(fields: List[Field], acc: Int): Int =
      if (fields.isEmpty) acc
      else loopSafe(fields.flatMap(_.fields), acc + fields.length)

    try loopUnsafe(rootField)
    catch {
      case _: StackOverflowError => loopSafe(rootField.fields, 0)
    }
  }

  /**
   * Returns a wrapper that check directives on fields and can potentially fail the query
   *
   * @param check a function from directives to a ZIO that can fail
   * @param excludePureFields if true, pure fields will not be checked
   */
  def checkDirectives[R](
    check: List[Directive] => ZIO[R, ExecutionError, Unit],
    excludePureFields: Boolean = true
  ): FieldWrapper[R] =
    new FieldWrapper[R](wrapPureValues = !excludePureFields) {
      def wrap[R1 <: R](
        query: ZQuery[R1, ExecutionError, ResponseValue],
        info: FieldInfo
      ): ZQuery[R1, ExecutionError, ResponseValue] = {
        val directives = info.parent
          .flatMap(_.getFieldOrNull(info.name) match {
            case null => None
            case f    => f.directives
          })
          .getOrElse(Nil)
        ZQuery.fromZIONow(check(directives)) *> query
      }
    }
}
