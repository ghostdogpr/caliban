package caliban.wrappers

import caliban.CalibanError.{ ExecutionError, ValidationError }
import caliban.Value.NullValue
import caliban.execution.{ ExecutionRequest, Field, FieldInfo }
import caliban.parsing.adt.{ Directive, Document }
import caliban.wrappers.Wrapper.{ FieldWrapper, OverallWrapper, ValidationWrapper }
import caliban._
import zio.Console.{ printLine, printLineError }
import zio._
import zio.metrics.MetricKeyType.Histogram
import zio.metrics.MetricLabel
import zio.query.ZQuery
import zio.stacktracer.TracingImplicits.disableAutoTrace

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
          process(request).tap(response =>
            ZIO.when(response.errors.nonEmpty)(
              printLineError(response.errors.flatMap(prettyStackStrace).mkString("", "\n", "\n")).orDie
            )
          )
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
    onSlowQueries(duration) { case (time, query) =>
      implicit val trace: Trace = Trace.empty
      printLine(s"Slow query took ${time.render}:\n$query").orDie
    }

  /**
   * Returns a wrapper that logs slow queries
   * @param duration threshold above which queries are considered slow
   */
  def logSlowQueries(duration: Duration): OverallWrapper[Any] =
    onSlowQueries(duration) { case (time, query) =>
      implicit val trace: Trace = Trace.empty
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
            ZIO.when(time > duration)(f(time, request.query.getOrElse(""))).as(res)
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
          process(request)
            .timeout(duration)
            .map(
              _.getOrElse(
                GraphQLResponse(
                  NullValue,
                  List(
                    ExecutionError(
                      s"Query was interrupted after timeout of ${duration.render}:\n${request.query.getOrElse("")}"
                    )
                  )
                )
              )
            )
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
            ZIO.unlessZIO(Configurator.configuration.map(_.skipValidation)) {
              calculateDepth(req.field).flatMap { depth =>
                ZIO.when(depth > maxDepth)(
                  ZIO.fail(ValidationError(s"Query is too deep: $depth. Max depth: $maxDepth.", ""))
                )
              }
            }
          }
    }

  private def calculateDepth(field: Field)(implicit trace: Trace): UIO[Int] = {
    val self     = if (field.name.nonEmpty) 1 else 0
    val children = field.fields
    ZIO
      .foreach(children)(calculateDepth)
      .map {
        case Nil  => self
        case list => list.max + self
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
            ZIO.unlessZIO(Configurator.configuration.map(_.skipValidation)) {
              countFields(req.field).flatMap { fields =>
                ZIO.when(fields > maxFields)(
                  ZIO.fail(ValidationError(s"Query has too many fields: $fields. Max fields: $maxFields.", ""))
                )
              }
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
  ): Wrapper.EffectfulWrapper[Any] =
    FieldMetrics.wrapper(totalLabel, durationLabel, buckets, extraLabels)

  private def countFields(field: Field)(implicit trace: Trace): UIO[Int] =
    innerFields(field.fields)

  private def innerFields(fields: List[Field])(implicit trace: Trace): UIO[Int] =
    ZIO.foreach(fields)(countFields).map(_.sum + fields.length)

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
        val directives = info.parent.flatMap(_.allFieldsMap.get(info.name)).flatMap(_.directives).getOrElse(Nil)
        ZQuery.fromZIO(check(directives)) *> query
      }
    }
}
