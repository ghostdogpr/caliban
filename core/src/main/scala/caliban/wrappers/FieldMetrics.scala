package caliban.wrappers

import caliban.execution.FieldInfo
import caliban.wrappers.Wrapper.OverallWrapper
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, ResponseValue }
import zio._
import zio.metrics.MetricKeyType.Histogram
import zio.metrics.{ Metric, MetricKey, MetricLabel }
import zio.query.ZQuery

import scala.jdk.CollectionConverters._

object FieldMetrics {
  private[caliban] val defaultBuckets = Histogram.Boundaries(
    Chunk(
      .005d, .01d, .025d, .05d, .075d, .1d, .25d, .5d, .75d, 1d, 2.5d, 5d, 7.5d, 10d
    )
  )

  private class Metrics(
    totalLabel: String,
    durationLabel: String,
    buckets: Histogram.Boundaries,
    extraLabels: Set[MetricLabel]
  ) {

    def recordFailures(fieldNames: List[String]): UIO[Unit] =
      ZIO.foreachDiscard(fieldNames)(fn => failed.tagged("field", fn).increment)

    def recordSuccesses(nodeOffsets: Map[Vector[Either[String, Int]], Long], timings: List[Timing]): UIO[Unit] =
      ZIO.foreachDiscard(timings) { timing =>
        val d = timing.duration - nodeOffsets.getOrElse(timing.path :+ Left(timing.name), 0L)
        succeeded.tagged(Set(MetricLabel("field", timing.fullName))).increment *>
          duration.tagged(Set(MetricLabel("field", timing.fullName))).update(d / 1e9)
      }

    private lazy val failed = makeCounter("error")
    private val succeeded   = makeCounter("ok")
    private val duration    = Metric.fromMetricKey(MetricKey.histogram(durationLabel, buckets).tagged(extraLabels))

    private def makeCounter(status: String) =
      Metric.fromMetricKey(MetricKey.counter(totalLabel).tagged(extraLabels + MetricLabel("status", status)))
  }

  private case class Timing(
    name: String,
    path: Vector[Either[String, Int]],
    fullName: String,
    duration: Long
  )

  def wrapper(
    totalLabel: String = "graphql_fields_total",
    durationLabel: String = "graphql_fields_duration_seconds",
    buckets: Histogram.Boundaries = defaultBuckets,
    extraLabels: Set[MetricLabel] = Set.empty
  ): Wrapper.EffectfulWrapper[Any] =
    Wrapper.EffectfulWrapper(
      for {
        timings  <- Ref.make(List.empty[Timing])
        failures <- Ref.make(List.empty[String])
        metrics   = new Metrics(totalLabel, durationLabel, buckets, extraLabels)
      } yield overallWrapper(timings, failures, metrics) |+| fieldWrapper(timings, failures)
    )

  private def overallWrapper(
    timings: Ref[List[Timing]],
    failures: Ref[List[String]],
    metrics: Metrics
  ): OverallWrapper[Any] =
    new OverallWrapper[Any] {
      def wrap[R1](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          process(request) <*
            (for {
              _          <- failures.get.flatMap(metrics.recordFailures)
              timings    <- timings.get
              nodeOffsets = resolveNodeOffsets(timings)
              _          <- metrics.recordSuccesses(nodeOffsets, timings)
            } yield ()).forkDaemon
    }

  private def resolveNodeOffsets(timings: List[Timing]): Map[Vector[Either[String, Int]], Long] = {
    val map = new java.util.HashMap[Vector[Either[String, Int]], Long]()
    timings.foreach { t =>
      val iter     = t.path.inits
      var continue = true
      while (continue) {
        val segment = iter.next()
        if (!iter.hasNext) {
          continue = false // Last element of `.inits` is an empty list
        } else if (segment.last.isLeft) { // List indices are not fields so we don't care about recording their offset
          map.compute(
            segment,
            {
              case (_, v) if v >= t.duration =>
                continue = false // We know that any subsequent segments will have a smaller offset
                v
              case _                         => t.duration
            }
          )
        }
      }
    }
    map.asScala.toMap
  }

  private def fieldWrapper(timings: Ref[List[Timing]], failures: Ref[List[String]]): Wrapper.FieldWrapper[Any] =
    new Wrapper.FieldWrapper[Any] {
      def wrap[R](
        query: ZQuery[R, CalibanError.ExecutionError, ResponseValue],
        info: FieldInfo
      ): ZQuery[R, CalibanError.ExecutionError, ResponseValue] = {

        def fieldName: String = {
          val parent = info.parent.flatMap(_.name).getOrElse("Unknown")
          new StringBuilder().append(parent).append('.').append(info.name).result()
        }

        def makeTiming(duration: Long) =
          Timing(
            name = info.name,
            path = info.path.view.reverse.toVector,
            fullName = fieldName,
            duration = duration
          )

        def recordFailure(e: CalibanError.ExecutionError) =
          ZQuery.fromZIO(failures.update(fieldName :: _) *> ZIO.fail(e))

        for {
          summarized        <- query.summarized(Clock.nanoTime)((s, e) => e - s).catchAll(recordFailure)
          (duration, result) = summarized
          timing             = makeTiming(duration)
          _                 <- ZQuery.fromZIO(timings.update(timing :: _))
        } yield result
      }
    }

}
