package caliban.wrappers

import caliban.Value.StringValue
import caliban._
import caliban.execution.FieldInfo
import caliban.wrappers.Wrapper.OverallWrapper
import zio._
import zio.metrics.MetricKeyType.Histogram
import zio.metrics.{ Metric, MetricKey, MetricLabel }
import zio.query.ZQuery
import zio.stacktracer.TracingImplicits.disableAutoTrace

import java.util.concurrent.atomic.AtomicReference
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
    private implicit val trace: Trace = Trace.empty

    def recordFailures(fieldNames: List[String]): UIO[Unit] =
      ZIO.foreachDiscard(fieldNames)(fn => failed.tagged("field", fn).increment)

    def recordSuccesses(nodeOffsets: Map[Vector[PathValue], Long], timings: List[Timing]): UIO[Unit] =
      ZIO.foreachDiscard(timings) { timing =>
        val d = timing.duration - nodeOffsets.getOrElse(timing.path :+ StringValue(timing.name), 0L)
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
    path: Vector[PathValue],
    fullName: String,
    duration: Long
  )

  def wrapper(
    totalLabel: String = "graphql_fields_total",
    durationLabel: String = "graphql_fields_duration_seconds",
    buckets: Histogram.Boundaries = defaultBuckets,
    extraLabels: Set[MetricLabel] = Set.empty
  ): Wrapper.EffectfulWrapper[Any] = {
    implicit val trace: Trace = Trace.empty
    Wrapper.EffectfulWrapper(
      for {
        timings  <- ZIO.succeed(new AtomicReference(List.empty[Timing]))
        failures <- ZIO.succeed(new AtomicReference(List.empty[String]))
        clock    <- ZIO.clock
        metrics   = new Metrics(totalLabel, durationLabel, buckets, extraLabels)
      } yield overallWrapper(timings, failures, metrics) |+|
        Unsafe.unsafe(implicit us => fieldWrapper(clock.unsafe.nanoTime(), timings, failures))
    )
  }

  private def overallWrapper(
    timings: AtomicReference[List[Timing]],
    failures: AtomicReference[List[String]],
    metrics: Metrics
  ): OverallWrapper[Any] =
    new OverallWrapper[Any] {
      def wrap[R1](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          process(request) <*
            ZIO
              .blocking(for {
                _          <- ZIO.suspendSucceed(metrics.recordFailures(failures.get))
                ts         <- ZIO.succeed(timings.get)
                nodeOffsets = resolveNodeOffsets(ts)
                _          <- metrics.recordSuccesses(nodeOffsets, ts)
              } yield ())
              .forkDaemon
    }

  private def resolveNodeOffsets(timings: List[Timing]): Map[Vector[PathValue], Long] = {

    val map       = new java.util.HashMap[Vector[PathValue], Long]()
    var remaining = timings
    while (!remaining.isEmpty) {
      val t        = remaining.head
      val iter     = t.path.inits
      val duration = t.duration
      var continue = true
      while (continue) {
        val segment = iter.next()
        if (!iter.hasNext) {
          continue = false // Last element of `.inits` is an empty list
        } else if (segment.last.isKey) { // List indices are not fields so we don't care about recording their offset
          map.compute(
            segment,
            (_, v) =>
              if (v >= duration) {
                continue = false // We know that any subsequent segments will have a smaller offset
                v
              } else duration
          )
        }
      }
      remaining = remaining.tail
    }
    map.asScala.toMap
  }

  private def fieldWrapper(
    nanoTime: => Long,
    timings: AtomicReference[List[Timing]],
    failures: AtomicReference[List[String]]
  ): Wrapper.FieldWrapper[Any] =
    new Wrapper.FieldWrapper[Any] {
      def wrap[R](
        query: ZQuery[R, CalibanError.ExecutionError, ResponseValue],
        info: FieldInfo
      ): ZQuery[R, CalibanError.ExecutionError, ResponseValue] = {

        def fieldName: String = {
          val parent = info.parent.flatMap(_.name).getOrElse("Unknown")
          val name   = info.name
          new StringBuilder(name.length + parent.length + 1).append(parent).append('.').append(name).result()
        }

        def makeTiming(duration: Long) =
          Timing(
            name = info.name,
            path = info.path.reverse.toVector,
            fullName = fieldName,
            duration = duration
          )

        ZQuery.suspend {
          val st = nanoTime
          query.foldQuery(
            e =>
              ZQuery.fail {
                val _ = failures.updateAndGet(fieldName :: _)
                e
              },
            result =>
              ZQuery.succeed {
                val t = makeTiming(nanoTime - st)
                val _ = timings.updateAndGet(t :: _)
                result
              }
          )
        }
      }
    }

}
