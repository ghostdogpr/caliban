package caliban.wrappers

import caliban.Value.StringValue
import caliban._
import caliban.execution.FieldInfo
import caliban.wrappers.Wrapper.OverallWrapper
import zio._
import zio.metrics.MetricKeyType.Histogram
import zio.metrics.{ Metric, MetricKey, MetricLabel }
import zio.query.ZQuery

import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters._
import java.time.{ Clock => JClock }

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
  )(implicit clock: Clock = Clock.ClockLive): Wrapper[Any] =
    Wrapper.suspend {
      val timings  = new AtomicReference(List.empty[Timing])
      val failures = new AtomicReference(List.empty[String])
      val metrics  = new Metrics(totalLabel, durationLabel, buckets, extraLabels)
      overallWrapper(timings, failures, metrics) |+| fieldWrapper(clock.unsafe, timings, failures)
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
            (for {
              _          <- ZIO.suspendSucceed(metrics.recordFailures(failures.get))
              ts         <- ZIO.succeed(timings.get)
              nodeOffsets = resolveNodeOffsets(ts)
              _          <- metrics.recordSuccesses(nodeOffsets, ts)
            } yield ()).forkDaemon
    }

  private def resolveNodeOffsets(timings: List[Timing]): Map[Vector[PathValue], Long] = {

    val map       = new java.util.HashMap[Vector[PathValue], Long]()
    val nil       = Nil
    var remaining = timings
    while (remaining ne nil) {
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
    clock: Clock#UnsafeAPI,
    timings: AtomicReference[List[Timing]],
    failures: AtomicReference[List[String]]
  ): Wrapper.FieldWrapper[Any] =
    new Wrapper.FieldWrapper[Any] {
      import caliban.implicits.unsafe

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
          val st = clock.nanoTime()
          query.foldQuery(
            e =>
              ZQuery.fail {
                val _ = failures.updateAndGet(fieldName :: _)
                e
              },
            result =>
              ZQuery.succeed {
                val t = makeTiming(clock.nanoTime() - st)
                val _ = timings.updateAndGet(t :: _)
                result
              }
          )
        }
      }
    }

}
