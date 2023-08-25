package caliban.wrappers

import caliban.execution.FieldInfo
import caliban.wrappers.Wrapper.OverallWrapper
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, ResponseValue }
import zio._
import zio.metrics.MetricKeyType.Histogram
import zio.metrics.{ Metric, MetricLabel }
import zio.query.ZQuery

import scala.jdk.CollectionConverters._

object FieldMetrics {
  private[caliban] val defaultBuckets = Histogram.Boundaries(
    Chunk(
      .005d, .01d, .025d, .05d, .075d, .1d, .25d, .5d, .75d, 1d, 2.5d, 5d, 7.5d, 10d
    )
  )

  private class Metrics private (
    succeeded: Metric.Counter[Long],
    failed: Metric.Counter[Long],
    duration: Metric.Histogram[Double]
  ) {
    def recordFailure(fieldName: String): UIO[Unit] = failed.tagged("field", fieldName).increment

    def recordSuccess(nodeOffsets: Map[Vector[Either[String, Int]], Long])(timing: Timing): UIO[Unit] = {
      val d = timing.duration - nodeOffsets.getOrElse(timing.path :+ Left(timing.name), 0L)
      succeeded.tagged("field", timing.fullName).increment *>
        duration.tagged("field", timing.fullName).update(d / 1e9)
    }
  }

  private object Metrics {
    def apply(counter: Metric.Counter[Long], histogram: Metric.Histogram[Double]): Metrics =
      new Metrics(
        succeeded = counter.tagged("status", "ok"),
        failed = counter.tagged("status", "error"),
        duration = histogram
      )
  }

  private case class Timing(
    name: String,
    path: Vector[Either[String, Int]],
    fullName: String,
    duration: Long
  )

  private case class Refs(
    timings: Ref[List[Timing]],
    failures: Ref[List[String]]
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
        refs      = Refs(timings, failures)
        metrics   = Metrics(
                      Metric.counter(totalLabel).tagged(extraLabels),
                      Metric.histogram(durationLabel, buckets).tagged(extraLabels)
                    )
      } yield overallWrapper(refs, metrics) |+| fieldWrapper(refs)
    )

  private def overallWrapper(refs: Refs, metrics: Metrics): OverallWrapper[Any] =
    new OverallWrapper[Any] {
      def wrap[R1](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          for {
            result     <- process(request)
            _          <- refs.failures.get.flatMap(ZIO.foreachDiscard(_)(metrics.recordFailure))
            timings    <- refs.timings.get
            nodeOffsets = resolveNodeOffsets(timings)
            _          <- ZIO.foreachDiscard(timings)(metrics.recordSuccess(nodeOffsets))
          } yield result
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

  private def fieldWrapper(refs: Refs): Wrapper.FieldWrapper[Any] =
    new Wrapper.FieldWrapper[Any] {
      def wrap[R](
        query: ZQuery[R, CalibanError.ExecutionError, ResponseValue],
        info: FieldInfo
      ): ZQuery[R, CalibanError.ExecutionError, ResponseValue] = {

        val fieldName: String = {
          val parent = info.parent.flatMap(_.name).getOrElse("Unknown")
          new StringBuilder().append(parent).append('.').append(info.name).result().intern()
        }

        def makeTiming(duration: Long) =
          Timing(
            name = info.name,
            path = info.path.view.reverse.toVector,
            fullName = fieldName,
            duration = duration
          )

        def recordFailure(e: CalibanError.ExecutionError) =
          ZQuery.fromZIO(refs.failures.update(fieldName :: _) *> ZIO.fail(e))

        for {
          summarized            <- query.summarized(Clock.nanoTime)((_, _)).catchAll(recordFailure)
          ((start, end), result) = summarized
          timing                 = makeTiming(duration = end - start)
          _                     <- ZQuery.fromZIO(refs.timings.update(timing :: _))
        } yield result
      }
    }

}
