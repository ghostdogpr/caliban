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

  private class Metrics(
    succeeded: Metric.Counter[Long],
    failed: Metric.Counter[Long],
    duration: Metric.Histogram[Double]
  ) {
    def addFailure(fieldName: String): UIO[Unit] =
      failed.tagged("field", fieldName).increment

    def addSuccess(pathTimings: Map[Vector[Either[String, Int]], Double])(timing: Timing): UIO[Unit] = {
      val offset = pathTimings.getOrElse(timing.path :+ Left(timing.name), 0.0)
      succeeded.tagged("field", timing.fullName).increment *>
        duration.tagged("field", timing.fullName).update(timing.duration - offset)
    }
  }

  private case class Timing(
    name: String,
    path: Vector[Either[String, Int]],
    fullName: String,
    duration: Double
  )

  private case class Refs(
    timings: Ref[Vector[Timing]],
    failures: Ref[List[String]],
    startTime: Ref[Long]
  )

  def wrapper(
    totalLabel: String = "graphql_fields_total",
    durationLabel: String = "graphql_fields_duration_seconds",
    buckets: Histogram.Boundaries = defaultBuckets,
    extraLabels: Set[MetricLabel] = Set.empty
  ): Wrapper.EffectfulWrapper[Any] =
    Wrapper.EffectfulWrapper(
      for {
        timings   <- Ref.make(Vector.empty[Timing])
        failures  <- Ref.make(List.empty[String])
        startTime <- Ref.make(0L)
        refs       = Refs(timings, failures, startTime)
        counter    = Metric.counter(totalLabel).tagged(extraLabels)
        metrics    = new Metrics(
                       succeeded = counter.tagged("status", "ok"),
                       failed = counter.tagged("status", "error"),
                       duration = Metric.histogram(durationLabel, buckets).tagged(extraLabels)
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
            nanoTime     <- Clock.nanoTime
            _            <- refs.startTime.set(nanoTime)
            result       <- process(request)
            _            <- refs.failures.get.flatMap(ZIO.foreachDiscard(_)(metrics.addFailure))
            timings      <- refs.timings.get
            nodeDurations = resolveNodeDurations(timings)
            _            <- ZIO.foreachDiscard(timings)(metrics.addSuccess(nodeDurations))
          } yield result
    }

  private def resolveNodeDurations(timings: Vector[Timing]): Map[Vector[Either[String, Int]], Double] = {
    val map = new java.util.HashMap[Vector[Either[String, Int]], Double]()
    timings.foreach { t =>
      val iter     = t.path.inits
      var continue = true
      while (continue) {
        val segment = iter.next()
        if (iter.hasNext) {
          if (segment.last.isLeft) {
            map.compute(
              segment,
              {
                case (_, v) if v >= t.duration => v
                case _                         =>
                  continue = false
                  t.duration
              }
            )
          }
        } else {
          continue = false
        }
      }
    }
    map.asScala.toMap
  }

  private def fieldWrapper(refs: Refs): Wrapper.FieldWrapper[Any] =
    new Wrapper.FieldWrapper[Any] {

      override def wrap[R](
        query: ZQuery[R, CalibanError.ExecutionError, ResponseValue],
        info: FieldInfo
      ): ZQuery[R, CalibanError.ExecutionError, ResponseValue] = {

        val fieldName: String = {
          val parent = info.parent.flatMap(_.name).getOrElse("Unknown")
          new StringBuilder().append(parent).append('.').append(info.name).result().intern()
        }

        def makeTiming(duration: Double) =
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
          timing                 = makeTiming(duration = (end - start) / 1e9)
          _                     <- ZQuery.fromZIO(refs.timings.update(_ :+ timing))
        } yield result
      }
    }

}
