package caliban.wrappers

import caliban.CalibanError
import caliban.ResponseValue
import caliban.execution.FieldInfo
import zio._
import zio.metrics.Metric
import zio.query.ZQuery
import zio.metrics.MetricKeyType.Histogram
import zio.metrics.MetricLabel

object FieldMetrics {
  private[caliban] val defaultBuckets = Histogram.Boundaries(
    Chunk(
      .005d, .01d, .025d, .05d, .075d, .1d, .25d, .5d, .75d, 1d, 2.5d, 5d, 7.5d, 10d
    )
  )

  type Timing = (String, Long)
  private def fieldDuration(name: String, field: String, buckets: Histogram.Boundaries) =
    Metric.histogram(name, buckets).tagged("field", field)

  private def fieldTotal(name: String, field: String) =
    Metric.counter(name).tagged("field", field)

  def wrapper(
    totalLabel: String = "graphql_fields_total",
    durationLabel: String = "graphql_fields_duration_seconds",
    buckets: Histogram.Boundaries = defaultBuckets,
    extraLabels: Set[MetricLabel] = Set.empty
  ): Wrapper.EffectfulWrapper[Any] =
    Wrapper.EffectfulWrapper(
      for {
        ref <- Ref.make(List.empty[Timing])
      } yield fieldDuration(totalLabel, durationLabel, ref, buckets, extraLabels)
    )

  private def fieldDuration(
    totalLabel: String,
    durationLabel: String,
    ref: Ref[List[Timing]],
    buckets: Histogram.Boundaries,
    extraLabels: Set[MetricLabel]
  ): Wrapper.FieldWrapper[Any] =
    new Wrapper.FieldWrapper[Any] {
      override def wrap[R](
        query: ZQuery[R, CalibanError.ExecutionError, ResponseValue],
        info: FieldInfo
      ): ZQuery[R, CalibanError.ExecutionError, ResponseValue] =
        for {
          summarized            <-
            query
              .ensuring(ZQuery.fromZIO(fieldTotal(totalLabel, fieldName(info)).tagged(extraLabels).increment))
              .summarized(Clock.nanoTime)((_, _))
          ((start, end), result) = summarized
          measure               <- ZQuery.fromZIO(for {
                                     currentPath <- ZIO.succeed(toPath(info))
                                     popped      <- ref.modify { state =>
                                                      val (popped, rest) = state.partition { case (path, _) =>
                                                        path.startsWith(currentPath)
                                                      }
                                                      (popped, rest)
                                                    }
                                     offset       = if (popped.isEmpty) ("", 0L)
                                                    else popped.maxBy { case (_, duration) => duration }
                                     duration     = end - start - offset._2
                                     _           <- ref.update(current => (currentPath, duration + offset._2) :: current)
                                   } yield duration / 1e9)
          _                     <-
            ZQuery.fromZIO(fieldDuration(durationLabel, fieldName(info), buckets).tagged(extraLabels).update(measure))
        } yield result
    }

  private def fieldName(fieldInfo: FieldInfo): String = {
    val parent = fieldInfo.parent.flatMap(_.name).getOrElse("Unknown")
    s"$parent.${fieldInfo.details.name}"
  }

  private def toPath(info: FieldInfo) =
    (Left(info.name) :: info.path).foldRight("") { case (part, acc) =>
      val withDot =
        if (acc == "") acc
        else s"$acc."

      withDot + (part match {
        case Left(value) => value
        case Right(i)    => i.toString
      })
    }
}
