package caliban.gateway

import caliban.gateway.GatewayWrapper.{ Event, Outcome, Result }
import caliban.gateway.GatewayWrapper.Outcome.Success
import zio.metrics.MetricKeyType.Histogram
import zio.metrics.{ Metric, MetricLabel }
import zio.{ Chunk, Clock, Exit, Trace, ZIO }

/**
 * Built-in bounded-cardinality gateway metrics.
 *
 * Attach [[wrapper]] with `Gateway.compose(... ) @@ GatewayMetrics.wrapper`. Metrics are opt-in so gateways that do not
 * collect them do not pay for clocks, labels, or metric-registry updates on their request path.
 */
object GatewayMetrics {
  private[gateway] val durationBuckets = Histogram.Boundaries(
    Chunk(.001d, .0025d, .005d, .01d, .025d, .05d, .1d, .25d, .5d, 1d, 2.5d, 5d, 10d, 30d, 60d)
  )
  private val byteBuckets              = Histogram.Boundaries(
    Chunk(256d, 1024d, 4096d, 16384d, 65536d, 262144d, 1048576d, 4194304d, 16777216d)
  )

  private val requests              = Metric.counter("caliban_gateway_requests_total")
  private val requestDuration       = Metric.histogram("caliban_gateway_request_duration_seconds", durationBuckets)
  private val requestsActive        = Metric.gauge("caliban_gateway_requests_active")
  private val routingDuration       = Metric.histogram("caliban_gateway_routing_duration_seconds", durationBuckets)
  private val sourceCalls           = Metric.counter("caliban_gateway_source_calls_total")
  private val sourceCallDuration    = Metric.histogram("caliban_gateway_source_call_duration_seconds", durationBuckets)
  private val sourceCallsActive     = Metric.gauge("caliban_gateway_source_calls_active")
  private val sourceAttempts        = Metric.counter("caliban_gateway_source_attempts_total")
  private val sourceAttemptDuration =
    Metric.histogram("caliban_gateway_source_attempt_duration_seconds", durationBuckets)
  private val sourceAttemptsActive  = Metric.gauge("caliban_gateway_source_attempts_active")
  private val sourceRequestBytes    = Metric.histogram("caliban_gateway_source_request_body_size_bytes", byteBuckets)
  private val sourceResponseBytes   = Metric.histogram("caliban_gateway_source_response_body_size_bytes", byteBuckets)
  private val retries               = Metric.counter("caliban_gateway_retries_total")
  private val cache                 = Metric.counter("caliban_gateway_operation_cache_total")
  private val admission             = Metric.counter("caliban_gateway_admission_total")
  private val admissionWait         =
    Metric.histogram("caliban_gateway_admission_wait_duration_seconds", durationBuckets)
  private val admissionActive       = Metric.gauge("caliban_gateway_admission_active")
  private val admissionWaiting      = Metric.gauge("caliban_gateway_admission_waiting")
  private val deduplicated          = Metric.counter("caliban_gateway_in_flight_deduplication_total")
  private val overdue               = Metric.counter("caliban_gateway_overdue_requests_total")

  val wrapper: GatewayWrapper[Any] = new GatewayWrapper[Any] {
    def wrap[R, E, A](event: Event)(effect: ZIO[R, E, A])(result: Exit[E, A] => Result)(implicit
      trace: Trace
    ): ZIO[R, E, A] =
      event match {
        case _: Event.Request            =>
          track(
            requestsActive,
            requestDuration,
            requests,
            Set.empty,
            totalOutcome = Some(value => if (value.outcome == Success) "success" else "error"),
            detailLabels = value =>
              Set(MetricLabel("operation_type", value.operationType.fold("unknown")(GatewayWrapper.operationTypeLabel)))
          )(effect)(result)
        case Event.Routing               =>
          trackDuration(routingDuration, Set.empty)(effect)(result)
        case Event.SourceCall(source, _) =>
          track(
            sourceCallsActive,
            sourceCallDuration,
            sourceCalls,
            Set(MetricLabel("source", source)),
            totalOutcome = None,
            detailLabels = _ => Set.empty
          )(effect)(result)
        case attempt: Event.Attempt      =>
          val labels = Set(MetricLabel("source", attempt.source))
          sourceRequestBytes.tagged(labels).update(attempt.requestBytes.toDouble) *>
            track(
              sourceAttemptsActive,
              sourceAttemptDuration,
              sourceAttempts,
              labels,
              totalOutcome = Some(_.outcome.label),
              detailLabels = _ => Set.empty,
              after = value =>
                value.responseBytes.fold[ZIO[Any, Nothing, Unit]](ZIO.unit)(bytes =>
                  sourceResponseBytes.tagged(labels).update(bytes.toDouble)
                )
            )(effect)(result)
        case Event.Retry(source, _)      => retries.tagged("source", source).update(1L) *> effect
        case Event.Completion            => effect
        case Event.CacheAccess(value)    => cache.tagged("result", value.label).update(1L) *> effect
        case Event.AdmissionWait(kind)   =>
          val labels  = Set(MetricLabel("kind", kind.label))
          val waiting = admissionWaiting.tagged(labels)
          waiting.increment *>
            trackDuration(admissionWait, labels)(effect)(result).ensuring(waiting.decrement)
        case Event.Admission(kind)       =>
          admission.tagged("kind", kind.label).update(1L) *>
            withActive(admissionActive.tagged("kind", kind.label))(effect)
        case Event.Deduplication(value)  => deduplicated.tagged("result", value.label).update(1L) *> effect
        case Event.RequestOverdue        => overdue.update(1L) *> effect
      }
  }

  private def track[R, E, A](
    active: Metric.Gauge[Double],
    duration: Metric.Histogram[Double],
    total: Metric.Counter[Long],
    labels: Set[MetricLabel],
    totalOutcome: Option[Result => String],
    detailLabels: Result => Set[MetricLabel],
    after: Result => ZIO[Any, Nothing, Unit] = (_: Result) => ZIO.unit
  )(effect: ZIO[R, E, A])(result: Exit[E, A] => Result)(implicit trace: Trace): ZIO[R, E, A] =
    ZIO.uninterruptibleMask { restore =>
      Clock.nanoTime.flatMap { startedAt =>
        active.tagged(labels).increment *>
          restore(effect).onExit { exit =>
            Clock.nanoTime.flatMap { finishedAt =>
              val value         = result(exit)
              val outcomeLabels = labels ++ detailLabels(value) + MetricLabel("outcome", value.outcome.label)
              val totalLabels   = totalOutcome.fold(labels)(render => labels + MetricLabel("outcome", render(value)))
              duration.tagged(outcomeLabels).update(seconds(finishedAt - startedAt)) *>
                total.tagged(totalLabels).update(1L) *>
                after(value) *>
                active.tagged(labels).decrement
            }
          }
      }
    }

  private def trackDuration[R, E, A](duration: Metric.Histogram[Double], labels: Set[MetricLabel])(
    effect: ZIO[R, E, A]
  )(result: Exit[E, A] => Result)(implicit trace: Trace): ZIO[R, E, A] =
    Clock.nanoTime.flatMap { startedAt =>
      effect.onExit { exit =>
        Clock.nanoTime.flatMap { finishedAt =>
          duration
            .tagged(labels + MetricLabel("outcome", result(exit).outcome.label))
            .update(seconds(finishedAt - startedAt))
        }
      }
    }

  private def withActive[R, E, A](active: Metric.Gauge[Double])(effect: ZIO[R, E, A])(implicit
    trace: Trace
  ): ZIO[R, E, A] =
    ZIO.uninterruptibleMask(restore => active.increment *> restore(effect).ensuring(active.decrement))

  private def seconds(nanos: Long): Double = nanos.toDouble / 1000000000d
}
