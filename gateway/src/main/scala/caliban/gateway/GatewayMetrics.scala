package caliban.gateway

import caliban.gateway.PhaseHooks.Outcome.Success
import caliban.gateway.PhaseHooks.{ Event, Result }
import zio.metrics.MetricKeyType.Histogram
import zio.metrics.{ Metric, MetricLabel }
import zio.{ Chunk, Clock, Trace, ZIO }

/**
 * Built-in bounded-cardinality gateway metrics.
 *
 * Attach [[hooks]] with `Gateway.compose(... ) @@ GatewayMetrics.hooks`. Metrics are opt-in so gateways that do not
 * collect them do not pay for clocks, labels, or metric-registry updates on their request path.
 */
object GatewayMetrics {
  private[gateway] val durationBuckets = Histogram.Boundaries(
    Chunk(.001d, .0025d, .005d, .01d, .025d, .05d, .1d, .25d, .5d, 1d, 2.5d, 5d, 10d, 30d, 60d)
  )

  private val requests                  = Metric.counter("caliban_gateway_requests_total")
  private val requestDuration           = Metric.histogram("caliban_gateway_request_duration_seconds", durationBuckets)
  private val requestsActive            = Metric.gauge("caliban_gateway_requests_active")
  private val routingDuration           = Metric.histogram("caliban_gateway_routing_duration_seconds", durationBuckets)
  private val subgraphCalls             = Metric.counter("caliban_gateway_subgraph_calls_total")
  private val subgraphCallDuration      = Metric.histogram("caliban_gateway_subgraph_call_duration_seconds", durationBuckets)
  private val subgraphCallsActive       = Metric.gauge("caliban_gateway_subgraph_calls_active")
  private val retries                   = Metric.counter("caliban_gateway_retries_total")
  private val cache                     = Metric.counter("caliban_gateway_operation_cache_total")
  private val admission                 = Metric.counter("caliban_gateway_admission_total")
  private val subscriptionsActive       = Metric.gauge("caliban_gateway_subscriptions_active")
  private val subscriptionAdmission     = Metric.counter("caliban_gateway_subscription_admission_total")
  private val subscriptionTerminated    = Metric.counter("caliban_gateway_subscription_terminations_total")
  private val subscriptionOverflow      = Metric.counter("caliban_gateway_subscription_overflows_total")
  private val subscriptionLifetime      = Metric.histogram(
    "caliban_gateway_subscription_duration_seconds",
    Histogram.Boundaries(Chunk(1d, 10d, 60d, 600d, 3600d, 86400d))
  )
  private val subscriptionSetup         =
    Metric.histogram("caliban_gateway_subscription_setup_duration_seconds", durationBuckets)
  private val subscriptionEventDuration =
    Metric.histogram("caliban_gateway_subscription_event_duration_seconds", durationBuckets)

  private val requestDetailLabels: Result => Set[MetricLabel] = result =>
    Set(
      MetricLabel("outcome", result.outcome.label),
      MetricLabel("operation_type", result.operationType.fold("unknown")(PhaseHooks.operationTypeLabel))
    )

  private val requestTotalLabels: Result => Set[MetricLabel] = result =>
    Set(MetricLabel("outcome", if (result.outcome == Success) "success" else "error"))

  private val subgraphDetailLabels: Result => Set[MetricLabel] = result =>
    Set(MetricLabel("outcome", result.outcome.label))

  private val noLabels: Result => Set[MetricLabel] = _ => Set.empty

  val hooks: PhaseHooks[Any] =
    PhaseHooks.subscriptionAdmission(
      PhaseHandler.incomingDiscard { ev =>
        subscriptionAdmission.tagged("result", if (ev.accepted) "accepted" else "rejected").increment *>
          subscriptionsActive.increment.whenDiscard(ev.accepted)
      }
    ) ++
      PhaseHooks
        .subscriptionTerminated(PhaseHandler.incomingDiscard { case Event.SubscriptionTerminated(reason, duration) =>
          subscriptionsActive.decrement *> subscriptionTerminated
            .tagged("reason", reason)
            .increment *>
            subscriptionLifetime.update(seconds(duration))
        }) ++
      PhaseHooks.subscriptionOverflow(PhaseHandler.incomingDiscard(_ => subscriptionOverflow.increment)) ++
      PhaseHooks.subscriptionSetup(trackPhaseDuration(subscriptionSetup)) ++
      PhaseHooks
        .request(
          trackPhase(
            requestsActive,
            requestDuration,
            requests,
            _ => Set.empty,
            requestDetailLabels,
            requestTotalLabels
          )
        ) ++
      PhaseHooks.subscriptionEvent(trackPhaseDuration(subscriptionEventDuration)) ++
      PhaseHooks.routing(trackPhaseDuration(routingDuration)) ++
      PhaseHooks.subgraphCall(
        trackPhase(
          subgraphCallsActive,
          subgraphCallDuration,
          subgraphCalls,
          ev => Set(MetricLabel("subgraph", ev.subgraph)),
          subgraphDetailLabels,
          noLabels
        )
      ) ++
      PhaseHooks.retry(PhaseHandler.incomingDiscard(ev => retries.tagged("subgraph", ev.subgraph).update(1L))) ++
      PhaseHooks.cacheAccess(
        PhaseHandler.incomingDiscard(ev => cache.tagged("result", ev.result.label).update(1L))
      ) ++
      PhaseHooks.admission(PhaseHandler.incomingDiscard(ev => admission.tagged("kind", ev.kind.label).increment))

  private def trackPhase[Event](
    active: Metric.Gauge[Double],
    duration: Metric.Histogram[Double],
    total: Metric.Counter[Long],
    labels: Event => Set[MetricLabel],
    detailLabels: Result => Set[MetricLabel],
    totalLabels: Result => Set[MetricLabel]
  ): PhaseHandler[Any, Event, Nothing, Result] =
    PhaseHandler((ev: Event) => enterTrack(active, labels(ev)).map(ev -> _))(
      (_, ctx: (Long, Set[MetricLabel]), out: Result) =>
        exitTrack(ctx._1, active, duration, total, ctx._2, detailLabels, totalLabels, out)
    )

  private def enterTrack(
    active: Metric.Gauge[Double],
    labels: Set[MetricLabel]
  )(implicit trace: Trace): ZIO[Any, Nothing, (Long, Set[MetricLabel])] =
    Clock.nanoTime.flatMap { startedAt =>
      active.tagged(labels).increment.as(startedAt -> labels)
    }

  private def exitTrack(
    startedAt: Long,
    active: Metric.Gauge[Double],
    duration: Metric.Histogram[Double],
    total: Metric.Counter[Long],
    labels: Set[MetricLabel],
    detailLabels: Result => Set[MetricLabel],
    totalLabels: Result => Set[MetricLabel],
    result: Result
  ): ZIO[Any, Nothing, Unit] =
    Clock.nanoTime.flatMap { finishedAt =>
      duration.tagged(labels ++ detailLabels(result)).update(seconds(finishedAt - startedAt)) *>
        total.tagged(labels ++ totalLabels(result)).update(1L) *>
        active.tagged(labels).decrement
    }

  private def trackPhaseDuration[Event](duration: Metric.Histogram[Double])(implicit
    trace: Trace
  ): PhaseHandler[Any, Event, Nothing, Result] =
    PhaseHandler((ev: Event) => Clock.nanoTime.map(ev -> _))((_, ctx: Long, out: Result) =>
      exitDuration(ctx, duration, out)
    )

  private def exitDuration(
    startedAt: Long,
    duration: Metric.Histogram[Double],
    result: Result
  ): ZIO[Any, Nothing, Unit] =
    Clock.nanoTime.flatMap { finishedAt =>
      duration
        .tagged("outcome", result.outcome.label)
        .update(seconds(finishedAt - startedAt))
    }

  private def seconds(nanos: Long): Double = nanos.toDouble / 1000000000d
}
