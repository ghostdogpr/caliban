package caliban.gateway

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

  private final val OutcomeLabel  = "outcome"
  private final val SubgraphLabel = "subgraph"
  private final val ResultLabel   = "result"

  private val requestDuration           = Metric.histogram("caliban_gateway_request_duration_seconds", durationBuckets)
  private val requestsActive            = Metric.gauge("caliban_gateway_requests_active")
  private val preparationDuration       = Metric.histogram("caliban_gateway_preparation_duration_seconds", durationBuckets)
  private val subgraphCallDuration      = Metric.histogram("caliban_gateway_subgraph_call_duration_seconds", durationBuckets)
  private val subgraphCallsActive       = Metric.gauge("caliban_gateway_subgraph_calls_active")
  private val retries                   = Metric.counter("caliban_gateway_retries_total")
  private val cache                     = Metric.counter("caliban_gateway_operation_cache_total")
  private val subscriptionsActive       = Metric.gauge("caliban_gateway_subscriptions_active")
  private val subscriptionAdmission     = Metric.counter("caliban_gateway_subscription_admission_total")
  private val subscriptionTerminated    = Metric.counter("caliban_gateway_subscription_terminations_total")
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
      MetricLabel(OutcomeLabel, result.outcome.label),
      MetricLabel("operation_type", result.operationType.fold("unknown")(PhaseHooks.operationTypeLabel))
    )

  private val subgraphDetailLabels: Result => Set[MetricLabel] = result =>
    Set(MetricLabel(OutcomeLabel, result.outcome.label))

  /**
   * Records request, preparation, subgraph call, retry, cache, and subscription metrics under the
   * `caliban_gateway_` prefix. Labels hold outcomes, operation types, subgraph names, and termination reasons only.
   */
  val hooks: PhaseHooks[Any] =
    PhaseHooks.subscriptionAdmission(
      PhaseHandler.incomingDiscard { event =>
        subscriptionAdmission.tagged(ResultLabel, if (event.accepted) "accepted" else "rejected").increment *>
          subscriptionsActive.increment.whenDiscard(event.accepted)
      }
    ) ++
      PhaseHooks
        .subscriptionTerminated(PhaseHandler.incomingDiscard { case Event.SubscriptionTerminated(reason, duration) =>
          subscriptionsActive.decrement *> subscriptionTerminated
            .tagged("reason", reason)
            .increment *>
            subscriptionLifetime.update(seconds(duration))
        }) ++
      PhaseHooks.subscriptionSetup(trackPhaseDuration(subscriptionSetup)) ++
      PhaseHooks
        .execution(
          trackPhase(requestsActive, requestDuration, _ => Set.empty, requestDetailLabels)
        ) ++
      PhaseHooks.subscriptionEvent(trackPhaseDuration(subscriptionEventDuration)) ++
      PhaseHooks.preparation(trackPhaseDuration(preparationDuration)) ++
      PhaseHooks.subgraphCall(
        trackPhase(
          subgraphCallsActive,
          subgraphCallDuration,
          event => Set(MetricLabel(SubgraphLabel, event.subgraph)),
          subgraphDetailLabels
        )
      ) ++
      PhaseHooks.attempt(
        PhaseHandler.incomingDiscard(event =>
          retries.tagged(SubgraphLabel, event.subgraph).increment.whenDiscard(event.number > 0)
        )
      ) ++
      PhaseHooks.cacheAccess(
        PhaseHandler.incomingDiscard(event => cache.tagged(ResultLabel, event.result.label).update(1L))
      )

  private def trackPhase[Ev](
    active: Metric.Gauge[Double],
    duration: Metric.Histogram[Double],
    labels: Ev => Set[MetricLabel],
    detailLabels: Result => Set[MetricLabel]
  ): PhaseHandler[Any, Ev, Nothing, Result] =
    PhaseHandler { (event: Ev) =>
      val eventLabels = labels(event)
      Clock.nanoTime.flatMap(startedAt => active.tagged(eventLabels).increment.as(event -> (startedAt -> eventLabels)))
    } { case (_, (startedAt, eventLabels), result) =>
      Clock.nanoTime.flatMap { finishedAt =>
        duration.tagged(eventLabels ++ detailLabels(result)).update(seconds(finishedAt - startedAt)) *>
          active.tagged(eventLabels).decrement
      }
    }

  private def trackPhaseDuration[Ev](duration: Metric.Histogram[Double])(implicit
    trace: Trace
  ): PhaseHandler[Any, Ev, Nothing, Result] =
    PhaseHandler((event: Ev) => Clock.nanoTime.map(event -> _)) { (_, startedAt: Long, result: Result) =>
      Clock.nanoTime.flatMap { finishedAt =>
        duration.tagged(OutcomeLabel, result.outcome.label).update(seconds(finishedAt - startedAt))
      }
    }

  private def seconds(nanos: Long): Double = nanos.toDouble / 1000000000d
}
