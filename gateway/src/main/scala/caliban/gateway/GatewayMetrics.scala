package caliban.gateway

import caliban.gateway.GatewayWrapper.{ Event, Result }
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

  val wrapper: GatewayWrapper[Any] = new GatewayWrapper[Any] {
    def wrap[R, E, A](event: Event)(effect: ZIO[R, E, A])(result: Exit[E, A] => Result)(implicit
      trace: Trace
    ): ZIO[R, E, A] =
      event match {
        case Event.SubscriptionAdmission(accepted)          =>
          subscriptionAdmission.tagged("result", if (accepted) "accepted" else "rejected").increment *>
            subscriptionsActive.increment.when(accepted) *> effect
        case Event.SubscriptionTerminated(reason, duration) =>
          subscriptionsActive.decrement *> subscriptionTerminated
            .tagged("reason", reason)
            .increment *>
            subscriptionLifetime.update(seconds(duration)) *> effect
        case Event.SubscriptionOverflow                     => subscriptionOverflow.increment *> effect
        case Event.SubscriptionSetup                        => trackDuration(subscriptionSetup)(effect)(result)
        case Event.SubscriptionEvent                        =>
          trackDuration(subscriptionEventDuration)(effect)(result)
        case _: Event.Request                               =>
          track(
            requestsActive,
            requestDuration,
            requests,
            Set.empty,
            requestDetailLabels,
            requestTotalLabels
          )(effect)(result)
        case Event.Routing                                  =>
          trackDuration(routingDuration)(effect)(result)
        case Event.SubgraphCall(subgraph, _)                =>
          track(
            subgraphCallsActive,
            subgraphCallDuration,
            subgraphCalls,
            Set(MetricLabel("subgraph", subgraph)),
            subgraphDetailLabels,
            noLabels
          )(effect)(result)
        case _: Event.Attempt                               => effect
        case Event.Retry(subgraph, _)                       => retries.tagged("subgraph", subgraph).update(1L) *> effect
        case Event.Completion                               => effect
        case Event.CacheAccess(value)                       => cache.tagged("result", value.label).update(1L) *> effect
        case Event.Admission(kind)                          => admission.tagged("kind", kind.label).increment *> effect
      }
  }

  private def track[R, E, A](
    active: Metric.Gauge[Double],
    duration: Metric.Histogram[Double],
    total: Metric.Counter[Long],
    labels: Set[MetricLabel],
    detailLabels: Result => Set[MetricLabel],
    totalLabels: Result => Set[MetricLabel]
  )(effect: ZIO[R, E, A])(result: Exit[E, A] => Result)(implicit
    trace: Trace
  ): ZIO[R, E, A] =
    ZIO.uninterruptibleMask { restore =>
      Clock.nanoTime.flatMap { startedAt =>
        active.tagged(labels).increment *>
          restore(effect).onExit { exit =>
            Clock.nanoTime.flatMap { finishedAt =>
              val value = result(exit)
              duration.tagged(labels ++ detailLabels(value)).update(seconds(finishedAt - startedAt)) *>
                total.tagged(labels ++ totalLabels(value)).update(1L) *>
                active.tagged(labels).decrement
            }
          }
      }
    }

  private val requestDetailLabels: Result => Set[MetricLabel] = result =>
    Set(
      MetricLabel("outcome", result.outcome.label),
      MetricLabel("operation_type", result.operationType.fold("unknown")(GatewayWrapper.operationTypeLabel))
    )

  private val requestTotalLabels: Result => Set[MetricLabel] = result =>
    Set(MetricLabel("outcome", if (result.outcome == Success) "success" else "error"))

  private val subgraphDetailLabels: Result => Set[MetricLabel] = result =>
    Set(MetricLabel("outcome", result.outcome.label))

  private val noLabels: Result => Set[MetricLabel] = _ => Set.empty

  private def trackDuration[R, E, A](duration: Metric.Histogram[Double])(
    effect: ZIO[R, E, A]
  )(result: Exit[E, A] => Result)(implicit trace: Trace): ZIO[R, E, A] =
    Clock.nanoTime.flatMap { startedAt =>
      effect.onExit { exit =>
        Clock.nanoTime.flatMap { finishedAt =>
          duration
            .tagged("outcome", result(exit).outcome.label)
            .update(seconds(finishedAt - startedAt))
        }
      }
    }

  private def seconds(nanos: Long): Double = nanos.toDouble / 1000000000d
}
