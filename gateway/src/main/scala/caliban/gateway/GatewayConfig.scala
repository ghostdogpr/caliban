package caliban.gateway

import caliban.gateway.GatewayConfigValidation._
import zio._

/**
 * Configuration for operation preparation, planning, request lifetimes, subscriptions, and remote error disclosure.
 */
final case class GatewayConfig private (
  maxOperationCacheWeight: Long,
  maxPlanningCandidates: Int,
  maxPlanningExpansions: Int,
  planningTimeout: Duration,
  maxOperationCost: Option[Long],
  requestTimeout: Duration,
  drainTimeout: Duration,
  reloadPollInterval: Duration,
  reloadJitter: Double,
  remoteErrorMessages: Boolean,
  subscriptions: GatewaySubscriptionConfig
) {

  /**
   * Transforms the subscription limits and timeouts.
   */
  def withSubscriptions(configure: GatewaySubscriptionConfig => GatewaySubscriptionConfig): GatewayConfig =
    copy(subscriptions = configure(subscriptions))

  /**
   * Sets the maximum total estimated weight of cached prepared operations and plans.
   * Custom validation functions participate in cache keys by equality (normally reference identity). Reuse those
   * functions across requests; allocating fresh lambdas causes cache misses and eviction churn.
   * The list itself may be rebuilt as long as it contains the same function instances.
   */
  def withMaxOperationCacheWeight(value: Long): GatewayConfig =
    copy(maxOperationCacheWeight = value)

  /**
   * Sets the maximum number of alternative route candidates considered while planning one operation.
   */
  def withMaxPlanningCandidates(value: Int): GatewayConfig =
    copy(maxPlanningCandidates = value)

  /**
   * Sets the maximum number of candidate plans expanded while planning one operation.
   */
  def withMaxPlanningExpansions(value: Int): GatewayConfig =
    copy(maxPlanningExpansions = value)

  /**
   * Sets the maximum duration spent planning one operation.
   */
  def withPlanningTimeout(value: Duration): GatewayConfig =
    copy(planningTimeout = value)

  /**
   * Rejects operations whose estimated cost exceeds `value`. Query and subscription operations have no base cost;
   * mutation operations have a base cost of ten. Composite output values cost one by default, while scalar and enum
   * values cost zero. Federation 2.9 `@cost` weights replace the default cost of the annotated schema element.
   */
  def withMaxOperationCost(value: Long): GatewayConfig =
    copy(maxOperationCost = Some(value))

  /**
   * Disables operation cost enforcement.
   */
  def withoutMaxOperationCost: GatewayConfig =
    copy(maxOperationCost = None)

  /**
   * Sets the maximum duration of one request, including preparation and response completion.
   */
  def withRequestTimeout(value: Duration): GatewayConfig =
    copy(requestTimeout = value)

  /**
   * Sets how long scope closure allows accepted requests to drain before interrupting them.
   */
  def withDrainTimeout(value: Duration): GatewayConfig =
    copy(drainTimeout = value)

  /**
   * Sets the delay after a completed reload cycle, including generation retirement. Cycles never overlap.
   */
  def withReloadPollInterval(value: Duration): GatewayConfig =
    copy(reloadPollInterval = value)

  /**
   * Sets fractional reload jitter in [0, 1). For example, 0.2 varies the delay by up to twenty percent.
   */
  def withReloadJitter(value: Double): GatewayConfig =
    copy(reloadJitter = value)

  /**
   * Enables remote GraphQL error messages. Only the `code` extension is retained.
   */
  def withRemoteErrorMessages(value: Boolean): GatewayConfig =
    copy(remoteErrorMessages = value)

  /**
   * The fastest delay [[reloadPollInterval]] and [[reloadJitter]] can produce, which is what a source with a
   * published polling floor has to be checked against.
   */
  private[gateway] def minimumReloadPollInterval: Duration = reloadDelay(0.0)

  private[gateway] def reloadDelay(random: Double): Duration =
    reloadPollInterval * (1.0 + (2.0 * random - 1.0) * reloadJitter)

  private[gateway] def diagnostics: List[String] =
    List(
      positive(maxOperationCacheWeight, "Gateway operation cache weight must be positive."),
      positive(maxPlanningCandidates, "Gateway maxPlanningCandidates must be positive."),
      positive(maxPlanningExpansions, "Gateway maxPlanningExpansions must be positive."),
      finitePositive(planningTimeout, "Gateway planning timeout must be finite and positive."),
      maxOperationCost.toList.flatMap(value => positive(value, "Gateway maxOperationCost must be positive.")),
      finitePositive(requestTimeout, "Gateway request timeout must be finite and positive."),
      finitePositive(drainTimeout, "Gateway drain timeout must be finite and positive."),
      finitePositive(reloadPollInterval, "Gateway reload poll interval must be finite and positive."),
      check(
        !reloadJitter.isNaN && reloadJitter >= 0.0 && reloadJitter < 1.0,
        "Gateway reload jitter must be finite and between zero (inclusive) and one (exclusive)."
      )
    ).flatten ::: subscriptions.diagnostics
}

object GatewayConfig {

  /**
   * The default finite gateway interpreter configuration.
   */
  val default: GatewayConfig =
    new GatewayConfig(
      maxOperationCacheWeight = 8L * 1024L * 1024L,
      maxPlanningCandidates = 8192,
      maxPlanningExpansions = 100000,
      planningTimeout = Duration.fromSeconds(2),
      maxOperationCost = None,
      requestTimeout = Duration.fromSeconds(30),
      drainTimeout = Duration.fromSeconds(30),
      reloadPollInterval = Duration.fromSeconds(30),
      reloadJitter = 0.2,
      remoteErrorMessages = false,
      subscriptions = GatewaySubscriptionConfig.default
    )
}

/**
 * Bounds active subscriptions and bursts. Overflow sheds the subscription; events are never silently dropped.
 */
final case class GatewaySubscriptionConfig private (
  maxActive: Int,
  bufferSize: Int,
  setupTimeout: Duration,
  eventTimeout: Duration
) {

  /**
   * Sets the maximum number of active subscriptions. New subscriptions are rejected at capacity.
   */
  def withMaxActive(value: Int): GatewaySubscriptionConfig = copy(maxActive = value)

  /**
   * Sets the number of events buffered per subscription. An overflow terminates the subscription.
   */
  def withBufferSize(value: Int): GatewaySubscriptionConfig = copy(bufferSize = value)

  /**
   * Sets the time allowed to open a subscription source, including hook work.
   */
  def withSetupTimeout(value: Duration): GatewaySubscriptionConfig = copy(setupTimeout = value)

  /**
   * Sets the time allowed to process one source event, regardless of the wait between events.
   */
  def withEventTimeout(value: Duration): GatewaySubscriptionConfig = copy(eventTimeout = value)

  private[gateway] def diagnostics: List[String] =
    positive(maxActive, "Subscription maxActive must be positive.") :::
      positive(bufferSize, "Subscription bufferSize must be positive.") :::
      finitePositive(setupTimeout, "Subscription setupTimeout must be finite and positive.") :::
      finitePositive(eventTimeout, "Subscription eventTimeout must be finite and positive.")
}

object GatewaySubscriptionConfig {

  /**
   * 1,024 active subscriptions, 32 buffered events each, and 30-second setup and event timeouts.
   */
  val default: GatewaySubscriptionConfig =
    new GatewaySubscriptionConfig(
      maxActive = 1024,
      bufferSize = 32,
      setupTimeout = Duration.fromSeconds(30),
      eventTimeout = Duration.fromSeconds(30)
    )
}

private[gateway] object GatewayConfigValidation {
  def positive(value: Long, message: String): List[String] =
    check(value > 0, message)

  def nonNegative(value: Long, message: String): List[String] =
    check(value >= 0, message)

  def finitePositive(value: Duration, message: String): List[String] =
    check(value.compareTo(Duration.Zero) > 0 && value.compareTo(Duration.Infinity) < 0, message)

  def finiteNonNegative(value: Duration, message: String): List[String] =
    check(value.compareTo(Duration.Zero) >= 0 && value.compareTo(Duration.Infinity) < 0, message)
}
