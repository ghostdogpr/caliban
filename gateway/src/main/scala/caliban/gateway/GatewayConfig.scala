package caliban.gateway

import caliban.gateway.GatewayConfigValidation._
import zio.Duration

/**
 * Operation-preparation, planning, admission, lifecycle, and remote-error disclosure configuration for one built gateway interpreter.
 */
final class GatewayConfig private (
  val maxOperationCacheWeight: Long,
  val maxPlanningCandidates: Int,
  val maxPlanningExpansions: Int,
  val planningTimeout: Duration,
  val maxConcurrentRequests: Int,
  val requestTimeout: Duration,
  val drainTimeout: Duration,
  val remoteErrorMessages: Boolean,
  val subscriptions: GatewaySubscriptionConfig
) {
  def withSubscriptions(value: GatewaySubscriptionConfig): GatewayConfig = copy(subscriptions = value)

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
   * Sets the maximum number of requests executing within this interpreter.
   */
  def withMaxConcurrentRequests(value: Int): GatewayConfig =
    copy(maxConcurrentRequests = value)

  /**
   * Sets the maximum duration of one request, including admission and response completion.
   */
  def withRequestTimeout(value: Duration): GatewayConfig =
    copy(requestTimeout = value)

  /**
   * Sets how long scope closure allows accepted requests to drain before interrupting them.
   */
  def withDrainTimeout(value: Duration): GatewayConfig =
    copy(drainTimeout = value)

  /**
   * Enables remote GraphQL error messages. Only the `code` extension is retained.
   */
  def withRemoteErrorMessages(value: Boolean): GatewayConfig =
    copy(remoteErrorMessages = value)

  private[gateway] def diagnostics: List[String] =
    List(
      positive(maxOperationCacheWeight, "Gateway operation cache weight must be positive."),
      positive(maxPlanningCandidates, "Gateway maxPlanningCandidates must be positive."),
      positive(maxPlanningExpansions, "Gateway maxPlanningExpansions must be positive."),
      finitePositive(planningTimeout, "Gateway planning timeout must be finite and positive."),
      positive(maxConcurrentRequests, "Gateway maxConcurrentRequests must be positive."),
      finitePositive(requestTimeout, "Gateway request timeout must be finite and positive."),
      finitePositive(drainTimeout, "Gateway drain timeout must be finite and positive.")
    ).flatten ::: subscriptions.diagnostics

  private def copy(
    maxOperationCacheWeight: Long = maxOperationCacheWeight,
    maxPlanningCandidates: Int = maxPlanningCandidates,
    maxPlanningExpansions: Int = maxPlanningExpansions,
    planningTimeout: Duration = planningTimeout,
    maxConcurrentRequests: Int = maxConcurrentRequests,
    requestTimeout: Duration = requestTimeout,
    drainTimeout: Duration = drainTimeout,
    remoteErrorMessages: Boolean = remoteErrorMessages,
    subscriptions: GatewaySubscriptionConfig = subscriptions
  ): GatewayConfig =
    new GatewayConfig(
      maxOperationCacheWeight,
      maxPlanningCandidates,
      maxPlanningExpansions,
      planningTimeout,
      maxConcurrentRequests,
      requestTimeout,
      drainTimeout,
      remoteErrorMessages,
      subscriptions
    )
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
      maxConcurrentRequests = 1024,
      requestTimeout = Duration.fromSeconds(30),
      drainTimeout = Duration.fromSeconds(30),
      remoteErrorMessages = false,
      subscriptions = GatewaySubscriptionConfig()
    )
}

private[gateway] object GatewayConfigValidation {
  def positive(value: Long, message: String): List[String] =
    if (value > 0) Nil else message :: Nil

  def nonNegative(value: Long, message: String): List[String] =
    if (value >= 0) Nil else message :: Nil

  def finitePositive(value: Duration, message: String): List[String] =
    if (value.compareTo(Duration.Zero) > 0 && value.compareTo(Duration.Infinity) < 0) Nil else message :: Nil

  def finiteNonNegative(value: Duration, message: String): List[String] =
    if (value.compareTo(Duration.Zero) >= 0 && value.compareTo(Duration.Infinity) < 0) Nil else message :: Nil
}
