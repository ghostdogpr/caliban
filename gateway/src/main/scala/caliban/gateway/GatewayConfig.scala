package caliban.gateway

import caliban.gateway.GatewayConfigValidation._
import zio.Duration

/**
 * Operation-preparation, planning, admission, lifecycle, and remote-error disclosure configuration for one built gateway interpreter.
 */
final class GatewayConfig private (
  val maxOperationCacheWeight: Long,
  val maxOperationTextBytes: Int,
  val maxOperationNesting: Int,
  val maxParsedOperationNodes: Int,
  val maxPlanningCandidates: Int,
  val maxPlanningExpansions: Int,
  val planningTimeout: Duration,
  val maxConcurrentRequests: Int,
  val maxConcurrentLocalCalls: Int,
  val requestTimeout: Duration,
  val drainTimeout: Duration,
  val remoteErrorDisclosure: RemoteGraphQLConfig.ErrorDisclosure,
  val subscriptions: GatewaySubscriptionConfig
) {
  def withSubscriptions(value: GatewaySubscriptionConfig): GatewayConfig = copy(subscriptions = value)

  /**
   * Sets the maximum total estimated weight of cached prepared operations and plans.
   */
  def withMaxOperationCacheWeight(value: Long): GatewayConfig =
    copy(maxOperationCacheWeight = value)

  /**
   * Sets the maximum UTF-8 byte length of canonical operation text.
   */
  def withMaxOperationTextBytes(value: Int): GatewayConfig =
    copy(maxOperationTextBytes = value)

  /**
   * Sets the maximum lexical nesting of an operation before parsing.
   */
  def withMaxOperationNesting(value: Int): GatewayConfig =
    copy(maxOperationNesting = value)

  /**
   * Sets the maximum number of nodes in a parsed operation document.
   */
  def withMaxParsedOperationNodes(value: Int): GatewayConfig =
    copy(maxParsedOperationNodes = value)

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
   * Sets the maximum number of concurrent calls to each local Caliban subgraph.
   */
  def withMaxConcurrentLocalCalls(value: Int): GatewayConfig =
    copy(maxConcurrentLocalCalls = value)

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
   * Transforms the default disclosure policy for GraphQL errors returned by remote subgraphs.
   */
  def withRemoteErrorDisclosure(
    configure: RemoteGraphQLConfig.ErrorDisclosure => RemoteGraphQLConfig.ErrorDisclosure
  ): GatewayConfig =
    copy(remoteErrorDisclosure = configure(remoteErrorDisclosure))

  private[gateway] def diagnostics: List[String] =
    List(
      positive(maxOperationCacheWeight, "Gateway operation cache weight must be positive."),
      positive(maxOperationTextBytes, "Gateway maxOperationTextBytes must be positive."),
      positive(maxOperationNesting, "Gateway maxOperationNesting must be positive."),
      positive(maxParsedOperationNodes, "Gateway maxParsedOperationNodes must be positive."),
      positive(maxPlanningCandidates, "Gateway maxPlanningCandidates must be positive."),
      positive(maxPlanningExpansions, "Gateway maxPlanningExpansions must be positive."),
      finitePositive(planningTimeout, "Gateway planning timeout must be finite and positive."),
      positive(maxConcurrentRequests, "Gateway maxConcurrentRequests must be positive."),
      positive(maxConcurrentLocalCalls, "Gateway maxConcurrentLocalCalls must be positive."),
      finitePositive(requestTimeout, "Gateway request timeout must be finite and positive."),
      finitePositive(drainTimeout, "Gateway drain timeout must be finite and positive.")
    ).flatten ::: subscriptions.diagnostics

  private def copy(
    maxOperationCacheWeight: Long = maxOperationCacheWeight,
    maxOperationTextBytes: Int = maxOperationTextBytes,
    maxOperationNesting: Int = maxOperationNesting,
    maxParsedOperationNodes: Int = maxParsedOperationNodes,
    maxPlanningCandidates: Int = maxPlanningCandidates,
    maxPlanningExpansions: Int = maxPlanningExpansions,
    planningTimeout: Duration = planningTimeout,
    maxConcurrentRequests: Int = maxConcurrentRequests,
    maxConcurrentLocalCalls: Int = maxConcurrentLocalCalls,
    requestTimeout: Duration = requestTimeout,
    drainTimeout: Duration = drainTimeout,
    remoteErrorDisclosure: RemoteGraphQLConfig.ErrorDisclosure = remoteErrorDisclosure,
    subscriptions: GatewaySubscriptionConfig = subscriptions
  ): GatewayConfig =
    new GatewayConfig(
      maxOperationCacheWeight,
      maxOperationTextBytes,
      maxOperationNesting,
      maxParsedOperationNodes,
      maxPlanningCandidates,
      maxPlanningExpansions,
      planningTimeout,
      maxConcurrentRequests,
      maxConcurrentLocalCalls,
      requestTimeout,
      drainTimeout,
      remoteErrorDisclosure,
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
      maxOperationTextBytes = 1024 * 1024,
      maxOperationNesting = 128,
      maxParsedOperationNodes = 100000,
      maxPlanningCandidates = 8192,
      maxPlanningExpansions = 100000,
      planningTimeout = Duration.fromSeconds(2),
      maxConcurrentRequests = 1024,
      maxConcurrentLocalCalls = 64,
      requestTimeout = Duration.fromSeconds(30),
      drainTimeout = Duration.fromSeconds(30),
      remoteErrorDisclosure = RemoteGraphQLConfig.ErrorDisclosure.default,
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
