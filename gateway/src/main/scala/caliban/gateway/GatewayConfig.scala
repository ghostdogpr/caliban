package caliban.gateway

import caliban.gateway.GatewayConfigValidation._
import zio.Duration

/**
 * Finite operation-preparation, admission, and lifecycle limits for one built gateway runtime.
 */
final class GatewayConfig private (
  private[gateway] val maxOperationCacheWeight: Long,
  private[gateway] val maxOperationTextBytes: Int,
  private[gateway] val maxOperationNesting: Int,
  private[gateway] val maxParsedOperationNodes: Int,
  private[gateway] val maxConcurrentRequests: Int,
  private[gateway] val maxConcurrentLocalCalls: Int,
  private[gateway] val requestTimeout: Duration,
  private[gateway] val drainTimeout: Duration
) {

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
   * Sets the maximum number of requests executing within this runtime.
   */
  def withMaxConcurrentRequests(value: Int): GatewayConfig =
    copy(maxConcurrentRequests = value)

  /**
   * Sets the maximum number of concurrent calls to each local Caliban source.
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

  private[gateway] def diagnostics: List[String] =
    List(
      positive(maxOperationCacheWeight, "Gateway operation cache weight must be positive."),
      positive(maxOperationTextBytes, "Gateway maxOperationTextBytes must be positive."),
      positive(maxOperationNesting, "Gateway maxOperationNesting must be positive."),
      positive(maxParsedOperationNodes, "Gateway maxParsedOperationNodes must be positive."),
      positive(maxConcurrentRequests, "Gateway maxConcurrentRequests must be positive."),
      positive(maxConcurrentLocalCalls, "Gateway maxConcurrentLocalCalls must be positive."),
      finitePositive(requestTimeout, "Gateway request timeout must be finite and positive."),
      finitePositive(drainTimeout, "Gateway drain timeout must be finite and positive.")
    ).flatten

  private def copy(
    maxOperationCacheWeight: Long = maxOperationCacheWeight,
    maxOperationTextBytes: Int = maxOperationTextBytes,
    maxOperationNesting: Int = maxOperationNesting,
    maxParsedOperationNodes: Int = maxParsedOperationNodes,
    maxConcurrentRequests: Int = maxConcurrentRequests,
    maxConcurrentLocalCalls: Int = maxConcurrentLocalCalls,
    requestTimeout: Duration = requestTimeout,
    drainTimeout: Duration = drainTimeout
  ): GatewayConfig =
    new GatewayConfig(
      maxOperationCacheWeight,
      maxOperationTextBytes,
      maxOperationNesting,
      maxParsedOperationNodes,
      maxConcurrentRequests,
      maxConcurrentLocalCalls,
      requestTimeout,
      drainTimeout
    )
}

object GatewayConfig {

  /**
   * The default finite gateway runtime configuration.
   */
  val default: GatewayConfig =
    new GatewayConfig(
      maxOperationCacheWeight = 8L * 1024L * 1024L,
      maxOperationTextBytes = 1024 * 1024,
      maxOperationNesting = 128,
      maxParsedOperationNodes = 100000,
      maxConcurrentRequests = 1024,
      maxConcurrentLocalCalls = 64,
      requestTimeout = Duration.fromSeconds(30),
      drainTimeout = Duration.fromSeconds(30)
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
