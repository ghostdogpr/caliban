package caliban.gateway

import zio.Duration

/**
 * Polling configuration for [[Gateway.reloadable]]. Acquisition and drain limits come from the gateway description.
 */
final class GatewayReloadConfig private (
  val pollInterval: Duration,
  val jitter: Double
) {

  /**
   * Delay after a completed cycle, including retirement. Cycles never overlap.
   */
  def withPollInterval(value: Duration): GatewayReloadConfig = new GatewayReloadConfig(value, jitter)

  /**
   * Fractional jitter in [0, 1). For example, 0.2 varies the delay by up to twenty percent in either direction.
   */
  def withJitter(value: Double): GatewayReloadConfig = new GatewayReloadConfig(pollInterval, value)

  private[gateway] def diagnostics: List[String] =
    GatewayConfigValidation.finitePositive(
      pollInterval,
      "Gateway reload poll interval must be finite and positive."
    ) :::
      (if (!jitter.isNaN && jitter >= 0.0 && jitter < 1.0) Nil
       else List("Gateway reload jitter must be finite and between zero (inclusive) and one (exclusive)."))
}

object GatewayReloadConfig {

  /**
   * Polls thirty seconds after each completed cycle, with twenty percent jitter.
   */
  val default: GatewayReloadConfig = new GatewayReloadConfig(Duration.fromSeconds(30), 0.2)
}
