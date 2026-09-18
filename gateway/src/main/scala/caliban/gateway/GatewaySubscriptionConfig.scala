package caliban.gateway

import caliban.gateway.GatewayConfigValidation._
import zio.Duration

/**
 * Bounds active subscriptions and bursts. Overflow sheds the subscription; events are never silently dropped.
 */
final case class GatewaySubscriptionConfig(
  maxActive: Int = 1024,
  bufferSize: Int = 32,
  setupTimeout: Duration = Duration.fromSeconds(30),
  eventTimeout: Duration = Duration.fromSeconds(30)
) {
  private[gateway] def diagnostics: List[String] =
    positive(maxActive, "Subscription maxActive must be positive.") :::
      positive(bufferSize, "Subscription bufferSize must be positive.") :::
      finitePositive(setupTimeout, "Subscription setupTimeout must be finite and positive.") :::
      finitePositive(eventTimeout, "Subscription eventTimeout must be finite and positive.")
}
