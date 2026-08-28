package caliban.gateway

import caliban._
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
  private[gateway] def diagnostics: List[String] = {
    import GatewayConfigValidation._
    positive(maxActive, "Subscription maxActive must be positive.") :::
      positive(bufferSize, "Subscription bufferSize must be positive.") :::
      finitePositive(setupTimeout, "Subscription setupTimeout must be finite and positive.") :::
      finitePositive(eventTimeout, "Subscription eventTimeout must be finite and positive.")
  }
}

private[gateway] object SubscriptionTermination {
  def apply(code: String, message: String): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      message,
      extensions = Some(ResponseValue.ObjectValue(List("code" -> Value.StringValue(code))))
    )
  val Reload                                                            = apply("SUBSCRIPTION_SCHEMA_RELOAD", "Gateway schema changed; resubscribe to the new generation.")
  val Shutdown                                                          = apply("SUBSCRIPTION_SHUTDOWN", "Gateway is shutting down.")
  val Capacity                                                          = apply("SUBSCRIPTION_CAPACITY_EXCEEDED", "Gateway subscription capacity exceeded.")
  val Overflow                                                          = apply("SUBSCRIPTION_OVERFLOW", "Subscription terminated because its buffer overflowed.")
  val SetupTimeout                                                      = apply("SUBSCRIPTION_SETUP_TIMEOUT", "Subscription setup timed out.")
  val EventTimeout                                                      = apply("SUBSCRIPTION_EVENT_TIMEOUT", "Subscription event execution timed out.")
  val Source                                                            = apply("SUBSCRIPTION_SOURCE_ERROR", "Subscription source failed.")
  val TooLarge                                                          = apply("SUBSCRIPTION_EVENT_TOO_LARGE", "Subscription event exceeds the configured size limit.")
  private val failures                                                  =
    List(Reload, Shutdown, Capacity, Overflow, SetupTimeout, EventTimeout, Source, TooLarge)

  // Identity distinguishes gateway signals from remote errors carrying the same public code.
  // Only classify original terminal failures, never decoded event errors: the latter do not retain object identity.
  def isGatewayError(error: CalibanError.ExecutionError): Boolean = failures.exists(_ eq error)

  def code(error: CalibanError.ExecutionError): String =
    if (isGatewayError(error))
      error.extensions
        .flatMap(_.fields.collectFirst { case ("code", Value.StringValue(code)) => code })
        .getOrElse("SUBSCRIPTION_SOURCE_ERROR")
    else "SUBSCRIPTION_SOURCE_ERROR"
}
