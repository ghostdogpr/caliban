package caliban.gateway.internal

import caliban.{ CalibanError, ResponseValue, Value }

private[gateway] object SubscriptionTermination {
  val Reload           = error("SUBSCRIPTION_SCHEMA_RELOAD", "Gateway schema changed; resubscribe to the new generation.")
  val Shutdown         = error("SUBSCRIPTION_SHUTDOWN", "Gateway is shutting down.")
  val Capacity         = error("SUBSCRIPTION_CAPACITY_EXCEEDED", "Gateway subscription capacity exceeded.")
  val Overflow         = error("SUBSCRIPTION_OVERFLOW", "Subscription terminated because its buffer overflowed.")
  val SetupTimeout     = error("SUBSCRIPTION_SETUP_TIMEOUT", "Subscription setup timed out.")
  val EventTimeout     = error("SUBSCRIPTION_EVENT_TIMEOUT", "Subscription event execution timed out.")
  val Source           = error("SUBSCRIPTION_SOURCE_ERROR", "Subscription source failed.")
  val TooLarge         = error("SUBSCRIPTION_EVENT_TOO_LARGE", "Subscription event exceeds the configured size limit.")
  private val failures = List(Reload, Shutdown, Capacity, Overflow, SetupTimeout, EventTimeout, Source, TooLarge)

  // Identity distinguishes gateway signals from remote errors carrying the same public code.
  // Only classify original terminal failures, never decoded event errors: the latter do not retain object identity.
  def isGatewayError(error: CalibanError.ExecutionError): Boolean = failures.exists(_ eq error)

  def code(error: CalibanError.ExecutionError): String =
    if (isGatewayError(error))
      error.extensions
        .flatMap(_.fields.collectFirst { case ("code", Value.StringValue(code)) => code })
        .getOrElse("SUBSCRIPTION_SOURCE_ERROR")
    else "SUBSCRIPTION_SOURCE_ERROR"

  private def error(code: String, message: String): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      message,
      extensions = Some(ResponseValue.ObjectValue(List("code" -> Value.StringValue(code))))
    )
}
