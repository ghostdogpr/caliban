package caliban.gateway.internal

import caliban.CalibanError
import caliban.gateway.errorCode

private[gateway] object SubscriptionTermination {
  // The class distinguishes gateway signals from remote errors carrying the same public code.
  // Only original terminal failures have it, never decoded event errors: the latter are plain execution errors.
  final class Error private[SubscriptionTermination] (val code: String, message: String)
      extends CalibanError.ExecutionError(message, extensions = errorCode(code))

  val Reload       = new Error("SUBSCRIPTION_SCHEMA_RELOAD", "Gateway schema changed; resubscribe to the new generation.")
  val Shutdown     = new Error("SUBSCRIPTION_SHUTDOWN", "Gateway is shutting down.")
  val Capacity     = new Error("SUBSCRIPTION_CAPACITY_EXCEEDED", "Gateway subscription capacity exceeded.")
  val Overflow     = new Error("SUBSCRIPTION_OVERFLOW", "Subscription terminated because its buffer overflowed.")
  val SetupTimeout = new Error("SUBSCRIPTION_SETUP_TIMEOUT", "Subscription setup timed out.")
  val EventTimeout = new Error("SUBSCRIPTION_EVENT_TIMEOUT", "Subscription event execution timed out.")
  val Source       = new Error("SUBSCRIPTION_SOURCE_ERROR", "Subscription source failed.")
  val TooLarge     = new Error("SUBSCRIPTION_EVENT_TOO_LARGE", "Subscription event exceeds the configured size limit.")

  def code(error: Throwable): String =
    error match {
      case error: Error => error.code
      case _            => Source.code
    }

  def fromFailure(error: Throwable): CalibanError.ExecutionError =
    error match {
      case error: CalibanError.ExecutionError => error
      case _                                  => Source
    }
}
