package caliban.gateway

import caliban.gateway.GatewayConfigValidation._
import sttp.model.Header
import zio.{ Duration, ZIO }

/**
 * Immutable acquisition and execution configuration for one remote GraphQL-over-HTTP subgraph.
 */
final class RemoteGraphQLConfig[-R] private (
  val acquisition: RemoteGraphQLConfig.Acquisition,
  val execution: RemoteGraphQLConfig.Execution,
  val effectfulHeaders: ZIO[R, Throwable, List[Header]],
  val subscription: RemoteSubscriptionConfig = RemoteSubscriptionConfig()
) {
  def withSubscription(value: RemoteSubscriptionConfig): RemoteGraphQLConfig[R] =
    new RemoteGraphQLConfig(acquisition, execution, effectfulHeaders, value)

  /**
   * Transforms the stored schema-acquisition configuration.
   */
  def withAcquisition(
    configure: RemoteGraphQLConfig.Acquisition => RemoteGraphQLConfig.Acquisition
  ): RemoteGraphQLConfig[R] =
    new RemoteGraphQLConfig(configure(acquisition), execution, effectfulHeaders, subscription)

  /**
   * Transforms the stored request-execution configuration.
   */
  def withExecution(
    configure: RemoteGraphQLConfig.Execution => RemoteGraphQLConfig.Execution
  ): RemoteGraphQLConfig[R] =
    new RemoteGraphQLConfig(acquisition, configure(execution), effectfulHeaders, subscription)

  /**
   * Adds effectful request-execution headers and their environment requirement. These headers are not used for
   * schema acquisition. Repeated header names are preserved as separate outbound values.
   */
  def withExecutionHeadersZIO[R1 <: R](value: ZIO[R1, Throwable, List[Header]]): RemoteGraphQLConfig[R1] =
    new RemoteGraphQLConfig(
      acquisition,
      execution,
      effectfulHeaders.zipWith(value)(_ ::: _),
      subscription
    )

  private[gateway] def diagnostics(includeAcquisition: Boolean): List[String] =
    execution.diagnostics ::: (if (includeAcquisition) acquisition.diagnostics else Nil) ::: subscription.diagnostics

}

object RemoteGraphQLConfig {

  /**
   * Finite schema-acquisition configuration for one remote GraphQL subgraph.
   */
  final class Acquisition private (
    val timeout: Duration,
    val maxResponseBytes: Int,
    val maxParsingDepth: Int,
    val headers: List[Header]
  ) {

    /**
     * Sets the maximum duration of schema acquisition.
     */
    def withTimeout(value: Duration): Acquisition =
      new Acquisition(value, maxResponseBytes, maxParsingDepth, headers)

    /**
     * Sets the maximum schema response body size.
     */
    def withMaxResponseBytes(value: Int): Acquisition =
      new Acquisition(timeout, value, maxParsingDepth, headers)

    /**
     * Sets the maximum JSON and embedded GraphQL nesting depth parsed during schema acquisition.
     */
    def withMaxParsingDepth(value: Int): Acquisition =
      new Acquisition(timeout, maxResponseBytes, value, headers)

    /**
     * Sets static headers sent only during schema acquisition. Repeated header names are preserved as separate
     * outbound values.
     */
    def withHeaders(values: Header*): Acquisition =
      new Acquisition(timeout, maxResponseBytes, maxParsingDepth, values.toList)

    private[gateway] def diagnostics: List[String] = {
      val protectedHeaders = headers.collect {
        case header if isProtocolHeader(header.name) =>
          s"Schema acquisition header '${header.name}' is owned by the GraphQL transport."
      }
      val timeoutError     = finitePositive(timeout, "Schema acquisition timeout must be finite and positive.")
      val responseError    = positive(maxResponseBytes, "Schema acquisition maxResponseBytes must be positive.")
      val parsingError     = positive(maxParsingDepth, "Schema acquisition maxParsingDepth must be positive.")

      timeoutError ::: responseError ::: parsingError ::: protectedHeaders
    }
  }

  object Acquisition {

    /**
     * The default finite schema-acquisition configuration.
     */
    val default: Acquisition =
      new Acquisition(
        timeout = Duration.fromSeconds(10),
        maxResponseBytes = 16 * 1024 * 1024,
        maxParsingDepth = 128,
        headers = Nil
      )
  }

  /**
   * Finite request-execution configuration for one remote GraphQL subgraph.
   *
   * Outbound headers use this precedence, from lowest to highest: selected incoming headers,
   * configured static headers, effectful headers, and GraphQL transport headers.
   */
  final class Execution private (
    val timeout: Duration,
    val maxRequestBytes: Int,
    val maxResponseBytes: Int,
    val retries: Int,
    val retryBackoff: Duration,
    val maxConcurrentCalls: Int,
    val inFlightQueryDeduplication: Boolean,
    val headers: List[Header],
    val forwardedHeaders: Set[String],
    val forwardsAllIncomingHeaders: Boolean
  ) {

    /**
     * Sets the maximum duration of one logical subgraph call, including retries.
     */
    def withTimeout(value: Duration): Execution =
      copy(timeout = value)

    /**
     * Sets the maximum encoded request body size.
     */
    def withMaxRequestBytes(value: Int): Execution =
      copy(maxRequestBytes = value)

    /**
     * Sets the maximum response body size.
     */
    def withMaxResponseBytes(value: Int): Execution =
      copy(maxResponseBytes = value)

    /**
     * Enables a bounded number of retries for replay-safe operations and retryable failures.
     */
    def withRetries(count: Int, backoff: Duration): Execution =
      copy(retries = count, retryBackoff = backoff)

    /**
     * Sets the maximum number of concurrent logical calls admitted for this subgraph.
     */
    def withMaxConcurrentCalls(value: Int): Execution =
      copy(maxConcurrentCalls = value)

    /**
     * Enables or disables sharing one in-flight remote call between concurrent identical queries.
     */
    def withInFlightQueryDeduplication(value: Boolean): Execution =
      copy(inFlightQueryDeduplication = value)

    /**
     * Sets static outbound headers. Repeated header names are preserved as separate outbound values.
     */
    def withHeaders(values: Header*): Execution =
      copy(headers = values.toList)

    /**
     * Selects incoming request headers to forward by case-insensitive name.
     */
    def forwardIncomingHeaders(names: String*): Execution =
      copy(forwardedHeaders = names.iterator.map(normalize).toSet, forwardsAllIncomingHeaders = false)

    /**
     * Explicitly enables forwarding of all incoming headers except transport-owned headers.
     */
    def forwardAllIncomingHeaders: Execution =
      copy(forwardedHeaders = Set.empty, forwardsAllIncomingHeaders = true)

    private[gateway] def diagnostics: List[String] = {
      val timeoutError            = finitePositive(timeout, "Subgraph execution timeout must be finite and positive.")
      val requestError            = positive(maxRequestBytes, "Subgraph execution maxRequestBytes must be positive.")
      val responseError           = positive(maxResponseBytes, "Subgraph execution maxResponseBytes must be positive.")
      val retryError              = nonNegative(retries, "Subgraph execution retry count must be non-negative.")
      val backoffError            =
        finiteNonNegative(retryBackoff, "Subgraph execution retry backoff must be finite and non-negative.")
      val maxConcurrentCallsError =
        positive(maxConcurrentCalls, "Subgraph execution maxConcurrentCalls must be positive.")
      val protectedHeaders        = headers.collect {
        case header if isProtocolHeader(header.name) =>
          s"Subgraph execution header '${header.name}' is owned by the GraphQL transport."
      }
      val protectedForwarding     = forwardedHeaders.toList.sorted.collect {
        case name if isProtocolHeader(name) =>
          s"Incoming header '$name' is owned by the GraphQL transport and cannot be forwarded."
      }

      timeoutError ::: requestError ::: responseError ::: retryError ::: backoffError ::: maxConcurrentCallsError :::
        protectedHeaders ::: protectedForwarding
    }

    private def copy(
      timeout: Duration = timeout,
      maxRequestBytes: Int = maxRequestBytes,
      maxResponseBytes: Int = maxResponseBytes,
      retries: Int = retries,
      retryBackoff: Duration = retryBackoff,
      maxConcurrentCalls: Int = maxConcurrentCalls,
      inFlightQueryDeduplication: Boolean = inFlightQueryDeduplication,
      headers: List[Header] = headers,
      forwardedHeaders: Set[String] = forwardedHeaders,
      forwardsAllIncomingHeaders: Boolean = forwardsAllIncomingHeaders
    ): Execution =
      new Execution(
        timeout,
        maxRequestBytes,
        maxResponseBytes,
        retries,
        retryBackoff,
        maxConcurrentCalls,
        inFlightQueryDeduplication,
        headers,
        forwardedHeaders,
        forwardsAllIncomingHeaders
      )
  }

  object Execution {

    /**
     * The default finite execution configuration. In-flight query deduplication is enabled;
     * retries and header forwarding are disabled.
     */
    val default: Execution =
      new Execution(
        timeout = Duration.fromSeconds(30),
        maxRequestBytes = 1024 * 1024,
        maxResponseBytes = 16 * 1024 * 1024,
        retries = 0,
        retryBackoff = Duration.fromMillis(100),
        maxConcurrentCalls = 64,
        inFlightQueryDeduplication = true,
        headers = Nil,
        forwardedHeaders = Set.empty,
        forwardsAllIncomingHeaders = false
      )
  }

  /**
   * The default finite remote GraphQL configuration.
   */
  val default: RemoteGraphQLConfig[Any] =
    new RemoteGraphQLConfig(Acquisition.default, Execution.default, ZIO.succeed(Nil))

  private[gateway] def normalize(name: String): String =
    name.toLowerCase(java.util.Locale.ROOT)

  private[gateway] def isProtocolHeader(name: String): Boolean =
    ProtocolHeaders.contains(normalize(name))

  private val ProtocolHeaders = Set(
    "accept",
    "accept-encoding",
    "connection",
    "content-encoding",
    "content-length",
    "content-type",
    "host",
    "keep-alive",
    "proxy-authenticate",
    "proxy-authorization",
    "te",
    "trailer",
    "transfer-encoding",
    "upgrade"
  )
}
