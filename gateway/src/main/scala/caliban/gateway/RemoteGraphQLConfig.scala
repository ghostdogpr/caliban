package caliban.gateway

import sttp.model.Header
import zio.{ Duration, ZIO }

/**
 * Immutable acquisition and execution configuration for one remote GraphQL-over-HTTP source.
 */
final class RemoteGraphQLConfig[-R] private (
  private[gateway] val acquisition: RemoteGraphQLConfig.Acquisition,
  private[gateway] val execution: RemoteGraphQLConfig.Execution,
  private[gateway] val effectfulHeaders: ZIO[R, Throwable, List[Header]]
) {

  /**
   * Transforms the stored schema-acquisition configuration.
   */
  def withAcquisition(
    configure: RemoteGraphQLConfig.Acquisition => RemoteGraphQLConfig.Acquisition
  ): RemoteGraphQLConfig[R] =
    new RemoteGraphQLConfig(configure(acquisition), execution, effectfulHeaders)

  /**
   * Transforms the stored request-execution configuration.
   */
  def withExecution(
    configure: RemoteGraphQLConfig.Execution => RemoteGraphQLConfig.Execution
  ): RemoteGraphQLConfig[R] =
    new RemoteGraphQLConfig(acquisition, configure(execution), effectfulHeaders)

  /**
   * Adds effectful request-execution headers and their environment requirement. These headers are not used for
   * schema acquisition.
   */
  def withExecutionHeadersZIO[R1](value: ZIO[R1, Throwable, List[Header]]): RemoteGraphQLConfig[R with R1] =
    new RemoteGraphQLConfig(acquisition, execution, effectfulHeaders.zipWith(value)(_ ::: _))

  private[gateway] def diagnostics(includeAcquisition: Boolean): List[String] =
    execution.diagnostics ::: (if (includeAcquisition) acquisition.diagnostics else Nil)
}

object RemoteGraphQLConfig {

  /**
   * Finite schema-acquisition configuration for one remote GraphQL source.
   */
  final class Acquisition private (
    private[gateway] val timeout: Duration,
    private[gateway] val maxResponseBytes: Int,
    private[gateway] val maxParsingDepth: Int,
    private[gateway] val headers: List[Header]
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
     * Sets the maximum JSON or GraphQL nesting depth parsed during schema acquisition.
     */
    def withMaxParsingDepth(value: Int): Acquisition =
      new Acquisition(timeout, maxResponseBytes, value, headers)

    /**
     * Sets static headers sent only during schema acquisition.
     */
    def withHeaders(values: Header*): Acquisition =
      new Acquisition(timeout, maxResponseBytes, maxParsingDepth, values.toList)

    private[gateway] def diagnostics: List[String] = {
      val protectedHeaders = headers.collect {
        case header if isProtocolHeader(header.name) =>
          s"Schema acquisition header '${header.name}' is owned by the GraphQL transport."
      }
      val timeoutError     =
        if (timeout.compareTo(Duration.Zero) <= 0 || timeout.compareTo(Duration.Infinity) >= 0)
          List("Schema acquisition timeout must be finite and positive.")
        else Nil
      val responseError    =
        if (maxResponseBytes <= 0) List("Schema acquisition maxResponseBytes must be positive.") else Nil
      val parsingError     =
        if (maxParsingDepth <= 0) List("Schema acquisition maxParsingDepth must be positive.") else Nil

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
   * Finite request-execution configuration for one remote GraphQL source.
   *
   * Outbound headers use this precedence, from lowest to highest: selected incoming headers,
   * configured static headers, effectful headers, and GraphQL transport headers.
   */
  final class Execution private (
    private[gateway] val timeout: Duration,
    private[gateway] val maxRequestBytes: Int,
    private[gateway] val maxResponseBytes: Int,
    private[gateway] val retries: Int,
    private[gateway] val retryBackoff: Duration,
    private[gateway] val maxConcurrentCalls: Int,
    private[gateway] val headers: List[Header],
    private[gateway] val forwardedHeaders: Set[String],
    private[gateway] val forwardAll: Boolean
  ) {

    /**
     * Sets the maximum duration of one logical source call, including retries.
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
     * Sets the maximum number of concurrent logical calls admitted for this source.
     */
    def withMaxConcurrentCalls(value: Int): Execution =
      copy(maxConcurrentCalls = value)

    /**
     * Sets static outbound headers.
     */
    def withHeaders(values: Header*): Execution =
      copy(headers = values.toList)

    /**
     * Selects incoming request headers to forward by case-insensitive name.
     */
    def forwardIncomingHeaders(names: String*): Execution =
      copy(forwardedHeaders = names.iterator.map(normalize).toSet, forwardAll = false)

    /**
     * Explicitly enables forwarding of all incoming headers except transport-owned headers.
     */
    def forwardAllIncomingHeaders: Execution =
      copy(forwardedHeaders = Set.empty, forwardAll = true)

    private[gateway] def diagnostics: List[String] = {
      val timeoutError            =
        if (timeout.compareTo(Duration.Zero) <= 0 || timeout.compareTo(Duration.Infinity) >= 0)
          List("Source execution timeout must be finite and positive.")
        else Nil
      val requestError            =
        if (maxRequestBytes <= 0) List("Source execution maxRequestBytes must be positive.") else Nil
      val responseError           =
        if (maxResponseBytes <= 0) List("Source execution maxResponseBytes must be positive.") else Nil
      val retryError              =
        if (retries < 0) List("Source execution retry count must be non-negative.") else Nil
      val backoffError            =
        if (retryBackoff.compareTo(Duration.Zero) < 0 || retryBackoff.compareTo(Duration.Infinity) >= 0)
          List("Source execution retry backoff must be finite and non-negative.")
        else Nil
      val maxConcurrentCallsError =
        if (maxConcurrentCalls <= 0) List("Source execution maxConcurrentCalls must be positive.") else Nil
      val protectedHeaders        = headers.collect {
        case header if isProtocolHeader(header.name) =>
          s"Source execution header '${header.name}' is owned by the GraphQL transport."
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
      headers: List[Header] = headers,
      forwardedHeaders: Set[String] = forwardedHeaders,
      forwardAll: Boolean = forwardAll
    ): Execution =
      new Execution(
        timeout,
        maxRequestBytes,
        maxResponseBytes,
        retries,
        retryBackoff,
        maxConcurrentCalls,
        headers,
        forwardedHeaders,
        forwardAll
      )
  }

  object Execution {

    /**
     * The default finite execution configuration. Retries and header forwarding are disabled.
     */
    val default: Execution =
      new Execution(
        timeout = Duration.fromSeconds(30),
        maxRequestBytes = 1024 * 1024,
        maxResponseBytes = 16 * 1024 * 1024,
        retries = 0,
        retryBackoff = Duration.fromMillis(100),
        maxConcurrentCalls = 64,
        headers = Nil,
        forwardedHeaders = Set.empty,
        forwardAll = false
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
