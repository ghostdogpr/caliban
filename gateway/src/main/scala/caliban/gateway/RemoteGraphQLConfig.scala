package caliban.gateway

import caliban.gateway.GatewayConfigValidation._
import sttp.model.Header
import zio.{ Duration, ZIO }

/**
 * Immutable acquisition, execution, and error-disclosure configuration for one remote GraphQL-over-HTTP source.
 */
final class RemoteGraphQLConfig[-R] private (
  private[gateway] val acquisition: RemoteGraphQLConfig.Acquisition,
  private[gateway] val execution: RemoteGraphQLConfig.Execution,
  private[gateway] val effectfulHeaders: ZIO[R, Throwable, List[Header]],
  private[gateway] val errorDisclosure: Option[RemoteGraphQLConfig.ErrorDisclosure]
) {

  /**
   * Transforms the stored schema-acquisition configuration.
   */
  def withAcquisition(
    configure: RemoteGraphQLConfig.Acquisition => RemoteGraphQLConfig.Acquisition
  ): RemoteGraphQLConfig[R] =
    new RemoteGraphQLConfig(configure(acquisition), execution, effectfulHeaders, errorDisclosure)

  /**
   * Transforms the stored request-execution configuration.
   */
  def withExecution(
    configure: RemoteGraphQLConfig.Execution => RemoteGraphQLConfig.Execution
  ): RemoteGraphQLConfig[R] =
    new RemoteGraphQLConfig(acquisition, configure(execution), effectfulHeaders, errorDisclosure)

  /**
   * Overrides the gateway-wide disclosure policy for GraphQL errors returned by this source. The first
   * transformation starts from the secure source default; subsequent transformations use the stored override.
   */
  def withErrorDisclosure(
    configure: RemoteGraphQLConfig.ErrorDisclosure => RemoteGraphQLConfig.ErrorDisclosure
  ): RemoteGraphQLConfig[R] = {
    val current = errorDisclosure.getOrElse(RemoteGraphQLConfig.ErrorDisclosure.default)
    new RemoteGraphQLConfig(acquisition, execution, effectfulHeaders, Some(configure(current)))
  }

  /**
   * Adds effectful request-execution headers and their environment requirement. These headers are not used for
   * schema acquisition.
   */
  def withExecutionHeadersZIO[R1](value: ZIO[R1, Throwable, List[Header]]): RemoteGraphQLConfig[R with R1] =
    new RemoteGraphQLConfig(acquisition, execution, effectfulHeaders.zipWith(value)(_ ::: _), errorDisclosure)

  private[gateway] def diagnostics(includeAcquisition: Boolean): List[String] =
    execution.diagnostics ::: (if (includeAcquisition) acquisition.diagnostics else Nil)

  private[gateway] def withDefaultErrorDisclosure(value: RemoteGraphQLConfig.ErrorDisclosure): RemoteGraphQLConfig[R] =
    if (errorDisclosure.nonEmpty) this
    else new RemoteGraphQLConfig(acquisition, execution, effectfulHeaders, Some(value))
}

object RemoteGraphQLConfig {

  /**
   * Controls which untrusted remote GraphQL error details may be returned to clients.
   */
  final class ErrorDisclosure private (
    private[gateway] val includeMessages: Boolean,
    private[gateway] val extensionKeys: Set[String]
  ) {

    /**
     * Enables or disables disclosure of remote GraphQL error messages.
     */
    def withMessages(value: Boolean): ErrorDisclosure =
      new ErrorDisclosure(value, extensionKeys)

    /**
     * Adds remote GraphQL error extension keys to the default `code` allowlist.
     */
    def withAdditionalExtensionKeys(values: String*): ErrorDisclosure =
      new ErrorDisclosure(includeMessages, extensionKeys ++ values)
  }

  object ErrorDisclosure {

    /**
     * Redacts remote messages and retains only the `code` extension.
     */
    val default: ErrorDisclosure = new ErrorDisclosure(includeMessages = false, Set("code"))
  }

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
      val timeoutError            = finitePositive(timeout, "Source execution timeout must be finite and positive.")
      val requestError            = positive(maxRequestBytes, "Source execution maxRequestBytes must be positive.")
      val responseError           = positive(maxResponseBytes, "Source execution maxResponseBytes must be positive.")
      val retryError              = nonNegative(retries, "Source execution retry count must be non-negative.")
      val backoffError            =
        finiteNonNegative(retryBackoff, "Source execution retry backoff must be finite and non-negative.")
      val maxConcurrentCallsError =
        positive(maxConcurrentCalls, "Source execution maxConcurrentCalls must be positive.")
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
    new RemoteGraphQLConfig(Acquisition.default, Execution.default, ZIO.succeed(Nil), None)

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
