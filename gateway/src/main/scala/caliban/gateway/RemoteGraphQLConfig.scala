package caliban.gateway

import caliban.InputValue
import caliban.gateway.GatewayConfigValidation._
import caliban.gateway.RemoteGraphQLConfig.{ Acquisition, Execution }
import zio.http.{ Header, Scheme, URL }
import zio.{ Duration, ZIO }

/**
 * Immutable acquisition and execution configuration for one remote GraphQL-over-HTTP subgraph.
 */
final class RemoteGraphQLConfig[-R] private (
  val acquisition: Acquisition,
  val execution: Execution,
  val effectfulHeaders: ZIO[R, Throwable, List[Header]],
  val subscription: RemoteSubscriptionConfig = RemoteSubscriptionConfig.default
) {

  /**
   * Transforms the upstream subscription configuration.
   */
  def withSubscription(configure: RemoteSubscriptionConfig => RemoteSubscriptionConfig): RemoteGraphQLConfig[R] =
    new RemoteGraphQLConfig(acquisition, execution, effectfulHeaders, configure(subscription))

  /**
   * Transforms the stored schema-acquisition configuration.
   */
  def withAcquisition(configure: Acquisition => Acquisition): RemoteGraphQLConfig[R] =
    new RemoteGraphQLConfig(configure(acquisition), execution, effectfulHeaders, subscription)

  /**
   * Transforms the stored request-execution configuration.
   */
  def withExecution(configure: Execution => Execution): RemoteGraphQLConfig[R] =
    new RemoteGraphQLConfig(acquisition, configure(execution), effectfulHeaders, subscription)

  /**
   * Adds effectful request-execution headers and their environment requirement. These headers are not used for
   * schema acquisition. Repeated header names are joined into one comma-separated outbound value.
   */
  def withExecutionHeadersZIO[R1 <: R](value: ZIO[R1, Throwable, List[Header]]): RemoteGraphQLConfig[R1] =
    new RemoteGraphQLConfig(acquisition, execution, effectfulHeaders.zipWith(value)(_ ::: _), subscription)

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
    val maxRedirects: Int,
    val headers: List[Header]
  ) {

    /**
     * Sets the maximum duration of schema acquisition.
     */
    def withTimeout(value: Duration): Acquisition =
      new Acquisition(value, maxResponseBytes, maxParsingDepth, maxRedirects, headers)

    /**
     * Sets the maximum schema response body size.
     */
    def withMaxResponseBytes(value: Int): Acquisition =
      new Acquisition(timeout, value, maxParsingDepth, maxRedirects, headers)

    /**
     * Sets the maximum JSON and embedded GraphQL nesting depth parsed during schema acquisition.
     */
    def withMaxParsingDepth(value: Int): Acquisition =
      new Acquisition(timeout, maxResponseBytes, value, maxRedirects, headers)

    /**
     * Sets how many redirects schema acquisition follows. Zero, the default, refuses them outright.
     * Acquisition headers are not resent to a redirect target, so a token configured here never
     * reaches the host a redirect points at.
     */
    def withMaxRedirects(value: Int): Acquisition =
      new Acquisition(timeout, maxResponseBytes, maxParsingDepth, value, headers)

    /**
     * Sets static headers sent only during schema acquisition. Repeated header names are joined into one comma-separated
     * outbound value.
     */
    def withHeaders(values: Header*): Acquisition =
      new Acquisition(timeout, maxResponseBytes, maxParsingDepth, maxRedirects, values.toList)

    private[gateway] def diagnostics: List[String] = {
      val protectedHeaders = protocolHeaderDiagnostics("Schema acquisition", headers)
      val timeoutError     = finitePositive(timeout, "Schema acquisition timeout must be finite and positive.")
      val responseError    = positive(maxResponseBytes, "Schema acquisition maxResponseBytes must be positive.")
      val parsingError     = positive(maxParsingDepth, "Schema acquisition maxParsingDepth must be positive.")
      val redirectsError   = nonNegative(maxRedirects, "Schema acquisition maxRedirects must be non-negative.")

      timeoutError ::: responseError ::: parsingError ::: redirectsError ::: protectedHeaders
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
        maxRedirects = 0,
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
     * Enables or disables sharing one in-flight remote call between concurrent identical queries.
     */
    def withInFlightQueryDeduplication(value: Boolean): Execution =
      copy(inFlightQueryDeduplication = value)

    /**
     * Sets static outbound headers. Repeated header names are joined into one comma-separated outbound value.
     */
    def withHeaders(values: Header*): Execution =
      copy(headers = values.toList)

    /**
     * Selects incoming request headers to forward by case-insensitive name.
     */
    def forwardIncomingHeaders(names: String*): Execution =
      copy(forwardedHeaders = names.iterator.map(lowercaseHeaderName).toSet, forwardsAllIncomingHeaders = false)

    /**
     * Explicitly enables forwarding of all incoming headers except transport-owned headers.
     */
    def forwardAllIncomingHeaders: Execution =
      copy(forwardedHeaders = Set.empty, forwardsAllIncomingHeaders = true)

    private[gateway] def diagnostics: List[String] = {
      val timeoutError        = finitePositive(timeout, "Subgraph execution timeout must be finite and positive.")
      val requestError        = positive(maxRequestBytes, "Subgraph execution maxRequestBytes must be positive.")
      val responseError       = positive(maxResponseBytes, "Subgraph execution maxResponseBytes must be positive.")
      val retryError          = nonNegative(retries, "Subgraph execution retry count must be non-negative.")
      val backoffError        =
        finiteNonNegative(retryBackoff, "Subgraph execution retry backoff must be finite and non-negative.")
      val protectedHeaders    = protocolHeaderDiagnostics("Subgraph execution", headers)
      val protectedForwarding = forwardedHeaders.toList.sorted.collect {
        case name if isProtocolHeader(name) =>
          s"Incoming header '$name' is owned by the GraphQL transport and cannot be forwarded."
      }

      timeoutError ::: requestError ::: responseError ::: retryError ::: backoffError :::
        protectedHeaders ::: protectedForwarding
    }

    private def copy(
      timeout: Duration = timeout,
      maxRequestBytes: Int = maxRequestBytes,
      maxResponseBytes: Int = maxResponseBytes,
      retries: Int = retries,
      retryBackoff: Duration = retryBackoff,
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

  private[gateway] def lowercaseHeaderName(name: String): String =
    name.toLowerCase(java.util.Locale.ROOT)

  private[gateway] def isProtocolHeader(name: String): Boolean =
    ProtocolHeaders.contains(lowercaseHeaderName(name))

  private[gateway] def headerName(header: Header): String =
    header match {
      case custom: Header.Custom => custom.customName.toString
      case other                 => other.headerName
    }

  private def protocolHeaderDiagnostics(owner: String, headers: List[Header]): List[String] =
    headers.map(headerName).collect {
      case name if isProtocolHeader(name) => s"$owner header '$name' is owned by the GraphQL transport."
    }

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

/**
 * One upstream connection per subscription. No replay, automatic reconnect, or pooling.
 */
final class RemoteSubscriptionConfig private (
  val transport: RemoteSubscriptionConfig.Transport,
  val endpoint: Option[URL],
  val connectionInit: Option[InputValue],
  val connectionTimeout: Duration,
  val keepAliveInterval: Duration,
  val bufferSize: Int
) {

  /**
   * Selects the upstream subscription transport: [[RemoteSubscriptionConfig.WebSocket]] or
   * [[RemoteSubscriptionConfig.Sse]].
   */
  def withTransport(value: RemoteSubscriptionConfig.Transport): RemoteSubscriptionConfig = copy(transport = value)

  /**
   * Sets the subscription URL. By default, subscriptions use the subgraph's HTTP endpoint, with the `ws` or `wss`
   * scheme for WebSocket.
   */
  def withEndpoint(value: URL): RemoteSubscriptionConfig = copy(endpoint = Some(value))

  /**
   * Sets a static `connection_init` payload for WebSocket connections.
   */
  def withConnectionInit(value: InputValue): RemoteSubscriptionConfig = copy(connectionInit = Some(value))

  /**
   * Sets the time allowed for WebSocket acknowledgements, pong replies, and writes.
   */
  def withConnectionTimeout(value: Duration): RemoteSubscriptionConfig = copy(connectionTimeout = value)

  /**
   * Sets the interval between WebSocket pings.
   */
  def withKeepAliveInterval(value: Duration): RemoteSubscriptionConfig = copy(keepAliveInterval = value)

  /**
   * Sets the number of upstream messages buffered per subscription.
   */
  def withBufferSize(value: Int): RemoteSubscriptionConfig = copy(bufferSize = value)

  private[gateway] def diagnostics: List[String] =
    positive(bufferSize, "Remote subscription bufferSize must be positive.") :::
      endpoint.toList.flatMap { url =>
        val allowed: Set[Scheme] = transport match {
          case RemoteSubscriptionConfig.WebSocket => Set(Scheme.HTTP, Scheme.HTTPS, Scheme.WS, Scheme.WSS)
          case _: RemoteSubscriptionConfig.Sse    => Set(Scheme.HTTP, Scheme.HTTPS)
        }
        check(url.scheme.exists(allowed), "Remote subscription endpoint has an unsupported URI scheme.")
      } :::
      List(connectionTimeout, keepAliveInterval).flatMap(
        finitePositive(_, "Remote subscription timeouts and keepalive interval must be finite and positive.")
      )

  private def copy(
    transport: RemoteSubscriptionConfig.Transport = transport,
    endpoint: Option[URL] = endpoint,
    connectionInit: Option[InputValue] = connectionInit,
    connectionTimeout: Duration = connectionTimeout,
    keepAliveInterval: Duration = keepAliveInterval,
    bufferSize: Int = bufferSize
  ): RemoteSubscriptionConfig =
    new RemoteSubscriptionConfig(transport, endpoint, connectionInit, connectionTimeout, keepAliveInterval, bufferSize)
}

object RemoteSubscriptionConfig {

  /**
   * WebSocket on the subgraph endpoint, a 30-second connection timeout, 15-second pings, and 32 buffered messages.
   */
  val default: RemoteSubscriptionConfig =
    new RemoteSubscriptionConfig(
      transport = WebSocket,
      endpoint = None,
      connectionInit = None,
      connectionTimeout = Duration.fromSeconds(30),
      keepAliveInterval = Duration.fromSeconds(15),
      bufferSize = 32
    )

  sealed trait Transport
  case object WebSocket                         extends Transport
  final case class Sse(useGet: Boolean = false) extends Transport
}
