package caliban.gateway

import caliban.InputValue
import caliban.gateway.GatewayConfigValidation._
import caliban.gateway.RemoteGraphQLConfig.Execution
import zio.http.{ Header, Scheme, URL }
import zio.{ Duration, ZIO }

/**
 * Immutable execution configuration for one remote GraphQL-over-HTTP subgraph.
 */
final case class RemoteGraphQLConfig[-R] private (
  execution: Execution,
  effectfulHeaders: ZIO[R, Throwable, List[Header]],
  subscription: RemoteSubscriptionConfig
) {

  /**
   * Transforms the upstream subscription configuration.
   */
  def withSubscription(configure: RemoteSubscriptionConfig => RemoteSubscriptionConfig): RemoteGraphQLConfig[R] =
    copy(subscription = configure(subscription))

  /**
   * Transforms the stored request-execution configuration.
   */
  def withExecution(configure: Execution => Execution): RemoteGraphQLConfig[R] =
    copy(execution = configure(execution))

  /**
   * Adds effectful request-execution headers and their environment requirement. These headers are not used for
   * schema acquisition. Repeated header names are joined into one comma-separated outbound value, or
   * semicolon-separated for `Cookie`.
   */
  def withExecutionHeadersZIO[R1 <: R](value: ZIO[R1, Throwable, List[Header]]): RemoteGraphQLConfig[R1] =
    copy(effectfulHeaders = effectfulHeaders.zipWith(value)(_ ::: _))
}

object RemoteGraphQLConfig {

  /**
   * Finite schema-acquisition configuration for a remote subgraph or supergraph source.
   */
  final case class Acquisition private (
    timeout: Duration,
    maxResponseBytes: Int,
    maxParsingDepth: Int,
    maxRedirects: Int,
    headers: List[Header]
  ) {

    /**
     * Sets the maximum duration of schema acquisition.
     */
    def withTimeout(value: Duration): Acquisition =
      copy(timeout = value)

    /**
     * Sets the maximum schema response body size.
     */
    def withMaxResponseBytes(value: Int): Acquisition =
      copy(maxResponseBytes = value)

    /**
     * Sets the maximum JSON and embedded GraphQL nesting depth parsed during schema acquisition.
     */
    def withMaxParsingDepth(value: Int): Acquisition =
      copy(maxParsingDepth = value)

    /**
     * Sets how many redirects schema acquisition follows. Zero refuses them. Configured headers never reach a
     * redirect target.
     */
    def withMaxRedirects(value: Int): Acquisition =
      copy(maxRedirects = value)

    /**
     * Sets static headers sent only during schema acquisition. Repeated header names are joined into one comma-separated
     * outbound value, or semicolon-separated for `Cookie`.
     */
    def withHeaders(values: Header*): Acquisition =
      copy(headers = values.toList)

    override def toString: String = "Acquisition(<redacted>)"

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
     * The default finite schema-acquisition configuration. It follows up to ten redirects.
     */
    val default: Acquisition =
      new Acquisition(
        timeout = Duration.fromSeconds(10),
        maxResponseBytes = 16 * 1024 * 1024,
        maxParsingDepth = 128,
        maxRedirects = 10,
        headers = Nil
      )
  }

  /**
   * Finite request-execution configuration for one remote GraphQL subgraph.
   *
   * Outbound headers use this precedence, from lowest to highest: selected incoming headers,
   * configured static headers, effectful headers, and GraphQL transport headers.
   */
  final case class Execution private (
    timeout: Duration,
    maxRequestBytes: Int,
    maxResponseBytes: Int,
    retries: Int,
    retryBackoff: Duration,
    inFlightQueryDeduplication: Boolean,
    headers: List[Header],
    forwardedHeaders: Option[Set[String]]
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
     * Sets static outbound headers. Repeated header names are joined into one comma-separated outbound value, or
     * semicolon-separated for `Cookie`.
     */
    def withHeaders(values: Header*): Execution =
      copy(headers = values.toList)

    /**
     * Selects incoming request headers to forward by case-insensitive name.
     */
    def forwardIncomingHeaders(names: String*): Execution =
      copy(forwardedHeaders = Some(names.toSet))

    /**
     * Explicitly enables forwarding of all incoming headers except transport-owned headers.
     */
    def forwardAllIncomingHeaders: Execution =
      copy(forwardedHeaders = None)

    override def toString: String = "Execution(<redacted>)"

    private[gateway] def diagnostics: List[String] = {
      val timeoutError        = finitePositive(timeout, "Subgraph execution timeout must be finite and positive.")
      val requestError        = positive(maxRequestBytes, "Subgraph execution maxRequestBytes must be positive.")
      val responseError       = positive(maxResponseBytes, "Subgraph execution maxResponseBytes must be positive.")
      val retryError          = nonNegative(retries, "Subgraph execution retry count must be non-negative.")
      val backoffError        =
        finiteNonNegative(retryBackoff, "Subgraph execution retry backoff must be finite and non-negative.")
      val protectedHeaders    = protocolHeaderDiagnostics("Subgraph execution", headers)
      val protectedForwarding = forwardedHeaders.toList.flatMap(_.toList.sorted).collect {
        case name if isProtocolHeader(name) =>
          s"Incoming header '$name' is owned by the GraphQL transport and cannot be forwarded."
      }

      timeoutError ::: requestError ::: responseError ::: retryError ::: backoffError :::
        protectedHeaders ::: protectedForwarding
    }
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
        forwardedHeaders = Some(Set.empty)
      )
  }

  /**
   * The default finite remote GraphQL configuration.
   */
  val default: RemoteGraphQLConfig[Any] =
    new RemoteGraphQLConfig(Execution.default, ZIO.succeed(Nil), RemoteSubscriptionConfig.default)

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
final case class RemoteSubscriptionConfig private (
  transport: RemoteSubscriptionConfig.Transport,
  endpoint: Option[URL],
  connectionInit: Option[InputValue],
  connectionTimeout: Duration,
  keepAliveInterval: Duration
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

  override def toString: String = "RemoteSubscriptionConfig(<redacted>)"

  private[gateway] def diagnostics: List[String] =
    endpoint.toList.flatMap { url =>
      val allowed: Set[Scheme] = transport match {
        case RemoteSubscriptionConfig.WebSocket => Set(Scheme.HTTP, Scheme.HTTPS, Scheme.WS, Scheme.WSS)
        case _: RemoteSubscriptionConfig.Sse    => Set(Scheme.HTTP, Scheme.HTTPS)
      }
      checkAbsolute(url, allowed, "Remote subscription endpoint must be an absolute URL with a supported scheme.")
    } :::
      List(connectionTimeout, keepAliveInterval).flatMap(
        finitePositive(_, "Remote subscription timeouts and keepalive interval must be finite and positive.")
      )
}

object RemoteSubscriptionConfig {

  /**
   * WebSocket on the subgraph endpoint, a 30-second connection timeout, and 15-second pings.
   */
  val default: RemoteSubscriptionConfig =
    new RemoteSubscriptionConfig(
      transport = WebSocket,
      endpoint = None,
      connectionInit = None,
      connectionTimeout = Duration.fromSeconds(30),
      keepAliveInterval = Duration.fromSeconds(15)
    )

  /**
   * The protocol used to open upstream subscriptions: `graphql-transport-ws` over WebSocket, or GraphQL over
   * Server-Sent Events, sent as a POST with a JSON body or, with `useGet`, as a GET with the operation in the query
   * string.
   */
  sealed trait Transport
  case object WebSocket                         extends Transport
  final case class Sse(useGet: Boolean = false) extends Transport
}
