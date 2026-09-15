package caliban.gateway

import caliban.InputValue
import zio.http.{ Scheme, URL }
import zio.Duration

/**
 * One upstream connection per subscription. No replay, automatic reconnect, or pooling.
 */
final case class RemoteSubscriptionConfig(
  transport: RemoteSubscriptionConfig.Transport = RemoteSubscriptionConfig.WebSocket,
  endpoint: Option[URL] = None,
  connectionInit: Option[InputValue] = None,
  connectionTimeout: Duration = Duration.fromSeconds(30),
  keepAliveInterval: Duration = Duration.fromSeconds(15),
  bufferSize: Int = 32
) {
  private[gateway] def diagnostics: List[String] = {
    import GatewayConfigValidation._
    positive(bufferSize, "Remote subscription bufferSize must be positive.") :::
      endpoint.toList.flatMap { url =>
        val allowed: Set[Scheme] = transport match {
          case RemoteSubscriptionConfig.WebSocket => Set(Scheme.HTTP, Scheme.HTTPS, Scheme.WS, Scheme.WSS)
          case _: RemoteSubscriptionConfig.Sse    => Set(Scheme.HTTP, Scheme.HTTPS)
        }
        if (url.scheme.exists(allowed)) Nil else List("Remote subscription endpoint has an unsupported URI scheme.")
      } :::
      List(connectionTimeout, keepAliveInterval).flatMap(
        finitePositive(_, "Remote subscription timeouts and keepalive interval must be finite and positive.")
      )
  }
}

object RemoteSubscriptionConfig {
  sealed trait Transport
  case object WebSocket                         extends Transport
  final case class Sse(useGet: Boolean = false) extends Transport
}
