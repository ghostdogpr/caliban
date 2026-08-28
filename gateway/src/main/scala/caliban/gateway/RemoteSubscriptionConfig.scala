package caliban.gateway

import caliban.InputValue
import sttp.model.Uri
import zio.Duration

/**
 * One upstream connection per subscription. No replay, automatic reconnect, or pooling.
 */
final case class RemoteSubscriptionConfig(
  transport: RemoteSubscriptionConfig.Transport = RemoteSubscriptionConfig.WebSocket,
  endpoint: Option[Uri] = None,
  connectionInit: Option[InputValue] = None,
  connectionTimeout: Duration = Duration.fromSeconds(30),
  keepAliveInterval: Duration = Duration.fromSeconds(15),
  bufferSize: Int = 32
) {
  private[gateway] def diagnostics: List[String] = {
    import GatewayConfigValidation._
    positive(bufferSize, "Remote subscription bufferSize must be positive.") :::
      endpoint.toList.flatMap { uri =>
        val allowed = transport match {
          case RemoteSubscriptionConfig.WebSocket => Set("http", "https", "ws", "wss")
          case _: RemoteSubscriptionConfig.Sse    => Set("http", "https")
        }
        if (uri.scheme.exists(allowed)) Nil else List("Remote subscription endpoint has an unsupported URI scheme.")
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
