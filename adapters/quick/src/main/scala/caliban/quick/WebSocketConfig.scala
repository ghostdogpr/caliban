package caliban.quick

import caliban.ws.WebSocketHooks
import zio._
import zio.http.{ WebSocketConfig => ZWebSocketConfig }

case class WebSocketConfig[-R](
  keepAliveTime: Option[Duration],
  hooks: WebSocketHooks[R, Any],
  zHttpConfig: ZWebSocketConfig
) {
  def withHooks[R1](newHooks: WebSocketHooks[R1, Any]): WebSocketConfig[R & R1] =
    copy(hooks = hooks ++ newHooks)

  def withKeepAliveTime(time: Duration): WebSocketConfig[R] =
    copy(keepAliveTime = Some(time))

  def withZHttpConfig(newConfig: ZWebSocketConfig): WebSocketConfig[R] =
    copy(zHttpConfig = newConfig)
}

object WebSocketConfig {
  def default: WebSocketConfig[Any] = WebSocketConfig(None, WebSocketHooks.empty, ZWebSocketConfig.default)
}
