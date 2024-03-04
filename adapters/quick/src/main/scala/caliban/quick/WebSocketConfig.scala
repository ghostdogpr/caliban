package caliban.quick

import caliban.ws.WebSocketHooks
import zio._

case class WebSocketConfig[-R](
  keepAliveTime: Option[Duration],
  hooks: WebSocketHooks[R, Any]
) {
  def withHooks[R1](newHooks: WebSocketHooks[R1, Any]): WebSocketConfig[R & R1] =
    copy(hooks = hooks ++ newHooks)

  def withKeepAliveTime(time: Duration): WebSocketConfig[R] =
    copy(keepAliveTime = Some(time))
}

object WebSocketConfig {
  val default: WebSocketConfig[Any] = WebSocketConfig(None, WebSocketHooks.empty)
}
