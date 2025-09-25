package caliban.quick

import zio._

case class SseConfig(
  heartbeatInterval: Option[Duration]
) {
  def withHeartbeatInterval(time: Duration): SseConfig =
    copy(heartbeatInterval = Some(time))
}

object SseConfig {
  def default: SseConfig = SseConfig(Some(10.seconds))
}
