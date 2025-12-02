package caliban.quick

import zio._

/**
 * Configuration settings for Server-Sent Events (SSE) connections.
 *
 * This configuration controls parameters like the frequency of heartbeat messages
 * sent to keep the connection alive.
 *
 * @param heartbeatInterval The interval at which a 'heartbeat' (a comment line)
 * should be sent to the client to prevent connection
 * timeouts. [[scala.None]] means no automatic heartbeats
 * will be sent by the server.
 */
case class SseConfig(
  heartbeatInterval: Option[Duration]
) {
  def withHeartbeatInterval(time: Duration): SseConfig =
    copy(heartbeatInterval = Some(time))
}

object SseConfig {
  def default: SseConfig = SseConfig(None)
}
