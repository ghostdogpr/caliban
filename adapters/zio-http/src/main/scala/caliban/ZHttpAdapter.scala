package caliban

import caliban.Configurator.ExecutionConfiguration
import caliban.ws.WebSocketHooks
import zio.Duration
import zio.http.{ WebSocketConfig => ZWebSocketConfig, _ }

@deprecated(
  "The `caliban-zio-http` package is deprecated and scheduled to be removed in a future release. To use Caliban with zio-http, use the `caliban-quick` module instead",
  "2.6.0"
)
object ZHttpAdapter {

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    config: ExecutionConfiguration = ExecutionConfiguration()
  ): RequestHandler[R, Nothing] =
    QuickAdapter(interpreter).configure(config).handlers.api

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    keepAliveTime: Option[Duration] = None,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty,
    zHttpConfig: ZWebSocketConfig = ZWebSocketConfig.default
  ): RequestHandler[R, Nothing] = {
    val config = quick.WebSocketConfig(keepAliveTime, webSocketHooks, zHttpConfig)
    QuickAdapter(interpreter).configureWebSocket(config).handlers.webSocket
  }

}
