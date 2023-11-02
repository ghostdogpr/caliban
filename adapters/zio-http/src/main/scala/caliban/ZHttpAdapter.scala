package caliban

import caliban.interop.tapir.ws.Protocol
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import sttp.capabilities.zio.ZioStreams
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }
import zio.http._
import sttp.model.HeaderNames.SecWebSocketProtocol
import sttp.tapir.model.ServerRequest

object ZHttpAdapter {

  @deprecated("Defining subprotocols in the server config is no longer required")
  val defaultWebSocketConfig: WebSocketConfig = {
    val subProtocols = List(Protocol.Legacy.name, Protocol.GraphQLWS.name).mkString(",")
    WebSocketConfig.default.subProtocol(Some(subProtocols))
  }

  val defaultWsConfig: ServerRequest => WebSocketConfig = { req =>
    val protocol = req.header(SecWebSocketProtocol).fold(Protocol.Legacy: Protocol)(Protocol.fromName)
    WebSocketConfig.default.subProtocol(Some(protocol.name))
  }

  def makeHttpService[R, E](interpreter: HttpInterpreter[R, E])(implicit
    serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]
  ): HttpApp[R] =
    ZioHttpInterpreter(serverOptions)
      .toHttp(interpreter.serverEndpoints[R, ZioStreams](ZioStreams))

  /**
   * Creates a websocket service from a [[WebSocketInterpreter]].
   *
   * Important: If overriding the default server options, make sure to include a custom websocket config that sets the
   * subprotocol to the one used by the client. Otherwise, the websocket connection will fail.
   * @see [[defaultWsConfig]]
   */
  def makeWebSocketService[R, E](interpreter: WebSocketInterpreter[R, E])(implicit
    serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R].withCustomWebSocketConfig(defaultWsConfig)
  ): HttpApp[R] =
    ZioHttpInterpreter(serverOptions)
      .toHttp(interpreter.serverEndpoint[R])
}
