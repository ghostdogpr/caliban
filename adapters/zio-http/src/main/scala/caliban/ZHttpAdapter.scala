package caliban

import caliban.interop.tapir.ws.Protocol
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import sttp.capabilities.zio.ZioStreams
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }
import zio.http._

object ZHttpAdapter {

  val defaultWebSocketConfig: WebSocketConfig = {
    val subProtocols = List(Protocol.Legacy.name, Protocol.GraphQLWS.name).mkString(",")
    WebSocketConfig.default.subProtocol(Some(subProtocols))
  }

  def makeHttpService[R, E](interpreter: HttpInterpreter[R, E])(implicit
    serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]
  ): HttpApp[R] =
    ZioHttpInterpreter(serverOptions)
      .toHttp(interpreter.serverEndpoints[R, ZioStreams](ZioStreams))

  def makeWebSocketService[R, E](interpreter: WebSocketInterpreter[R, E])(implicit
    serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]
  ): HttpApp[R] =
    ZioHttpInterpreter(serverOptions)
      .toHttp(interpreter.serverEndpoint[R])
}
