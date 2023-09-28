package caliban

import caliban.interop.tapir.ws.Protocol
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import sttp.capabilities.zio.ZioStreams
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }
import zio.http._

object ZHttpAdapter {

  val defaultWebSocketConfig: WebSocketConfig = {
    val subProtocols = List(Protocol.Legacy.name, Protocol.GraphQLWS.name).mkString(",")
    WebSocketConfig.default.withSubProtocol(Some(subProtocols))
  }

  def makeHttpService[R, E](interpreter: HttpInterpreter[R, E])(implicit
    serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]
  ): App[R] =
    ZioHttpInterpreter(serverOptions)
      .toHttp(interpreter.serverEndpoints[R, ZioStreams](ZioStreams))
      .withDefaultErrorResponse

  def makeWebSocketService[R, E](interpreter: WebSocketInterpreter[R, E])(implicit
    serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]
  ): App[R] =
    ZioHttpInterpreter(serverOptions)
      .toHttp(interpreter.serverEndpoint[R])
      .withDefaultErrorResponse
}
