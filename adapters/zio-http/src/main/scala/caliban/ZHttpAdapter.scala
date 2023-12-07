package caliban

import caliban.interop.tapir.ws.Protocol
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import sttp.capabilities.zio.ZioStreams
import sttp.model.HeaderNames
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }
import zio.http._

object ZHttpAdapter {

  @deprecated("Defining subprotocols in the server config is no longer required")
  val defaultWebSocketConfig: WebSocketConfig = {
    val subProtocols = List(Protocol.Legacy.name, Protocol.GraphQLWS.name).mkString(",")
    WebSocketConfig.default.subProtocol(Some(subProtocols))
  }

  def makeHttpService[R, E](interpreter: HttpInterpreter[R, E])(implicit
    serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]
  ): RequestHandler[R, Nothing] =
    ZioHttpInterpreter(serverOptions)
      .toHttp(interpreter.serverEndpoints[R, ZioStreams](ZioStreams))
      .toHandler

  def makeWebSocketService[R, E](interpreter: WebSocketInterpreter[R, E])(implicit
    serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]
  ): RequestHandler[R, Nothing] =
    ZioHttpInterpreter(patchWsServerOptions(serverOptions))
      .toHttp(interpreter.serverEndpoint[R])
      .toHandler

  private def patchWsServerOptions[R](serverOptions: ZioHttpServerOptions[R]) =
    serverOptions.withCustomWebSocketConfig { req =>
      val protocol = req.header(HeaderNames.SecWebSocketProtocol).fold(Protocol.Legacy: Protocol)(Protocol.fromName)
      serverOptions.customWebSocketConfig(req) match {
        case Some(existing) => existing.subProtocol(Some(protocol.name))
        case _              => WebSocketConfig.default.subProtocol(Some(protocol.name))
      }
    }
}
