package caliban

import caliban.interop.tapir.ws.Protocol
import caliban.interop.tapir.{ HttpInterpreter, WebSocketHooks }
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.DecodeResult
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }
import zio._
import zio.http.ChannelEvent.UserEvent.HandshakeComplete
import zio.http.ChannelEvent.{ ChannelRead, UserEventTriggered }
import zio.http._
import zio.http.socket._
import zio.stream._

object ZHttpAdapter {

  def makeHttpService[R, E](interpreter: HttpInterpreter[R, E])(implicit
    serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]
  ): HttpApp[R, Throwable] =
    ZioHttpInterpreter(serverOptions).toHttp(interpreter.serverEndpoints[R])

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    keepAliveTime: Option[Duration] = None,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty
  )(implicit inputCodec: JsonCodec[GraphQLWSInput], outputCodec: JsonCodec[GraphQLWSOutput]): App[R] =
    Handler
      .fromFunctionZIO[Request] { req =>
        val protocol = req.headers.get(Header.SecWebSocketProtocol) match {
          case Some(value) => Protocol.fromName(value.renderedValue)
          case None        => Protocol.Legacy
        }

        for {
          queue <- Queue.unbounded[GraphQLWSInput]
          pipe  <- protocol.make(interpreter, keepAliveTime, webSocketHooks)
          in     = ZStream.fromQueueWithShutdown(queue)
          out    = pipe(in).map {
                     case Right(output) => WebSocketFrame.Text(outputCodec.encode(output))
                     case Left(close)   => WebSocketFrame.Close(close.code, Some(close.reason))
                   }
          socket = Http
                     .collectZIO[WebSocketChannelEvent] {
                       case ChannelEvent(ch, UserEventTriggered(HandshakeComplete)) =>
                         out.runForeach(ch.writeAndFlush(_)).race(ch.awaitClose)
                       case ChannelEvent(_, ChannelRead(WebSocketFrame.Text(text))) =>
                         ZIO.fromEither {
                           inputCodec.decode(text) match {
                             case DecodeResult.Value(v)    => Right(v)
                             case DecodeResult.Error(_, e) => Left(e)
                             case f: DecodeResult.Failure  =>
                               Left(new Throwable(s"failed to decode input: ${f.toString}"))
                           }
                         }.flatMap(queue.offer)
                     }
          app   <- Response.fromSocketApp(
                     socket.toSocketApp.withProtocol(SocketProtocol(subprotocols = Some(protocol.name)))
                   )
        } yield app
      }
      .toHttp
}
