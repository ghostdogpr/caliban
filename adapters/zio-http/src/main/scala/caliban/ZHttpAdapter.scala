package caliban

import caliban.execution.QueryExecution
import caliban.interop.tapir.ws.Protocol
import caliban.interop.tapir.{ RequestInterceptor, TapirAdapter, WebSocketHooks }
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.DecodeResult
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }
import zhttp.http._
import zhttp.service.ChannelEvent
import zhttp.service.ChannelEvent.UserEvent.HandshakeComplete
import zhttp.service.ChannelEvent.{ ChannelRead, UserEventTriggered }
import zhttp.socket._
import zio._
import zio.stream._

object ZHttpAdapter {

  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    responseCodec: JsonCodec[GraphQLResponse[E]],
    serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]
  ): HttpApp[R, Throwable] = {
    val endpoints = TapirAdapter.makeHttpService[R, E](
      interpreter,
      skipValidation,
      enableIntrospection,
      queryExecution,
      requestInterceptor
    )
    ZioHttpInterpreter(serverOptions).toHttp(endpoints)
  }

  def makeWebSocketService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    keepAliveTime: Option[Duration] = None,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty
  )(implicit inputCodec: JsonCodec[GraphQLWSInput], outputCodec: JsonCodec[GraphQLWSOutput]): HttpApp[R, E] =
    Http.fromFunctionZIO[Request] { req =>
      val protocol = req.headers.header("Sec-WebSocket-Protocol") match {
        case Some((_, value)) => Protocol.fromName(value.toString)
        case None             => Protocol.Legacy
      }

      for {
        queue <- Queue.unbounded[GraphQLWSInput]
        pipe  <- protocol
                   .make(
                     interpreter,
                     skipValidation,
                     enableIntrospection,
                     keepAliveTime,
                     queryExecution,
                     webSocketHooks
                   )
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
                           case _                        => Left(new Throwable("failed to decode input"))
                         }
                       }.flatMap(queue.offer)
                   }
        app   <- Response.fromSocketApp(
                   socket.toSocketApp.withProtocol(SocketProtocol.subProtocol(protocol.name))
                 )
      } yield app
    }
}
