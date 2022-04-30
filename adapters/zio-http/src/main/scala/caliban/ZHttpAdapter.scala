package caliban

import caliban.execution.QueryExecution
import caliban.interop.tapir.ws.Protocol
import caliban.interop.tapir.{ ws, RequestInterceptor, TapirAdapter, WebSocketHooks }
import io.circe.parser._
import io.circe.syntax._
import sttp.tapir.json.circe._
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions }
import zhttp.http._
import zhttp.socket._
import zio._
import zio.clock.Clock
import zio.duration._
import zio.stream._

object ZHttpAdapter {
  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    requestInterceptor: RequestInterceptor[R] = RequestInterceptor.empty
  )(implicit serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]): HttpApp[R, Throwable] = {
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
  ): HttpApp[R with Clock, E] =
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
                   case Right(output) => WebSocketFrame.Text(output.asJson.dropNullValues.noSpaces)
                   case Left(close)   => WebSocketFrame.Close(close.code, Some(close.reason))
                 }
        socket = Socket
                   .collect[WebSocketFrame] { case WebSocketFrame.Text(text) =>
                     ZStream
                       .fromEffect(ZIO.fromEither(decode[GraphQLWSInput](text)))
                       .mapM(queue.offer) *> ZStream.empty
                   }
                   .merge(Socket.fromStream[Any, Throwable, WebSocketFrame](out))
        app   <- Response.fromSocketApp[R with Clock](
                   SocketApp(socket).withProtocol(SocketProtocol.subProtocol(protocol.name))
                 )
      } yield app
    }
}
