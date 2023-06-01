package caliban

import caliban.interop.tapir.ws.Protocol
import caliban.interop.tapir.{ HttpInterpreter, WebSocketInterpreter }
import sttp.capabilities.zio.ZioStreams
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.DecodeResult
import sttp.tapir.server.ziohttp.{ ZioHttpInterpreter, ZioHttpServerOptions, ZioHttpServerRequest }
import zio._
import zio.http.ChannelEvent._
import zio.http._
import zio.stream._

object ZHttpAdapter {

  lazy val defaultWebSocketConfig: WebSocketConfig = {
    val subProtocols = List(Protocol.Legacy.name, Protocol.GraphQLWS.name).mkString(",")
    WebSocketConfig.default.withSubProtocol(Some(subProtocols))
  }

  def makeHttpService[R, E](interpreter: HttpInterpreter[R, E])(implicit
    serverOptions: ZioHttpServerOptions[R] = ZioHttpServerOptions.default[R]
  ): App[R] =
    ZioHttpInterpreter(serverOptions)
      .toHttp(interpreter.serverEndpoints[R, ZioStreams](ZioStreams))
      .withDefaultErrorResponse

  def makeWebSocketService[R, E](
    interpreter: WebSocketInterpreter[R, E]
  )(implicit inputCodec: JsonCodec[GraphQLWSInput], outputCodec: JsonCodec[GraphQLWSOutput]): App[R] =
    Handler
      .fromFunctionZIO[Request] { req =>
        val protocol = req.headers.get(Header.SecWebSocketProtocol) match {
          case Some(value) => Protocol.fromName(value.renderedValue)
          case None        => Protocol.Legacy
        }

        for {
          queue <- Queue.unbounded[GraphQLWSInput]
          pipe  <- interpreter.makeProtocol(ZioHttpServerRequest(req), protocol.name).flatMap {
                     case Left(response)   =>
                       ZIO.fail(
                         Response(
                           Status.fromInt(response.code.code).getOrElse(Status.InternalServerError),
                           Headers(response.headers.map(header => Header.Custom(header.name, header.value))),
                           Body.fromString(response.body)
                         )
                       )
                     case Right((_, pipe)) => ZIO.succeed(pipe)
                   }
          in     = ZStream.fromQueueWithShutdown(queue)
          out    = pipe(in).map {
                     case Right(output) => WebSocketFrame.Text(outputCodec.encode(output))
                     case Left(close)   => WebSocketFrame.Close(close.code, Some(close.reason))
                   }
          socket = Handler.webSocket { ch =>
                     ch.receiveAll {
                       case ChannelEvent.UserEventTriggered(UserEvent.HandshakeComplete) =>
                         out
                           .interruptWhen(ch.awaitShutdown)
                           .runForeach(msg => ch.send(ChannelEvent.Read(msg)))
                           .forkDaemon
                       case ChannelEvent.Read(WebSocketFrame.Text(text))                 =>
                         ZIO.fromEither {
                           inputCodec.decode(text) match {
                             case DecodeResult.Value(v)    => Right(v)
                             case DecodeResult.Error(_, e) => Left(e)
                             case f: DecodeResult.Failure  =>
                               Left(new Throwable(s"failed to decode input: ${f.toString}"))
                           }
                         }.flatMap(queue.offer)
                       case _                                                            =>
                         ZIO.unit
                     }
                   }
          app   <- Response.fromSocketApp(socket)
        } yield app
      }
      .toHttp
}
