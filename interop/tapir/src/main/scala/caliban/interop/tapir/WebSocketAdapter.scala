package caliban.interop.tapir

import caliban._
import caliban.interop.tapir.TapirAdapter._
import caliban.interop.tapir.ws.Protocol
import sttp.capabilities.zio.ZioStreams
import sttp.model.{ headers => _ }
import sttp.tapir.Codec.JsonCodec
import sttp.tapir._
import sttp.tapir.model.{ ServerRequest, UnsupportedWebSocketFrameException }
import sttp.tapir.server.ServerEndpoint
import sttp.ws.WebSocketFrame
import zio._

trait WebSocketAdapter[-R, E] { self =>

  protected val endpoint: PublicEndpoint[(ServerRequest, String), TapirResponse, (String, CalibanPipe), ZioWebSockets]

  def serverEndpoint[R1 <: R]: ServerEndpoint[ZioWebSockets, RIO[R1, *]] =
    endpoint.serverLogic[RIO[R1, *]] { case (serverRequest, protocol) =>
      makeProtocol(serverRequest, protocol)
    }

  protected def makeProtocol(
    serverRequest: ServerRequest,
    protocol: String
  ): URIO[R, Either[TapirResponse, (String, CalibanPipe)]]

  def configure[R1](configurator: ZLayer[R1 & ServerRequest, TapirResponse, R]): WebSocketAdapter[R1, E] =
    new WebSocketAdapter[R1, E] {
      val endpoint: PublicEndpoint[(ServerRequest, String), TapirResponse, (String, CalibanPipe), ZioWebSockets] =
        self.endpoint

      def makeProtocol(
        serverRequest: ServerRequest,
        protocol: String
      ): URIO[R1, Either[TapirResponse, (String, CalibanPipe)]] =
        self
          .makeProtocol(serverRequest, protocol)
          .provideSome[R1](ZLayer.succeed(serverRequest), configurator)
          .catchAll(ZIO.left(_))
    }
}

object WebSocketAdapter {
  def apply[R, E](
    interpreter: GraphQLInterpreter[R, E],
    keepAliveTime: Option[Duration] = None,
    webSocketHooks: WebSocketHooks[R, E] = WebSocketHooks.empty[R, E]
  )(implicit inputCodec: JsonCodec[GraphQLWSInput], outputCodec: JsonCodec[GraphQLWSOutput]): WebSocketAdapter[R, E] =
    new WebSocketAdapter[R, E] {
      val endpoint: PublicEndpoint[(ServerRequest, String), TapirResponse, (String, CalibanPipe), ZioWebSockets] =
        makeWebSocketEndpoint

      def makeProtocol(
        serverRequest: ServerRequest,
        protocol: String
      ): URIO[R, Either[TapirResponse, (String, CalibanPipe)]] =
        Protocol
          .fromName(protocol)
          .make(interpreter, keepAliveTime, webSocketHooks)
          .map(res => Right((protocol, res)))
    }

  /**
   * A codec which expects only text and close frames (all other frames cause a decoding error). Close frames correspond to a `Left`,
   * while text frames are handled using the given `stringCodec` and wrapped with a `Right`
   */
  private implicit def textOrCloseWebSocketFrameEither[A, CF <: CodecFormat](implicit
    stringCodec: Codec[String, A, CF]
  ): Codec[WebSocketFrame, Either[GraphQLWSClose, A], CF] =
    Codec
      .id[WebSocketFrame, CF](stringCodec.format, Schema.string)
      .mapDecode {
        case WebSocketFrame.Text(s, _, _)       => stringCodec.decode(s).map(Right(_))
        case WebSocketFrame.Close(code, reason) => DecodeResult.Value(Left(GraphQLWSClose(code, reason)))
        case f                                  => DecodeResult.Error(f.toString, new UnsupportedWebSocketFrameException(f))
      } {
        case Left(value)  => WebSocketFrame.Close(value.code, value.reason)
        case Right(value) => WebSocketFrame.text(stringCodec.encode(value))
      }

  def makeWebSocketEndpoint(implicit
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): PublicEndpoint[(ServerRequest, String), TapirResponse, (String, CalibanPipe), ZioWebSockets] = {
    val protocolHeader: EndpointIO.Header[String] = header[String]("sec-websocket-protocol")
    endpoint
      .in(extractFromRequest(identity))
      .in(protocolHeader)
      .out(protocolHeader)
      .out(
        webSocketBody[GraphQLWSInput, CodecFormat.Json, Either[GraphQLWSClose, GraphQLWSOutput], CodecFormat.Json](
          ZioStreams
        )
      )
      .errorOut(errorBody)
  }
}
