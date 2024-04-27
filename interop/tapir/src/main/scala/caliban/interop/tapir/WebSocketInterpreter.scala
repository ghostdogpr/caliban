package caliban.interop.tapir

import caliban._
import caliban.interop.tapir.TapirAdapter._
import caliban.ws.Protocol
import sttp.capabilities.zio.ZioStreams
import sttp.model.{ headers => _ }
import sttp.tapir.Codec.JsonCodec
import sttp.tapir._
import sttp.tapir.model.{ ServerRequest, UnsupportedWebSocketFrameException }
import sttp.tapir.server.ServerEndpoint
import sttp.ws.WebSocketFrame
import zio._

sealed trait WebSocketInterpreter[-R, E] { self =>
  protected val endpoint: PublicEndpoint[(ServerRequest, String), TapirResponse, (String, CalibanPipe), ZioWebSockets]

  def makeProtocol(
    serverRequest: ServerRequest,
    protocol: String
  ): URIO[R, Either[TapirResponse, (String, CalibanPipe)]]

  def serverEndpoint[R1 <: R]: ServerEndpoint[ZioWebSockets, RIO[R1, *]] =
    endpoint.serverLogic[RIO[R1, *]] { case (serverRequest, protocol) =>
      makeProtocol(serverRequest, protocol)
    }

  def intercept[R1](interceptor: Interceptor[R1, R]): WebSocketInterpreter[R1, E] =
    WebSocketInterpreter.Intercepted(self, interceptor)

  def prependPath(path: List[String]): WebSocketInterpreter[R, E] =
    WebSocketInterpreter.Prepended(self, path)

  def configure[R1](configurator: Configurator[R1]): WebSocketInterpreter[R & R1, E] =
    intercept[R & R1](ZLayer.scopedEnvironment[R & R1 & ServerRequest](configurator *> ZIO.environment[R]))
}

object WebSocketInterpreter {
  private case class Base[R, E](
    interpreter: GraphQLInterpreter[R, E],
    keepAliveTime: Option[Duration],
    webSocketHooks: ws.WebSocketHooks[R, E]
  )(implicit
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ) extends WebSocketInterpreter[R, E] {
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

  private case class Intercepted[R1, R, E](
    interpreter: WebSocketInterpreter[R, E],
    layer: ZLayer[R1 & ServerRequest, TapirResponse, R]
  ) extends WebSocketInterpreter[R1, E] {
    override def intercept[R2](interceptor: Interceptor[R2, R1]): WebSocketInterpreter[R2, E] =
      Intercepted[R2, R, E](interpreter, ZLayer.makeSome[R2 & ServerRequest, R](interceptor, layer))

    val endpoint: PublicEndpoint[(ServerRequest, String), TapirResponse, (String, CalibanPipe), ZioWebSockets] =
      interpreter.endpoint

    def makeProtocol(
      serverRequest: ServerRequest,
      protocol: String
    ): URIO[R1, Either[TapirResponse, (String, CalibanPipe)]] =
      interpreter
        .makeProtocol(serverRequest, protocol)
        .provideSome[R1](ZLayer.succeed(serverRequest), layer)
        .catchAll(ZIO.left(_))
  }

  private case class Prepended[R, E](
    interpreter: WebSocketInterpreter[R, E],
    path: List[String]
  ) extends WebSocketInterpreter[R, E] {
    val endpoint: PublicEndpoint[(ServerRequest, String), TapirResponse, (String, CalibanPipe), ZioWebSockets] = {
      if (path.nonEmpty) {
        val p: List[EndpointInput[Unit]]   = path.map(stringToPath)
        val fixedPath: EndpointInput[Unit] = p.tail.foldLeft(p.head)(_ / _)

        interpreter.endpoint.prependIn(fixedPath)
      } else {
        interpreter.endpoint
      }
    }

    def makeProtocol(
      serverRequest: ServerRequest,
      protocol: String
    ): URIO[R, Either[TapirResponse, (String, CalibanPipe)]] =
      interpreter.makeProtocol(serverRequest, protocol)
  }

  def apply[R, E](
    interpreter: GraphQLInterpreter[R, E],
    keepAliveTime: Option[Duration] = None,
    webSocketHooks: ws.WebSocketHooks[R, E] = ws.WebSocketHooks.empty[R, E]
  )(implicit
    inputCodec: JsonCodec[GraphQLWSInput],
    outputCodec: JsonCodec[GraphQLWSOutput]
  ): WebSocketInterpreter[R, E] =
    Base(interpreter, keepAliveTime, webSocketHooks)

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
