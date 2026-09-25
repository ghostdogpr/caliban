package caliban.gateway.internal.execution

import caliban._
import caliban.gateway.{ traverseOption, RemoteSubscriptionConfig }
import caliban.gateway.internal.{ GatewayHttpClient, RemoteTransport, SubscriptionBuffer, SubscriptionTermination }
import caliban.ResponseValue.ListValue
import caliban.ws.Protocol.GraphQLWS
import caliban.ws.Protocol.GraphQLWS.Ops
import com.github.plokhotnyuk.jsoniter_scala.core._
import io.netty.handler.codec.http.websocketx.{ CorruptedWebSocketFrameException, WebSocketCloseStatus }
import zio._
import zio.http._
import zio.stream.ZStream

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8

private[gateway] final class RemoteSubscription(
  endpoint: URL,
  http: GatewayHttpClient,
  config: RemoteSubscriptionConfig,
  maxBytes: Int,
  decode: Array[Byte] => Either[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]],
  decodeValue: ResponseValue => Either[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]],
  validate: Array[Byte] => Either[SubgraphExecutor.Failure, Unit],
  remoteErrorMessages: Boolean
) {
  import RemoteSubscription._

  val target: URL = config.endpoint.getOrElse(endpoint)

  def open(headers: List[Header], request: GraphQLRequest, body: Array[Byte])(implicit
    trace: Trace
  ): ZIO[Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]] =
    for {
      queue    <- SubscriptionBuffer.make[GraphQLResponse[CalibanError]](BufferSize)
      finished <- Promise.make[Throwable, Nothing]
      ready    <- Promise.make[Throwable, Unit]
      emit      = (response: GraphQLResponse[CalibanError]) =>
                    queue
                      .offer(response)
                      .flatMap(offered => ZIO.fail(SubscriptionTermination.Overflow).unless(offered).unit)
      run       = config.transport match {
                    case RemoteSubscriptionConfig.WebSocket =>
                      val wsTarget = target.scheme match {
                        case Some(Scheme.HTTPS) => target.scheme(Scheme.WSS)
                        case Some(Scheme.HTTP)  => target.scheme(Scheme.WS)
                        case _                  => target
                      }
                      runWebSocket(wsTarget, headers, body, ready, emit)
                    case RemoteSubscriptionConfig.Sse(get)  =>
                      runSse(target, headers, request, body, get, ready, emit)
                  }
      _        <- run
                    .mapError(SubscriptionTermination.fromFailure)
                    .exit
                    .flatMap {
                      case Exit.Success(_)     => queue.end
                      case Exit.Failure(cause) => ready.failCause(cause) *> finished.failCause(cause).unit
                    }
                    .forkScoped
      _        <- ready.await.timeoutFail(SubscriptionTermination.SetupTimeout)(config.connectionTimeout)
    } yield
    // queue.end drains buffered events on success; only a source failure interrupts the consumer immediately.
    queue.stream.interruptWhen(finished.await)

  private def runSse(
    target: URL,
    headers: List[Header],
    request: GraphQLRequest,
    body: Array[Byte],
    get: Boolean,
    ready: Promise[Throwable, Unit],
    emit: GraphQLResponse[CalibanError] => Task[Unit]
  )(implicit trace: Trace): Task[Unit] = {
    val httpRequest =
      if (get) {
        val params =
          request.operationName.map("operationName" -> _).toList :::
            request.variables
              .map(values => "variables" -> writeToString[InputValue](InputValue.ObjectValue(values)))
              .toList
        Request.get(target.addQueryParams(QueryParams("query" -> request.query.getOrElse(""), params: _*)))
      } else
        Request.post(target, Body.fromArray(body)).addHeader(GatewayHttpClient.jsonContentType)
    ZIO.scoped {
      http
        .stream(httpRequest.addHeader(Header.Accept(MediaType.text.`event-stream`)), headers)
        .flatMap { response =>
          val mediaType = RemoteTransport.mediaType(response.rawHeader(Header.ContentType))
          if (!response.status.isSuccess || !mediaType.contains("text/event-stream"))
            ZIO.fail(SubscriptionTermination.Source)
          else {
            val parser = new SseDecoder
            ready.succeed(()) *> response.body.asStream
              .mapChunks(parser.feedAll)
              .mapZIO(ZIO.fromEither(_))
              .takeUntil(_.name == SseEvent.Complete)
              .runForeach {
                case SseEvent(SseEvent.Complete, _) => ZIO.unit
                case SseEvent(SseEvent.Next, value) =>
                  ZIO
                    .fromEither(decode(value.getBytes(UTF_8)))
                    .mapError(_ => SubscriptionTermination.Source)
                    .flatMap(emit)
                case _                              => ZIO.fail(SubscriptionTermination.Source)
              } *> ZIO.fail(SubscriptionTermination.Source).unless(parser.isComplete).unit
          }
        }
    }
  }

  private def runWebSocket(
    endpoint: URL,
    headers: List[Header],
    body: Array[Byte],
    ready: Promise[Throwable, Unit],
    emit: GraphQLResponse[CalibanError] => Task[Unit]
  )(implicit trace: Trace): Task[Unit] = ZIO.scoped {
    for {
      connected <- Promise.make[Nothing, WebSocketChannel]
      app        = Handler
                     .webSocket(channel => connected.succeed(channel) *> channel.awaitShutdown)
                     .withConfig(
                       WebSocketConfig.default
                         .subProtocol(Some(GraphQLWS.name))
                         .decoderConfig(SocketDecoder.default.maxFramePayloadLength(maxBytes))
                     )
      _         <- http.socket(endpoint, headers, app)
      socket    <- connected.await
      _         <- ZIO.addFinalizer(socket.shutdown)
      _         <- awaitHandshake(socket)
      sendText   = (text: String) =>
                     socket
                       .send(ChannelEvent.Read(WebSocketFrame.Text(text)))
                       .timeoutFail(SubscriptionTermination.Source)(config.connectionTimeout)
      send       = (message: GraphQLWSInput) => sendText(writeToString(message))
      _         <- send(GraphQLWSInput(Ops.ConnectionInit, None, config.connectionInit))
      pong      <- Ref.make(Option.empty[Promise[Nothing, Unit]])
      read       = readMessage(socket)
      control    = (message: GraphQLWSOutput) =>
                     message.`type` match {
                       case Ops.Ping =>
                         sendText(writeToString(GraphQLWSOutput(Ops.Pong, None, message.payload)))
                       case Ops.Pong => pong.get.flatMap(ZIO.foreachDiscard(_)(_.succeed(())))
                       case _        => ZIO.fail(SubscriptionTermination.Source)
                     }
      _         <- read.flatMap { first =>
                     def ack(message: GraphQLWSOutput): Task[Unit] =
                       if (message.`type` == Ops.ConnectionAck) ZIO.unit
                       else control(message) *> read.flatMap(ack)
                     ack(first)
                   }
      payload   <- ZIO.attempt(readFromArray[InputValue](body))
      _         <- send(GraphQLWSInput(Ops.Subscribe, Some(SubscriptionId), Some(payload)))
      _         <- ZIO.addFinalizer(send(GraphQLWSInput(Ops.Complete, Some(SubscriptionId), None)).ignore)
      _         <- ready.succeed(())
      heartbeat  = (Clock.sleep(config.keepAliveInterval) *> Promise.make[Nothing, Unit].flatMap { received =>
                     pong.set(Some(received)) *> send(GraphQLWSInput(Ops.Ping, None, None)) *>
                       received.await.timeoutFail(SubscriptionTermination.Source)(config.connectionTimeout) *> pong
                         .set(None)
                   }).forever
      receive    = read.flatMap { message =>
                     if (message.`type` == Ops.Ping || message.`type` == Ops.Pong)
                       control(message).as(true)
                     else if (!message.id.contains(SubscriptionId)) ZIO.fail(SubscriptionTermination.Source)
                     else
                       message.`type` match {
                         case Ops.Next     =>
                           message.payload match {
                             case Some(value) =>
                               ZIO
                                 .fromEither(decodeValue(value))
                                 .mapError(_ => SubscriptionTermination.Source)
                                 .flatMap(emit)
                                 .as(true)
                             case None        => ZIO.fail(SubscriptionTermination.Source)
                           }
                         case Ops.Complete => ZIO.succeed(false)
                         case Ops.Error    =>
                           message.payload match {
                             case Some(ListValue(values)) if values.nonEmpty =>
                               traverseOption(values)(CalibanError.fromResponseValue) match {
                                 case Some(errors) => ZIO.fail(RemoteError.sanitize(errors.head, remoteErrorMessages))
                                 case None         => ZIO.fail(SubscriptionTermination.Source)
                               }
                             case _                                          => ZIO.fail(SubscriptionTermination.Source)
                           }
                         case _            => ZIO.fail(SubscriptionTermination.Source)
                       }
                   }.repeatWhile(identity).unit
      _         <- receive.raceFirst(heartbeat)
    } yield ()
  }

  private def awaitHandshake(socket: WebSocketChannel)(implicit trace: Trace): Task[Unit] =
    socket.receive.flatMap {
      case ChannelEvent.UserEventTriggered(ChannelEvent.UserEvent.HandshakeComplete) => ZIO.unit
      case ChannelEvent.Registered                                                   => awaitHandshake(socket)
      case _                                                                         => ZIO.fail(SubscriptionTermination.Source)
    }

  private def readMessage(socket: WebSocketChannel)(implicit trace: Trace): Task[GraphQLWSOutput] =
    ZIO.suspendSucceed {
      val message = new ByteArrayOutputStream

      def append(bytes: Array[Byte], last: Boolean): Task[Array[Byte]] =
        if (message.size() + bytes.length > maxBytes) ZIO.fail(SubscriptionTermination.TooLarge)
        else {
          message.write(bytes, 0, bytes.length)
          if (last) ZIO.succeed(message.toByteArray) else loop
        }

      def loop: Task[Array[Byte]] = socket.receive.flatMap {
        case ChannelEvent.Read(frame: WebSocketFrame.Text)                => append(frame.text.getBytes(UTF_8), frame.isFinal)
        case ChannelEvent.Read(frame: WebSocketFrame.Continuation)        => append(frame.buffer.toArray, frame.isFinal)
        case ChannelEvent.Read(WebSocketFrame.Ping | WebSocketFrame.Pong) => loop
        case ChannelEvent.ExceptionCaught(cause: CorruptedWebSocketFrameException)
            if cause.closeStatus == WebSocketCloseStatus.MESSAGE_TOO_BIG =>
          ZIO.fail(SubscriptionTermination.TooLarge)
        case _                                                            => ZIO.fail(SubscriptionTermination.Source)
      }

      loop
    }.flatMap { bytes =>
      ZIO.fromEither(validate(bytes)).mapError(_ => SubscriptionTermination.Source) *>
        ZIO.attempt(readFromArray[GraphQLWSOutput](bytes)).mapError(_ => SubscriptionTermination.Source)
    }

  /**
   * Bounds retained event fields and the current line, including unterminated comments.
   * Completed comments and ignored fields do not consume the next line's budget.
   */
  private final class SseDecoder {
    private val line                = new ByteArrayOutputStream
    private val data                = new StringBuilder
    private var event               = ""
    private var size                = 0L
    private var afterCarriageReturn = false
    private var firstLine           = true
    private var complete            = false

    def isComplete: Boolean = complete

    def feedAll(bytes: Chunk[Byte]): Chunk[Either[Throwable, SseEvent]] = {
      val events = ChunkBuilder.make[Either[Throwable, SseEvent]]()
      var index  = 0
      var failed = false
      while (index < bytes.length && !failed && !complete) {
        feed(bytes(index)) match {
          case Right(Some(event)) => events += Right(event)
          case Right(None)        => ()
          case Left(error)        =>
            events += Left(error)
            failed = true
        }
        index += 1
      }
      events.result()
    }

    private def feed(byte: Byte): Either[Throwable, Option[SseEvent]] = {
      val skip = afterCarriageReturn && byte == LineFeed
      afterCarriageReturn = byte == CarriageReturn
      if (skip) NoEvent
      else if (byte != LineFeed && byte != CarriageReturn) {
        if (size + line.size().toLong >= maxBytes) Left(SubscriptionTermination.TooLarge)
        else {
          line.write(byte.toInt)
          NoEvent
        }
      } else {
        val decoded = new String(line.toByteArray, UTF_8)
        val text    = if (firstLine) decoded.stripPrefix(ByteOrderMark) else decoded
        firstLine = false
        line.reset()
        if (text.isEmpty) {
          val result =
            if (event == SseEvent.Complete || data.nonEmpty)
              Right(Some(SseEvent(event, data.toString.stripSuffix("\n"))))
            else NoEvent
          complete = event == SseEvent.Complete
          event = ""
          data.clear()
          size = 0
          result
        } else {
          val colon    = text.indexOf(':')
          val key      = if (colon < 0) text else text.substring(0, colon)
          val value    = if (colon < 0) "" else text.substring(colon + 1).stripPrefix(" ")
          val nextSize = size + (key match {
            case "event" => value.getBytes(UTF_8).length.toLong - event.getBytes(UTF_8).length
            case "data"  => value.getBytes(UTF_8).length.toLong + 1L
            case _       => 0L
          })
          if (nextSize > maxBytes) Left(SubscriptionTermination.TooLarge)
          else {
            size = nextSize
            if (key == "event") event = value
            else if (key == "data") data.append(value).append(LineFeed)
            NoEvent
          }
        }
      }
    }

  }
}

private[gateway] object RemoteSubscription {
  private final case class SseEvent(name: String, data: String)

  private object SseEvent {
    final val Next     = "next"
    final val Complete = "complete"
  }

  private val NoEvent: Either[Throwable, Option[SseEvent]] = Right(None)

  private[gateway] final val BufferSize = 32
  private final val SubscriptionId      = "1"
  private final val LineFeed            = '\n'
  private final val CarriageReturn      = '\r'
  private final val ByteOrderMark       = "\uFEFF"
}
