package caliban.gateway.internal.execution

import caliban._
import caliban.gateway.{ RemoteGraphQLConfig, RemoteSubscriptionConfig, SubscriptionTermination }
import caliban.gateway.internal.SubscriptionBuffer
import caliban.ResponseValue.{ ListValue, ObjectValue }
import com.github.plokhotnyuk.jsoniter_scala.core._
import sttp.capabilities.zio.ZioStreams
import sttp.client4._
import sttp.client4.httpclient.zio.SttpClient
import sttp.client4.ws.async._
import sttp.model.{ Header, Method, Uri }
import sttp.ws.{ WebSocket, WebSocketFrame }
import zio._
import zio.stream.ZStream

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8

private[gateway] object RemoteSubscription {
  type Response = GraphQLResponse[CalibanError]

  def open(
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteSubscriptionConfig,
    headers: List[Header],
    request: GraphQLRequest,
    body: Array[Byte],
    maxBytes: Int,
    decode: Array[Byte] => Either[SubgraphExecutor.Failure, Response],
    validate: Array[Byte] => Either[SubgraphExecutor.Failure, Unit],
    disclosure: RemoteGraphQLConfig.ErrorDisclosure
  )(implicit trace: Trace): ZIO[Scope, Throwable, ZStream[Any, Throwable, Response]] =
    for {
      queue    <- SubscriptionBuffer.make[Response](config.bufferSize)
      finished <- Promise.make[Throwable, Unit]
      ready    <- Promise.make[Throwable, Unit]
      emit      = (response: Response) =>
                    queue.offer(response).flatMap(ok => ZIO.fail(SubscriptionTermination.Overflow).unless(ok).unit)
      target    = config.endpoint.getOrElse(endpoint)
      run       = config.transport match {
                    case RemoteSubscriptionConfig.WebSocket =>
                      val wsTarget = target.scheme match {
                        case Some("https") => target.scheme("wss")
                        case Some("http")  => target.scheme("ws")
                        case _             => target
                      }
                      websocket(
                        wsTarget,
                        backend,
                        config,
                        headers,
                        request,
                        maxBytes,
                        decode,
                        validate,
                        disclosure,
                        ready,
                        emit
                      )
                    case RemoteSubscriptionConfig.Sse(get)  =>
                      val url  = if (get) {
                        target
                          .addParam("query", request.query.getOrElse(""))
                          .addParams(request.operationName.map("operationName" -> _).toList: _*)
                          .addParams(
                            request.variables
                              .map(values => "variables" -> writeToString[InputValue](InputValue.ObjectValue(values)))
                              .toList: _*
                          )
                      } else target
                      val base = basicRequest
                        .method(if (get) Method.GET else Method.POST, url)
                        .headers(headers: _*)
                        .header("Accept", "text/event-stream")
                        .followRedirects(false)
                        .readTimeout(scala.concurrent.duration.Duration.Inf)
                      val http = if (get) base else base.body(body).contentType("application/json")
                      http
                        .response(asStreamAlwaysWithMetadata(ZioStreams) { (bytes, metadata) =>
                          if (
                            !metadata.code.isSuccess || !metadata.contentType
                              .exists(_.takeWhile(_ != ';').trim.equalsIgnoreCase("text/event-stream"))
                          )
                            ZIO.fail(SubscriptionTermination.Source)
                          else {
                            val parser = new SseDecoder(maxBytes)
                            ready.succeed(()) *> bytes
                              .mapZIO(byte => ZIO.fromEither(parser.feed(byte)))
                              .collectSome
                              .takeUntil(_._1 == "complete")
                              .runForeach {
                                case ("complete", _) => ZIO.unit
                                case ("next", value) =>
                                  ZIO
                                    .fromEither(decode(value.getBytes(UTF_8)))
                                    .mapError(_ => SubscriptionTermination.Source)
                                    .flatMap(emit)
                                case _               => ZIO.fail(SubscriptionTermination.Source)
                              } *> ZIO.fail(SubscriptionTermination.Source).unless(parser.completed).unit
                          }
                        })
                        .send(backend)
                        .unit
                  }
      _        <- run.mapError {
                    case error: CalibanError.ExecutionError => error
                    case _                                  => SubscriptionTermination.Source
                  }.exit.flatMap {
                    case Exit.Success(_)     => queue.end *> finished.succeed(()).unit
                    case Exit.Failure(cause) => ready.failCause(cause) *> finished.failCause(cause).unit
                  }.forkScoped
      _        <- ready.await
    } yield
    // queue.end drains buffered events on success; only a source failure interrupts the consumer immediately.
    queue.stream.interruptWhen(finished.await.flatMap(_ => ZIO.never))

  private def websocket(
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteSubscriptionConfig,
    headers: List[Header],
    request: GraphQLRequest,
    maxBytes: Int,
    decode: Array[Byte] => Either[SubgraphExecutor.Failure, Response],
    validate: Array[Byte] => Either[SubgraphExecutor.Failure, Unit],
    disclosure: RemoteGraphQLConfig.ErrorDisclosure,
    ready: Promise[Throwable, Unit],
    emit: Response => Task[Unit]
  )(implicit trace: Trace): Task[Unit] = ZIO.scoped {
    for {
      upgraded <- ZIO.acquireRelease(
                    basicRequest
                      .get(endpoint)
                      .headers(headers: _*)
                      .header("Sec-WebSocket-Protocol", "graphql-transport-ws")
                      .followRedirects(false)
                      .response(asWebSocketAlwaysUnsafe[Task])
                      .send(backend)
                  )(response => response.body.close().ignore.timeout(config.writeTimeout).unit)
      socket    = upgraded.body
      _        <- ZIO
                    .fail(SubscriptionTermination.Source)
                    .unless(upgraded.header("Sec-WebSocket-Protocol").contains("graphql-transport-ws"))
      send      = (message: GraphQLWSInput) =>
                    socket.sendText(writeToString(message)).timeoutFail(SubscriptionTermination.Source)(config.writeTimeout)
      _        <- send(GraphQLWSInput("connection_init", None, config.connectionInit))
      pong     <- Ref.make(Option.empty[Promise[Nothing, Unit]])
      read      = readMessage(socket, maxBytes, validate, config.writeTimeout)
      control   = (message: GraphQLWSOutput) =>
                    message.`type` match {
                      case "ping" =>
                        socket
                          .sendText(writeToString(GraphQLWSOutput("pong", None, message.payload)))
                          .timeoutFail(SubscriptionTermination.Source)(config.writeTimeout)
                      case "pong" => pong.get.flatMap(ZIO.foreachDiscard(_)(_.succeed(())))
                      case _      => ZIO.fail(SubscriptionTermination.Source)
                    }
      _        <- read.flatMap { first =>
                    def ack(message: GraphQLWSOutput): Task[Unit] =
                      if (message.`type` == "connection_ack") ZIO.unit else control(message) *> read.flatMap(ack)
                    ack(first)
                  }.timeoutFail(SubscriptionTermination.SetupTimeout)(config.acknowledgementTimeout)
      payload  <- ZIO.attempt(readFromArray[InputValue](writeToArray(request.copy(extensions = None))))
      _        <- send(GraphQLWSInput("subscribe", Some("1"), Some(payload)))
      _        <- ZIO.addFinalizer(send(GraphQLWSInput("complete", Some("1"), None)).ignore)
      _        <- ready.succeed(())
      heartbeat = (Clock.sleep(config.keepAliveInterval) *> Promise.make[Nothing, Unit].flatMap { received =>
                    pong.set(Some(received)) *> send(GraphQLWSInput("ping", None, None)) *>
                      received.await.timeoutFail(SubscriptionTermination.Source)(config.pongTimeout) *> pong.set(None)
                  }).forever
      receive   = read.flatMap { message =>
                    if (message.`type` == "ping" || message.`type` == "pong") control(message).as(true)
                    else if (!message.id.contains("1")) ZIO.fail(SubscriptionTermination.Source)
                    else
                      message.`type` match {
                        case "next"     =>
                          message.payload match {
                            case Some(value) =>
                              ZIO
                                .fromEither(decode(writeToArray(value)))
                                .mapError(_ => SubscriptionTermination.Source)
                                .flatMap(emit)
                                .as(true)
                            case None        => ZIO.fail(SubscriptionTermination.Source)
                          }
                        case "complete" => ZIO.succeed(false)
                        case "error"    =>
                          message.payload match {
                            case Some(ListValue(values)) if values.nonEmpty =>
                              val errors = values.flatMap(CalibanError.fromResponseValue)
                              if (errors.size != values.size) ZIO.fail(SubscriptionTermination.Source)
                              else ZIO.fail(RemoteError.disclose(errors.head, disclosure))
                            case _                                          => ZIO.fail(SubscriptionTermination.Source)
                          }
                        case _          => ZIO.fail(SubscriptionTermination.Source)
                      }
                  }.repeatWhile(identity).unit
      _        <- receive.raceFirst(heartbeat)
    } yield ()
  }

  private def readMessage(
    socket: WebSocket[Task],
    maxBytes: Int,
    validate: Array[Byte] => Either[SubgraphExecutor.Failure, Unit],
    writeTimeout: Duration
  )(implicit trace: Trace): Task[GraphQLWSOutput] =
    ZIO.suspendSucceed {
      val parts                         = new StringBuilder
      def loop(size: Int): Task[String] = socket.receive().flatMap {
        case WebSocketFrame.Text(text, last, _) =>
          val next = size + text.getBytes(UTF_8).length
          if (next > maxBytes) ZIO.fail(SubscriptionTermination.TooLarge)
          else {
            parts.append(text)
            if (last) ZIO.succeed(parts.toString) else loop(next)
          }
        case WebSocketFrame.Ping(payload)       =>
          socket.send(WebSocketFrame.Pong(payload)).timeoutFail(SubscriptionTermination.Source)(writeTimeout) *> loop(
            size
          )
        case _: WebSocketFrame.Pong             => loop(size)
        case _                                  => ZIO.fail(SubscriptionTermination.Source)
      }
      loop(0)
    }.flatMap { text =>
      val bytes = text.getBytes(UTF_8)
      ZIO.fromEither(validate(bytes)).mapError(_ => SubscriptionTermination.Source) *>
        ZIO.attempt(readFromArray[GraphQLWSOutput](bytes)).mapError(_ => SubscriptionTermination.Source)
    }

  /**
   * Bounds retained event fields and the current line, including unterminated comments.
   * Completed comments and ignored fields do not consume the next line's budget.
   */
  private final class SseDecoder(maxBytes: Int) {
    private val line                                                  = new ByteArrayOutputStream
    private val data                                                  = new StringBuilder
    private var event                                                 = ""
    private var size                                                  = 0L
    private var afterCR                                               = false
    private var firstLine                                             = true
    var completed                                                     = false
    def feed(byte: Byte): Either[Throwable, Option[(String, String)]] = {
      val skip = afterCR && byte == 10
      afterCR = byte == 13
      if (skip) Right(None)
      else if (byte != 10 && byte != 13) {
        if (size + line.size().toLong >= maxBytes) Left(SubscriptionTermination.TooLarge)
        else { line.write(byte.toInt); Right(None) }
      } else {
        val decoded = new String(line.toByteArray, UTF_8)
        val text    = if (firstLine) decoded.stripPrefix("\uFEFF") else decoded
        firstLine = false
        line.reset()
        if (text.isEmpty) {
          val value = if (event == "complete" || data.nonEmpty) Some(event -> data.toString.stripSuffix("\n")) else None
          completed = event == "complete"
          event = ""; data.clear(); size = 0
          Right(value)
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
            else if (key == "data") data.append(value).append('\n')
            Right(None)
          }
        }
      }
    }
  }
}
