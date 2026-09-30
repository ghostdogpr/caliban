package caliban

import caliban.Configurator.ExecutionConfiguration
import caliban.GraphQLResponseContext.{ Outcome, ServerFailure }
import caliban.HttpUtils.{ DeferMultipart, Delivery, ServerSentEvents }
import caliban.Value.NullValue
import caliban.interop.jsoniter.{ GraphQLResponseJsoniter, ValueJsoniter }
import caliban.uploads.{ FileMeta, GraphQLUploadRequest, Uploads }
import caliban.wrappers.Caching
import caliban.ws.Protocol
import com.github.plokhotnyuk.jsoniter_scala.core._
import zio._
import zio.http.ChannelEvent.UserEvent.HandshakeComplete
import zio.http._
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.stream.{ UStream, ZPipeline, ZStream }

import java.nio.charset.StandardCharsets.UTF_8
import scala.util.Try
import scala.util.control.NonFatal

final private class QuickRequestHandler[R](
  interpreter: GraphQLInterpreter[R, Any],
  wsConfig: quick.WebSocketConfig[R],
  sseConfig: quick.SseConfig,
  httpConfig: quick.HttpConfig
) {
  import QuickRequestHandler._
  import ValueJsoniter.stringListCodec

  private def copy[R1 <: R](
    interpreter: GraphQLInterpreter[R1, Any] = this.interpreter,
    wsConfig: quick.WebSocketConfig[R1] = this.wsConfig,
    sseConfig: quick.SseConfig = this.sseConfig,
    httpConfig: quick.HttpConfig = this.httpConfig
  ): QuickRequestHandler[R1] =
    new QuickRequestHandler(interpreter, wsConfig, sseConfig, httpConfig)

  def configure(config: ExecutionConfiguration)(implicit trace: Trace): QuickRequestHandler[R] =
    copy(
      interpreter = interpreter.wrapExecutionWith[R, Any](Configurator.locally(config)(_))
    )

  def configure[R1](configurator: QuickAdapter.Configurator[R1])(implicit
    trace: Trace
  ): QuickRequestHandler[R & R1] =
    copy[R & R1](
      interpreter = interpreter.wrapExecutionWith[R & R1, Any](exec => ZIO.scoped[R1 & R](configurator *> exec))
    )

  def configureWebSocket[R1](config: quick.WebSocketConfig[R1]): QuickRequestHandler[R & R1] =
    copy[R & R1](wsConfig = config)

  def configureSse(config: quick.SseConfig): QuickRequestHandler[R] =
    copy(sseConfig = config)

  def configureHttp(config: quick.HttpConfig): QuickRequestHandler[R] =
    copy(httpConfig = config)

  def handleHttpRequest(request: Request)(implicit
    trace: Trace
  ): URIO[R, Response] =
    if (request.method != Method.GET && request.method != Method.POST) ZIO.succeed(MethodNotAllowedResponse)
    else if (isUploadRequest(request)) handleUploadRequest(request)
    else
      ZIO.suspendSucceed {
        transformHttpRequest(request)
          .flatMap(executeRequest(request, _))
          .foldZIO(Exit.succeed, Exit.succeed)
      }

  def handleUploadRequest(request: Request)(implicit trace: Trace): URIO[R, Response] = ZIO.suspendSucceed {
    transformUploadRequest(request).flatMap { case (req, fileHandle) =>
      executeRequest(request, req).provideSomeLayer[R](fileHandle)
    }.foldZIO(Exit.succeed, Exit.succeed)
  }

  def handleWebSocketRequest(request: Request)(implicit trace: Trace): URIO[R, Response] =
    Response.fromSocketApp {
      val protocol = request.headers.get(Header.SecWebSocketProtocol) match {
        case Some(value) => Protocol.fromName(value.renderedValue)
        case None        => Protocol.Legacy
      }
      Handler
        .webSocket(ch => IncomingRequestHeaders.locally(headerValues(request))(webSocketChannelListener(protocol)(ch)))
        .withConfig(wsConfig.zHttpConfig.subProtocol(Some(protocol.name)))
    }

  private def transformHttpRequest(httpReq: Request)(implicit trace: Trace): IO[Response, GraphQLRequest] = {

    def decodeQueryParams(queryParams: QueryParams): Either[Response, GraphQLRequest] = {
      def extractField(key: String) =
        try
          Right(queryParams.getAll(key).headOption.map(readFromString[InputValue.ObjectValue](_, readerConfig).fields))
        catch { case NonFatal(_) => Left(badRequest(s"Invalid $key query param")) }

      for {
        vars <- extractField("variables")
        exts <- extractField("extensions")
      } yield GraphQLRequest(
        query = queryParams.getAll("query").headOption,
        operationName = queryParams.getAll("operationName").headOption,
        variables = vars,
        extensions = exts
      )
    }

    def checkNonEmptyRequest(r: GraphQLRequest): IO[Response, GraphQLRequest] =
      if (!r.isEmpty) Exit.succeed(r) else Exit.fail(EmptyRequestErrorResponse)

    def decodeBody(body: Body): ZIO[Any, Response, GraphQLRequest] = {
      val mediaType        = body.mediaType
      val isApplicationGql = mediaType.exists { mt =>
        mt.subType.equalsIgnoreCase("graphql") &&
        mt.mainType.equalsIgnoreCase("application")
      }
      // `fetch` sends text/plain when no content type is set, which the GraphQL over HTTP spec rejects.
      val isPlainText      = mediaType.exists(MediaType.text.plain.matches(_, ignoreParameters = true))

      if (isPlainText) Exit.fail(UnsupportedMediaTypeResponse)
      else
        readBody(body, httpConfig.maxRequestBodyBytes).flatMap { arr =>
          if (isApplicationGql) Exit.succeed(GraphQLRequest(Some(new String(arr, UTF_8))))
          else
            try checkNonEmptyRequest(readFromArray[GraphQLRequest](arr, readerConfig))
            catch { case NonFatal(_) => Exit.fail(BodyDecodeErrorResponse) }
        }
    }

    val queryParams = httpReq.url.queryParams

    if ((httpReq.method eq Method.GET) || queryParams.hasQueryParam("query")) {
      decodeQueryParams(queryParams).fold(Exit.fail, checkNonEmptyRequest)
    } else {
      val req = decodeBody(httpReq.body)
      if (isFtv1Request(httpReq)) req.map(_.withFederatedTracing)
      else req
    }

  }

  private def transformUploadRequest(
    request: Request
  )(implicit trace: Trace): IO[Response, (GraphQLRequest, ULayer[Uploads])] = {
    def extractField[A](
      partsMap: Map[String, FormField],
      key: String
    )(implicit jsonValueCodec: JsonValueCodec[A]): IO[Response, A] =
      Exit
        .fromOption(partsMap.get(key))
        .flatMap(_.asChunk)
        .flatMap(v => Exit.fromTry(Try(readFromArray[A](v.toArray, readerConfig))))
        .orElseFail(Response.badRequest)

    def parsePath(path: String): List[PathValue] = path.split('.').toList.map(PathValue.parse)

    for {
      body       <- boundedBody(request.body, httpConfig.maxUploadBodyBytes)
      partsMap   <- body.asMultipartForm.mapBoth(_ => Response.internalServerError, _.map)
      gqlReq     <- extractField[GraphQLRequest](partsMap, "operations")
      rawMap     <- extractField[Map[String, List[String]]](partsMap, "map")
      filePaths   = rawMap.map { case (key, value) => (key, value.map(parsePath)) }.toList
                      .flatMap(kv => kv._2.map(kv._1 -> _))
      handler     = Uploads.handler(handle =>
                      (for {
                        uuid <- Random.nextUUID
                        fp   <- ZIO.fromOption(partsMap.get(handle))
                        body <- fp.asChunk
                      } yield FileMeta(
                        uuid.toString,
                        body.toArray,
                        Some(fp.contentType.fullType),
                        fp.filename.getOrElse(""),
                        body.length
                      )).option
                    )
      uploadQuery = GraphQLUploadRequest(gqlReq, filePaths, handler)
      query       = if (isFtv1Request(request)) uploadQuery.remap.withFederatedTracing else uploadQuery.remap
    } yield query -> ZLayer(uploadQuery.fileHandle)

  }

  private def executeRequest(request: Request, req: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R, Response, Response] =
    IncomingRequestHeaders.locally(headerValues(request))(
      GraphQLResponseContext
        .capture(interpreter.executeRequest(if (request.method == Method.GET) req.asHttpGetRequest else req))(
          buildResponse(request, _, _)
        )
    )

  private def responseHeaders(headers: Headers, cacheDirective: Option[String]): Headers =
    cacheDirective match {
      case None    => headers
      case Some(h) => headers.addHeader(Header.CacheControl.name, h)
    }

  private def buildResponse(
    request: Request,
    response: GraphQLResponse[Any],
    outcome: Outcome
  )(implicit trace: Trace): Response = {
    val encoding       = responseEncoding(request)
    val cacheDirective = response.extensions.flatMap(HttpUtils.computeCacheDirective)

    val status = outcome match {
      case ServerFailure.Internal                  => Status.InternalServerError
      case ServerFailure.Unavailable               => Status.ServiceUnavailable
      case ServerFailure.TimedOut                  => Status.GatewayTimeout
      case Outcome.MutationOverGet                 => Status.MethodNotAllowed
      case Outcome.RequestError if encoding.strict => Status.BadRequest
      case _                                       => Status.Ok
    }

    val encoded = HttpUtils.delivery(response, outcome) match {
      case Delivery.Incremental(responses)               =>
        Response(
          Status.Ok,
          headers = ResponseEncoding.Multipart.contentType,
          body = Body.fromStreamChunked(encodeMultipartMixedResponse(responses))
        )
      case subscription: Delivery.Subscription           =>
        if (acceptsEventStream(request)) Response.fromServerSentEvents(encodeTextEventStream(subscription.recovered))
        else SubscriptionOverJsonResponse
      case _ if encoding == ResponseEncoding.EventStream =>
        Response
          .fromServerSentEvents(encodeTextEventStream(ZStream.succeed(response.toResponseValue)))
          .copy(status = status)
      case _ if encoding == ResponseEncoding.Multipart   =>
        Response(
          status,
          headers = responseHeaders(encoding.contentType, cacheDirective),
          body = Body.fromStreamChunked(encodeMultipartMixedResponse(ZStream.succeed(response.toResponseValue)))
        )
      case _                                             =>
        val codec   = if (!encoding.strict || status == Status.Ok) responseWithDataCodec else responseWithoutDataCodec
        val visible =
          if (cacheDirective.isEmpty) response
          else response.copy(extensions = response.extensions.flatMap(withoutCacheDirective))
        GraphQLResponseJsoniter.writeToArray(visible, httpConfig.maxResponseBodyBytes, codec) match {
          case Some(bytes) =>
            Response(status, responseHeaders(encoding.contentType, cacheDirective), Body.fromArray(bytes))
          case None        =>
            Response(Status.InternalServerError, encoding.contentType, Body.fromArray(responseLimitErrorBytes))
        }
    }
    if (outcome == Outcome.MutationOverGet) encoded.addHeaders(AllowPostHeaders) else encoded
  }

  private def encodeMultipartMixedResponse(
    responses: ZStream[Any, Throwable, ResponseValue]
  )(implicit trace: Trace): ZStream[Any, Throwable, Byte] = {
    import HttpUtils.DeferMultipart._

    responses
      .map(encodeWithinLimit)
      // later @defer payloads would patch data the client never received
      .takeUntil(_.isEmpty)
      .map(_.getOrElse(responseLimitErrorBytes))
      .intersperse(InnerBoundary.getBytes(UTF_8), InnerBoundary.getBytes(UTF_8), EndBoundary.getBytes(UTF_8))
      .mapConcatChunk(Chunk.fromArray)
  }

  private def encodeTextEventStream(
    events: UStream[ResponseValue]
  )(implicit trace: Trace): UStream[ServerSentEvent[String]] =
    ServerSentEvents
      .fromEvents(
        events,
        v => ServerSentEvent(new String(encodeWithinLimit(v).getOrElse(responseLimitErrorBytes), UTF_8), Some("next")),
        CompleteSse,
        sseConfig.heartbeatInterval.map(d => ZStream.succeed(ServerSentEvent.heartbeat).repeat(Schedule.fixed(d)))
      )

  private def encodeWithinLimit(value: ResponseValue): Option[Array[Byte]] =
    GraphQLResponseJsoniter.writeToArray(value, httpConfig.maxResponseBodyBytes, ValueJsoniter.responseValueCodec)

  private def isFtv1Request(req: Request) =
    req.headers.get(GraphQLRequest.`apollo-federation-include-trace`) match {
      case None    => false
      case Some(h) => h.equalsIgnoreCase(GraphQLRequest.ftv1)
    }

  private def readBody(body: Body, maxBytes: Int)(implicit trace: Trace): IO[Response, Array[Byte]] =
    body.knownContentLength match {
      case Some(length) if length > maxBytes.toLong => ZIO.fail(RequestEntityTooLargeResponse)
      case Some(_)                                  =>
        body.asArray.mapError(_ => BodyDecodeErrorResponse)
      case None                                     =>
        body.asStream
          .take(maxBytes.toLong + 1L)
          .runCollect
          .mapError(_ => BodyDecodeErrorResponse)
          .flatMap { bytes =>
            if (bytes.length > maxBytes) ZIO.fail(RequestEntityTooLargeResponse)
            else ZIO.succeed(bytes.toArray)
          }
    }

  private def boundedBody(body: Body, maxBytes: Int)(implicit trace: Trace): IO[Response, Body] =
    readBody(body, maxBytes).map { bytes =>
      val bounded = Body.fromArray(bytes)
      body.contentType.fold(bounded)(bounded.contentType)
    }

  private def responseEncoding(request: Request): ResponseEncoding =
    request.headers.get(Header.Accept.name) match {
      case None        => ResponseEncoding.Json
      case Some(value) =>
        val accept = value.trim
        if (accept == "*/*" || accept.equalsIgnoreCase("application/json")) ResponseEncoding.Json
        else ResponseEncoding.negotiate(value, ResponseEncoding.single).getOrElse(ResponseEncoding.Json)
    }

  private def acceptsEventStream(request: Request): Boolean =
    request.headers
      .get(Header.Accept.name)
      .exists(ResponseEncoding.negotiate(_, ResponseEncoding.subscription).isDefined)

  private def webSocketChannelListener(protocol: Protocol)(ch: WebSocketChannel)(implicit trace: Trace): RIO[R, Unit] =
    for {
      queue <- Queue.unbounded[GraphQLWSInput]
      pipe  <- protocol.make(interpreter, wsConfig.keepAliveTime, wsConfig.hooks).map(ZPipeline.fromFunction(_))
      out    = ZStream
                 .fromQueueWithShutdown(queue)
                 .via(pipe)
                 .interruptWhen(ch.awaitShutdown)
                 .map {
                   case Right(output) => WebSocketFrame.Text(writeToString(output))
                   case Left(close)   => WebSocketFrame.Close(close.code, Some(close.reason))
                 }
      _     <- ZIO.scoped(ch.receiveAll {
                 case ChannelEvent.UserEventTriggered(HandshakeComplete) =>
                   out
                     .runForeach(frame => ch.send(ChannelEvent.Read(frame)))
                     .ensuring(ch.shutdown)
                     .forkScoped
                 case ChannelEvent.Read(WebSocketFrame.Text(text))       =>
                   ZIO.suspend(queue.offer(readFromString[GraphQLWSInput](text, readerConfig)))
                 case _                                                  =>
                   ZIO.unit
               })
    } yield ()
}

object QuickRequestHandler {
  private sealed abstract class ResponseEncoding(val mediaType: MediaType, val strict: Boolean) {
    val contentType: Headers = Headers(Header.ContentType(mediaType).untyped)
  }

  private object ResponseEncoding {
    case object GraphQLJson extends ResponseEncoding(MediaType("application", "graphql-response+json"), strict = true)
    case object Json        extends ResponseEncoding(MediaType.application.json, strict = false)
    case object EventStream extends ResponseEncoding(MediaType.text.`event-stream`, strict = false)
    case object Multipart
        extends ResponseEncoding(
          MediaType.multipart.mixed.copy(parameters = DeferMultipart.DeferHeaderParams),
          strict = true
        )

    private final case class Negotiated(value: ResponseEncoding, quality: Double, specificity: Int) {
      def isPreferredOver(other: Negotiated): Boolean =
        quality > other.quality || quality == other.quality &&
          (specificity > other.specificity || specificity == 0 && other.specificity == 0 && value == Json)
    }

    val single: List[ResponseEncoding]       = List(GraphQLJson, Json, EventStream, Multipart)
    val subscription: List[ResponseEncoding] = List(EventStream)

    def negotiate(accept: String, supported: List[ResponseEncoding]): Option[ResponseEncoding] =
      Header.Accept.parse(accept).toOption.flatMap { header =>
        val ranges = header.mimeTypes.toList
        if (ranges.exists(range => range.mediaType.parameters.contains("q") && range.qFactor.isEmpty)) None
        else
          supported.flatMap { candidate =>
            bestMatch(candidate.mediaType, ranges).flatMap { range =>
              val quality = range.qFactor.getOrElse(1d)
              // Parameters pick the matching range but must not rank encodings against each other.
              if (quality > 0d && quality <= 1d) Some(Negotiated(candidate, quality, typeSpecificity(range.mediaType)))
              else None
            }
          }.reduceOption((current, candidate) => if (candidate.isPreferredOver(current)) candidate else current)
            .map(_.value)
      }

    private def bestMatch(
      candidate: MediaType,
      ranges: List[Header.Accept.MediaTypeWithQFactor]
    ): Option[Header.Accept.MediaTypeWithQFactor] =
      ranges
        .filter(range => matches(candidate, range.mediaType))
        .reduceOption((best, range) => if (specificity(range.mediaType) > specificity(best.mediaType)) range else best)

    private def matches(candidate: MediaType, range: MediaType): Boolean = {
      val parameters = range.parameters.filterNot { case (name, _) => isIgnoredParameter(range, name) }
      (range.mainType == "*" || range.mainType.equalsIgnoreCase(candidate.mainType)) &&
      (range.subType == "*" || range.subType.equalsIgnoreCase(candidate.subType)) &&
      parameters.forall {
        case (name, value) if name.equalsIgnoreCase("charset") =>
          val normalized = unquote(value)
          normalized.equalsIgnoreCase("utf-8") || normalized.equalsIgnoreCase("utf8")
        case (name, value)                                     =>
          candidate.parameters.exists { case (key, candidateValue) =>
            key.equalsIgnoreCase(name) && unquote(candidateValue).equalsIgnoreCase(unquote(value))
          }
      }
    }

    private def typeSpecificity(mediaType: MediaType): Int =
      if (mediaType.mainType == "*") 0
      else if (mediaType.subType == "*") 1
      else 2

    private def specificity(mediaType: MediaType): Int =
      typeSpecificity(mediaType) * 100 + mediaType.parameters.keysIterator.count(!isIgnoredParameter(mediaType, _))

    private def isIgnoredParameter(mediaType: MediaType, name: String): Boolean =
      name.equalsIgnoreCase("q") || name.equalsIgnoreCase("boundary") && mediaType.mainType.equalsIgnoreCase(
        "multipart"
      )

    private def unquote(value: String): String =
      if (value.length >= 2 && value.head == '"' && value.last == '"') value.substring(1, value.length - 1)
      else value
  }

  private def badRequest(msg: String) =
    errorResponse(Status.BadRequest, msg)

  private def errorResponse(status: Status, message: String) =
    Response(status, body = Body.fromString(message))

  private def isUploadRequest(request: Request): Boolean =
    request.body.mediaType.exists(MediaType.multipart.`form-data`.matches(_, ignoreParameters = true))

  private def headerValues(request: Request): List[(String, String)] =
    request.headers.iterator.map(header => header.headerName -> header.renderedValue).toList

  private val AllowPostHeaders = Headers(Header.Custom("Allow", "POST"))

  private val MethodNotAllowedResponse =
    errorResponse(Status.MethodNotAllowed, "Method not allowed.").addHeader(Header.Custom("Allow", "GET, POST"))

  private val responseLimitErrorBytes: Array[Byte] =
    writeToArray(
      GraphQLResponse(
        NullValue,
        List(CalibanError.ExecutionError("Encoded GraphQL response exceeds the configured limit."))
      ).toResponseValue
    )(ValueJsoniter.responseValueCodec)

  private val SubscriptionOverJsonResponse =
    Response(
      Status.BadRequest,
      ResponseEncoding.Json.contentType,
      Body.fromArray(writeToArray(HttpUtils.SubscriptionOverJsonError)(ValueJsoniter.responseValueCodec))
    )

  private val CompleteSse = ServerSentEvent("", Some("complete"))

  private val BodyDecodeErrorResponse =
    badRequest("Failed to decode json body")

  private val EmptyRequestErrorResponse =
    badRequest("No GraphQL query to execute")

  private val RequestEntityTooLargeResponse =
    errorResponse(Status.RequestEntityTooLarge, "GraphQL request body exceeds the configured limit.")

  private val UnsupportedMediaTypeResponse =
    errorResponse(Status.UnsupportedMediaType, "Unsupported GraphQL request media type.")

  private implicit val inputObjectCodec: JsonValueCodec[InputValue.ObjectValue] =
    new JsonValueCodec[InputValue.ObjectValue] {
      private val inputValueCodec = ValueJsoniter.inputValueCodec

      override def decodeValue(in: JsonReader, default: InputValue.ObjectValue): InputValue.ObjectValue =
        inputValueCodec.decodeValue(in, default) match {
          case o: InputValue.ObjectValue => o
          case _                         => in.decodeError("expected json object")
        }
      override def encodeValue(x: InputValue.ObjectValue, out: JsonWriter): Unit                        =
        inputValueCodec.encodeValue(x, out)
      override def nullValue: InputValue.ObjectValue                                                    =
        null
    }

  private val responseWithDataCodec: JsonValueCodec[GraphQLResponse[Any]]    =
    GraphQLResponseJsoniter.graphQLResponseCodec
  private val responseWithoutDataCodec: JsonValueCodec[GraphQLResponse[Any]] =
    GraphQLResponseJsoniter.codec(keepDataOnErrors = false)

  private def withoutCacheDirective(extensions: ResponseValue.ObjectValue): Option[ResponseValue.ObjectValue] =
    extensions.fields.filterNot(_._1 == Caching.DirectiveName) match {
      case Nil    => None
      case fields => Some(ResponseValue.ObjectValue(fields))
    }

  private val readerConfig: ReaderConfig = ReaderConfig
    .withAppendHexDumpToParseException(false)
    .withMaxBufSize(Int.MaxValue - 2)
    .withMaxCharBufSize(Int.MaxValue - 2)
}
