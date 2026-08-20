package caliban

import caliban.Configurator.ExecutionConfiguration
import caliban.GraphQLResponseContext.Outcome
import caliban.HttpUtils.{ DeferMultipart, ServerSentEvents }
import caliban.ResponseValue.StreamValue
import caliban.interop.jsoniter.ValueJsoniter
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
  maxRequestBodyBytes: Int
) {
  import QuickRequestHandler._
  import ValueJsoniter.stringListCodec

  def configure(config: ExecutionConfiguration)(implicit trace: Trace): QuickRequestHandler[R] =
    new QuickRequestHandler[R](
      interpreter.wrapExecutionWith[R, Any](Configurator.locally(config)(_)),
      wsConfig,
      sseConfig,
      maxRequestBodyBytes
    )

  def configure[R1](configurator: QuickAdapter.Configurator[R1])(implicit
    trace: Trace
  ): QuickRequestHandler[R & R1] =
    new QuickRequestHandler[R & R1](
      interpreter.wrapExecutionWith[R & R1, Any](exec => ZIO.scoped[R1 & R](configurator *> exec)),
      wsConfig,
      sseConfig,
      maxRequestBodyBytes
    )

  def configureWebSocket[R1](config: quick.WebSocketConfig[R1]): QuickRequestHandler[R & R1] =
    new QuickRequestHandler[R & R1](interpreter, config, sseConfig, maxRequestBodyBytes)

  def configureSse(config: quick.SseConfig): QuickRequestHandler[R] =
    new QuickRequestHandler[R](interpreter, wsConfig, config, maxRequestBodyBytes)

  def withMaxRequestBodyBytes(value: Int): QuickRequestHandler[R] = {
    require(value > 0, "Maximum request-body size must be positive.")
    new QuickRequestHandler[R](interpreter, wsConfig, sseConfig, value)
  }

  def handleHttpRequest(request: Request)(implicit
    trace: Trace
  ): URIO[R, Response] =
    if (request.method != Method.GET && request.method != Method.POST)
      ZIO.succeed(methodNotAllowed("GET, POST"))
    else if (responseEncoding(request).isEmpty)
      ZIO.succeed(Response.status(Status.NotAcceptable))
    else if (request.body.mediaType.exists(MediaType.multipart.`form-data`.matches(_, ignoreParameters = true))) {
      handleUploadRequest(request)
    } else {
      ZIO.suspendSucceed {
        transformHttpRequest(request)
          .flatMap(req => executeRequest(request, req))
          .foldZIO(
            Exit.succeed,
            result => Exit.succeed(transformResponse(request, result.value, result.outcome))
          )
      }
    }

  def handleUploadRequest(request: Request)(implicit trace: Trace): URIO[R, Response] = ZIO.suspendSucceed {
    transformUploadRequest(request).flatMap { case (req, fileHandle) =>
      executeRequest(request, req).provideSomeLayer[R](fileHandle)
    }.foldZIO(
      Exit.succeed,
      result => Exit.succeed(transformResponse(request, result.value, result.outcome))
    )
  }

  def handleWebSocketRequest(request: Request)(implicit trace: Trace): URIO[R, Response] =
    Response.fromSocketApp {
      val protocol = request.headers.get(Header.SecWebSocketProtocol) match {
        case Some(value) => Protocol.fromName(value.renderedValue)
        case None        => Protocol.Legacy
      }
      Handler
        .webSocket(webSocketChannelListener(protocol))
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

    def decodeBody(body: Body) = {

      def decodeApplicationGql() =
        readBody(body).map(bytes => GraphQLRequest(Some(new String(bytes, UTF_8))))

      def decodeJson(): ZIO[Any, Response, GraphQLRequest] =
        readBody(body).foldZIO(
          Exit.fail,
          arr =>
            try checkNonEmptyRequest(readFromArray[GraphQLRequest](arr, readerConfig))
            catch { case NonFatal(_) => Exit.fail(BodyDecodeErrorResponse) }
        )

      val isApplicationGql =
        httpReq.body.mediaType.exists { mt =>
          mt.subType.equalsIgnoreCase("graphql") &&
          mt.mainType.equalsIgnoreCase("application")
        }

      if (isApplicationGql) decodeApplicationGql()
      else if (httpReq.body.mediaType.exists(MediaType.application.json.matches(_, ignoreParameters = true)))
        decodeJson()
      else Exit.fail(UnsupportedMediaTypeResponse)
    }

    val queryParams = httpReq.url.queryParams

    if (httpReq.method eq Method.GET) {
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
      body       <- boundedBody(request.body)
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

  private def executeRequest(httpRequest: Request, req: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R, Response, GraphQLResponseContext.Classified[GraphQLResponse[Any]]] =
    GraphQLResponseContext.capture(
      IncomingRequestHeaders.locally(
        httpRequest.headers.iterator.map(header => header.headerName -> header.renderedValue).toList
      )(interpreter.executeRequest(if (httpRequest.method == Method.GET) req.asHttpGetRequest else req))
    )

  private def responseHeaders(headers: Headers, cacheDirective: Option[String]): Headers =
    cacheDirective match {
      case None    => headers
      case Some(h) => headers.addHeader(Header.CacheControl.name, h)
    }

  private def transformResponse(httpReq: Request, resp: GraphQLResponse[Any], outcome: Outcome)(implicit
    trace: Trace
  ): Response = {
    val encoding       = responseEncoding(httpReq).getOrElse(ResponseEncoding.Json)
    val cacheDirective = resp.extensions.flatMap(HttpUtils.computeCacheDirective)
    val mutationOnGet  = outcome == Outcome.MethodNotAllowed

    def responseStatus(requestErrorsAreBadRequests: Boolean): Status =
      outcome match {
        case Outcome.ServerError(status)                         => Status.fromInt(status)
        case Outcome.MethodNotAllowed                            => Status.MethodNotAllowed
        case Outcome.RequestError if requestErrorsAreBadRequests => Status.BadRequest
        case _                                                   => Status.Ok
      }

    resp match {
      case resp @ GraphQLResponse(StreamValue(stream), _, _, _) =>
        Response(
          Status.Ok,
          headers = responseHeaders(ContentTypeMultipart, None),
          body = Body.fromStreamChunked(encodeMultipartMixedResponse(resp, stream))
        )
      case resp if encoding == ResponseEncoding.EventStream     =>
        val response = Response.fromServerSentEvents(encodeTextEventStream(resp))
        response.copy(
          status = responseStatus(requestErrorsAreBadRequests = true),
          headers = response.headers ++ allowPost(mutationOnGet)
        )
      case resp if encoding == ResponseEncoding.Multipart       =>
        Response(
          responseStatus(requestErrorsAreBadRequests = true),
          headers = responseHeaders(ContentTypeMultipart, cacheDirective) ++ allowPost(mutationOnGet),
          body = Body.fromStreamChunked(encodeMultipartMixedResponse(resp, ZStream.succeed(resp.data)))
        )
      case resp if encoding == ResponseEncoding.GraphQLJson     =>
        Response(
          status = responseStatus(requestErrorsAreBadRequests = true),
          headers = responseHeaders(ContentTypeGql, cacheDirective) ++ allowPost(mutationOnGet),
          body = encodeSingleResponse(
            resp,
            keepDataOnErrors = outcome == Outcome.Executed,
            hasCacheDirective = cacheDirective.isDefined
          )
        )
      case resp                                                 =>
        Response(
          status = responseStatus(requestErrorsAreBadRequests = false),
          headers = responseHeaders(ContentTypeJson, cacheDirective) ++ allowPost(mutationOnGet),
          body = encodeSingleResponse(
            resp,
            keepDataOnErrors = true,
            hasCacheDirective = cacheDirective.isDefined
          )
        )
    }
  }

  private def encodeSingleResponse(
    resp: GraphQLResponse[Any],
    keepDataOnErrors: Boolean,
    hasCacheDirective: Boolean
  ): Body = {
    val excludeExtensions = if (hasCacheDirective) Some(Set(Caching.DirectiveName)) else None
    Body.fromArray(writeToArray(resp.toResponseValue(keepDataOnErrors, excludeExtensions)))
  }

  private def encodeMultipartMixedResponse(
    resp: GraphQLResponse[Any],
    stream: ZStream[Any, Throwable, ResponseValue]
  )(implicit trace: Trace): ZStream[Any, Throwable, Byte] = {
    import HttpUtils.DeferMultipart._
    val pipeline = createPipeline(resp)

    stream
      .via(pipeline)
      .map(writeToArray(_))
      .intersperse(InnerBoundary.getBytes(UTF_8), InnerBoundary.getBytes(UTF_8), EndBoundary.getBytes(UTF_8))
      .mapConcatChunk(Chunk.fromArray)
  }

  private def encodeTextEventStream(
    resp: GraphQLResponse[Any]
  )(implicit trace: Trace): UStream[ServerSentEvent[String]] =
    ServerSentEvents.transformResponse(
      resp,
      v => ServerSentEvent(writeToString(v), Some("next")),
      CompleteSse,
      sseConfig.heartbeatInterval.map(d => ZStream.succeed(ServerSentEvent.heartbeat).repeat(Schedule.fixed(d)))
    )

  private def isFtv1Request(req: Request) =
    req.headers.get(GraphQLRequest.`apollo-federation-include-trace`) match {
      case None    => false
      case Some(h) => h.equalsIgnoreCase(GraphQLRequest.ftv1)
    }

  private def readBody(body: Body)(implicit trace: Trace): IO[Response, Array[Byte]] =
    if (body.knownContentLength.exists(_ > maxRequestBodyBytes.toLong))
      ZIO.fail(RequestEntityTooLargeResponse)
    else
      body.asStream
        .take(maxRequestBodyBytes.toLong + 1L)
        .runCollect
        .mapError(_ => BodyDecodeErrorResponse)
        .flatMap { bytes =>
          if (bytes.length > maxRequestBodyBytes) ZIO.fail(RequestEntityTooLargeResponse)
          else ZIO.succeed(bytes.toArray)
        }

  private def boundedBody(body: Body)(implicit trace: Trace): IO[Response, Body] =
    readBody(body).map { bytes =>
      val bounded = Body.fromArray(bytes)
      body.contentType.fold(bounded)(bounded.contentType)
    }

  private def responseEncoding(request: Request): Option[ResponseEncoding] =
    request.headers.get(Header.Accept.name) match {
      case None        => Some(ResponseEncoding.Json)
      case Some(value) =>
        Header.Accept
          .parse(value)
          .toOption
          .flatMap(accept => ResponseEncoding.negotiate(accept.mimeTypes.toList))
    }

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
  private sealed trait ResponseEncoding

  private object ResponseEncoding {
    case object GraphQLJson extends ResponseEncoding
    case object Json        extends ResponseEncoding
    case object EventStream extends ResponseEncoding
    case object Multipart   extends ResponseEncoding

    private final case class Supported(value: ResponseEncoding, mediaType: MediaType)
    private final case class Negotiated(value: ResponseEncoding, quality: Double, preference: Int)
    private final case class MatchedRange(
      value: Header.Accept.MediaTypeWithQFactor,
      specificity: Int,
      position: Int
    )

    private val supported = List(
      Supported(GraphQLJson, MediaType("application", "graphql-response+json")),
      Supported(Json, MediaType.application.json),
      Supported(EventStream, MediaType.text.`event-stream`),
      Supported(
        Multipart,
        MediaType.multipart.mixed.copy(parameters = DeferMultipart.DeferHeaderParams.map { case (name, value) =>
          name.toLowerCase -> value
        })
      )
    )

    def negotiate(ranges: List[Header.Accept.MediaTypeWithQFactor]): Option[ResponseEncoding] =
      if (ranges.exists(range => range.mediaType.parameters.contains("q") && range.qFactor.isEmpty)) None
      else
        supported.zipWithIndex.flatMap { case (candidate, preference) =>
          bestMatch(candidate.mediaType, ranges).flatMap { range =>
            val quality = range.qFactor.getOrElse(1d)
            if (quality > 0d && quality <= 1d) Some(Negotiated(candidate.value, quality, preference)) else None
          }
        }.reduceOption { (current, candidate) =>
          if (
            candidate.quality > current.quality ||
            (candidate.quality == current.quality && candidate.preference < current.preference)
          ) candidate
          else current
        }.map(_.value)

    private def bestMatch(
      candidate: MediaType,
      ranges: List[Header.Accept.MediaTypeWithQFactor]
    ): Option[Header.Accept.MediaTypeWithQFactor] =
      ranges.zipWithIndex.collect {
        case (range, position) if matches(candidate, range.mediaType) =>
          MatchedRange(range, specificity(range.mediaType), position)
      }.reduceOption { (current, matched) =>
        if (
          matched.specificity > current.specificity ||
          (matched.specificity == current.specificity && matched.position < current.position)
        ) matched
        else current
      }.map(_.value)

    private def matches(candidate: MediaType, range: MediaType): Boolean = {
      val parameters = range.parameters - "q"
      (range.mainType == "*" || range.mainType.equalsIgnoreCase(candidate.mainType)) &&
      (range.subType == "*" || range.subType.equalsIgnoreCase(candidate.subType)) &&
      parameters.forall { case (name, value) =>
        candidate.parameters.get(name.toLowerCase).exists(_.equalsIgnoreCase(value))
      }
    }

    private def specificity(mediaType: MediaType): Int = {
      val typeSpecificity =
        if (mediaType.mainType == "*") 0
        else if (mediaType.subType == "*") 1
        else 2
      typeSpecificity * 100 + (mediaType.parameters - "q").size
    }
  }

  private def badRequest(msg: String) =
    Response(Status.BadRequest, body = Body.fromString(msg))

  private def methodNotAllowed(allow: String) =
    Response(Status.MethodNotAllowed, headers = Headers(Header.Custom("Allow", allow)))

  private def allowPost(enabled: Boolean): Headers =
    if (enabled) Headers(Header.Custom("Allow", "POST")) else Headers.empty

  private val ContentTypeJson =
    Headers(Header.ContentType(MediaType.application.json).untyped)

  private val ContentTypeGql =
    Headers(Header.ContentType(MediaType("application", "graphql-response+json")).untyped)

  private val ContentTypeMultipart =
    Headers(Header.ContentType(MediaType.multipart.mixed.copy(parameters = DeferMultipart.DeferHeaderParams)).untyped)

  private val CompleteSse = ServerSentEvent("", Some("complete"))

  private val BodyDecodeErrorResponse =
    badRequest("Failed to decode json body")

  private val EmptyRequestErrorResponse =
    badRequest("No GraphQL query to execute")

  private val RequestEntityTooLargeResponse =
    Response.status(Status.RequestEntityTooLarge)

  private val UnsupportedMediaTypeResponse =
    Response.status(Status.UnsupportedMediaType)

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
        null.asInstanceOf[InputValue.ObjectValue]
    }

  private implicit val responseCodec: JsonValueCodec[ResponseValue] = ValueJsoniter.responseValueCodec

  private val readerConfig: ReaderConfig = ReaderConfig
    .withAppendHexDumpToParseException(false)
    .withMaxBufSize(Int.MaxValue - 2)
    .withMaxCharBufSize(Int.MaxValue - 2)
}
