package caliban

import caliban.Configurator.ExecutionConfiguration
import caliban.GraphQLResponseContext.{ Outcome, ServerFailure }
import caliban.HttpUtils.{ DeferMultipart, ServerSentEvents }
import caliban.ResponseValue.StreamValue
import caliban.Value.NullValue
import caliban.interop.jsoniter.{ BoundedOutputStream, GraphQLResponseJsoniter, ValueJsoniter }
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

final private class QuickRequestHandler[R] private (
  interpreter: GraphQLInterpreter[R, Any],
  requestExecution: QuickRequestHandler.RequestExecution[R],
  wsConfig: quick.WebSocketConfig[R],
  sseConfig: quick.SseConfig,
  maxRequestBodyBytes: Int,
  maxUploadBodyBytes: Int,
  maxResponseBodyBytes: Int
) {
  import QuickRequestHandler._
  import ValueJsoniter.stringListCodec

  private def copy[R1 <: R](
    interpreter: GraphQLInterpreter[R1, Any] = this.interpreter,
    requestExecution: RequestExecution[R1] = this.requestExecution,
    wsConfig: quick.WebSocketConfig[R1] = this.wsConfig,
    sseConfig: quick.SseConfig = this.sseConfig,
    maxRequestBodyBytes: Int = this.maxRequestBodyBytes,
    maxUploadBodyBytes: Int = this.maxUploadBodyBytes,
    maxResponseBodyBytes: Int = this.maxResponseBodyBytes
  ): QuickRequestHandler[R1] =
    new QuickRequestHandler(
      interpreter,
      requestExecution,
      wsConfig,
      sseConfig,
      maxRequestBodyBytes,
      maxUploadBodyBytes,
      maxResponseBodyBytes
    )

  def configure(config: ExecutionConfiguration)(implicit trace: Trace): QuickRequestHandler[R] =
    copy(
      interpreter = interpreter.wrapExecutionWith[R, Any](Configurator.locally(config)(_)),
      requestExecution = requestExecution.configure(config)
    )

  def configure[R1](configurator: QuickAdapter.Configurator[R1])(implicit
    trace: Trace
  ): QuickRequestHandler[R & R1] =
    copy[R & R1](
      interpreter = interpreter.wrapExecutionWith[R & R1, Any](exec => ZIO.scoped[R1 & R](configurator *> exec)),
      requestExecution = requestExecution.configure(configurator)
    )

  def configureWebSocket[R1](config: quick.WebSocketConfig[R1]): QuickRequestHandler[R & R1] =
    copy[R & R1](wsConfig = config)

  def configureSse(config: quick.SseConfig): QuickRequestHandler[R] =
    copy(sseConfig = config)

  def withMaxRequestBodyBytes(value: Int): QuickRequestHandler[R] =
    copy(maxRequestBodyBytes = value)

  def withMaxUploadBodyBytes(value: Int): QuickRequestHandler[R] =
    copy(maxUploadBodyBytes = value)

  def withMaxResponseBodyBytes(value: Int): QuickRequestHandler[R] =
    copy(maxResponseBodyBytes = value)

  def handleHttpRequest(request: Request)(implicit
    trace: Trace
  ): URIO[R, Response] =
    if (request.method != Method.GET && request.method != Method.POST)
      ZIO.succeed(methodNotAllowed("GET, POST"))
    else
      responseEncoding(request) match {
        case None           => ZIO.succeed(NotAcceptableResponse)
        case Some(encoding) =>
          if (request.body.mediaType.exists(MediaType.multipart.`form-data`.matches(_, ignoreParameters = true)))
            handleUploadRequest(request, encoding)
          else
            ZIO.suspendSucceed {
              transformHttpRequest(request)
                .flatMap(req => executeRequest(request, req, encoding))
                .foldZIO(
                  Exit.succeed,
                  Exit.succeed
                )
            }
      }

  def handleUploadRequest(request: Request)(implicit trace: Trace): URIO[R, Response] =
    responseEncoding(request) match {
      case None           => ZIO.succeed(NotAcceptableResponse)
      case Some(encoding) => handleUploadRequest(request, encoding)
    }

  private def handleUploadRequest(request: Request, encoding: ResponseEncoding)(implicit
    trace: Trace
  ): URIO[R, Response] = ZIO.suspendSucceed {
    transformUploadRequest(request).flatMap { case (req, fileHandle) =>
      executeRequest(request, req, encoding).provideSomeLayer[R](fileHandle)
    }.foldZIO(
      Exit.succeed,
      Exit.succeed
    )
  }

  def handleWebSocketRequest(request: Request)(implicit trace: Trace): URIO[R, Response] =
    Response.fromSocketApp {
      val protocol = request.headers.get(Header.SecWebSocketProtocol) match {
        case Some(value) => Protocol.fromName(value.renderedValue)
        case None        => Protocol.Legacy
      }
      Handler
        .webSocket(ch =>
          IncomingRequestHeaders.locally(request.headers.iterator.map(h => h.headerName -> h.renderedValue).toList)(
            webSocketChannelListener(protocol)(ch)
          )
        )
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
      val isJson           =
        httpReq.body.mediaType.forall { mt =>
          MediaType.application.json.matches(mt, ignoreParameters = true) ||
          (mt.mainType.equalsIgnoreCase("application") && mt.subType.equalsIgnoreCase("graphql+json"))
        }

      if (isApplicationGql) decodeApplicationGql()
      else if (isJson) decodeJson()
      else Exit.fail(UnsupportedMediaTypeResponse)
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
      body       <- boundedBody(request.body, maxUploadBodyBytes)
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

  private def executeRequest(httpRequest: Request, req: GraphQLRequest, encoding: ResponseEncoding)(implicit
    trace: Trace
  ): ZIO[R, Response, Response] =
    IncomingRequestHeaders
      .locally(
        httpRequest.headers.iterator.map(header => header.headerName -> header.renderedValue).toList
      )(
        requestExecution.execute(if (httpRequest.method == Method.GET) req.asHttpGetRequest else req) { result =>
          buildResponse(httpRequest, result, encoding)
        }
      )
      .absolve

  private def responseHeaders(headers: Headers, cacheDirective: Option[String]): Headers =
    cacheDirective match {
      case None    => headers
      case Some(h) => headers.addHeader(Header.CacheControl.name, h)
    }

  private def buildResponse(
    httpReq: Request,
    result: GraphQLResponseContext.Classified[GraphQLResponse[Any]],
    encoding: ResponseEncoding
  )(implicit trace: Trace): Either[Response, Response] = {
    val response       = result.value
    val outcome        = result.outcome
    val cacheDirective = response.extensions.flatMap(HttpUtils.computeCacheDirective)
    val mutationOnGet  =
      httpReq.method == Method.GET && response.errors.contains(HttpUtils.MutationOverGetError)

    def responseStatus(requestErrorsAreBadRequests: Boolean): Status =
      if (mutationOnGet) Status.MethodNotAllowed
      else
        outcome match {
          case Outcome.ServerError(ServerFailure.Internal)         => Status.InternalServerError
          case Outcome.ServerError(ServerFailure.Unavailable)      => Status.ServiceUnavailable
          case Outcome.ServerError(ServerFailure.TimedOut)         => Status.GatewayTimeout
          case Outcome.RequestError if requestErrorsAreBadRequests => Status.BadRequest
          case _                                                   => Status.Ok
        }

    // Top-level streams with hasNext (even false) are incremental; without it, elements are full subscription responses.
    response match {
      case resp @ GraphQLResponse(StreamValue(stream), _, _, Some(_)) =>
        Right(
          Response(
            Status.Ok,
            headers = responseHeaders(ContentTypeMultipart, None),
            body = Body.fromStreamChunked(encodeMultipartMixedResponse(resp, stream))
          )
        )
      case resp if encoding == ResponseEncoding.EventStream           =>
        val encoded = Response.fromServerSentEvents(encodeTextEventStream(resp))
        Right(
          encoded.copy(
            status = responseStatus(requestErrorsAreBadRequests = false),
            headers = encoded.headers ++ allowPost(mutationOnGet)
          )
        )
      case GraphQLResponse(_: StreamValue, _, _, None)                =>
        Left(errorResponse(Status.BadRequest, "Subscriptions require text/event-stream or WebSocket."))
      case resp if encoding == ResponseEncoding.Multipart             =>
        Right(
          Response(
            responseStatus(requestErrorsAreBadRequests = true),
            headers = responseHeaders(ContentTypeMultipart, cacheDirective) ++ allowPost(mutationOnGet),
            body = Body.fromStreamChunked(encodeMultipartMixedResponse(resp, ZStream.succeed(resp.data)))
          )
        )
      case resp                                                       =>
        val contentType = if (encoding == ResponseEncoding.GraphQLJson) ContentTypeGql else ContentTypeJson
        encodeSingleResponse(
          resp,
          keepDataOnErrors = encoding != ResponseEncoding.GraphQLJson || outcome == Outcome.Executed,
          hasCacheDirective = cacheDirective.isDefined
        ).map(bytes =>
          Response(
            status = responseStatus(requestErrorsAreBadRequests = encoding == ResponseEncoding.GraphQLJson),
            headers = responseHeaders(contentType, cacheDirective) ++ allowPost(mutationOnGet),
            body = Body.fromArray(bytes)
          )
        )
    }
  }

  private def encodeSingleResponse(
    resp: GraphQLResponse[Any],
    keepDataOnErrors: Boolean,
    hasCacheDirective: Boolean
  ): Either[Response, Array[Byte]] = {
    val codec: JsonValueCodec[GraphQLResponse[Any]] =
      responseCodec(keepDataOnErrors, hasCacheDirective)

    try
      Right(GraphQLResponseJsoniter.writeToArray(resp, maxResponseBodyBytes, codec))
    catch {
      case BoundedOutputStream.LimitExceeded =>
        Left(errorResponse(Status.InternalServerError, "Encoded GraphQL response exceeds the configured limit."))
    }
  }

  private def encodeMultipartMixedResponse(
    resp: GraphQLResponse[Any],
    stream: ZStream[Any, Throwable, ResponseValue]
  )(implicit trace: Trace): ZStream[Any, Throwable, Byte] = {
    import HttpUtils.DeferMultipart._
    val pipeline = createPipeline(resp)

    stream
      .via(pipeline)
      .map(encodeResponseValue)
      .catchAllCause { cause =>
        if (responseLimitExceeded(cause)) ZStream.succeed(responseLimitErrorBytes)
        else ZStream.failCause(cause)
      }
      .intersperse(InnerBoundary.getBytes(UTF_8), InnerBoundary.getBytes(UTF_8), EndBoundary.getBytes(UTF_8))
      .mapConcatChunk(Chunk.fromArray)
  }

  private def encodeTextEventStream(
    resp: GraphQLResponse[Any]
  )(implicit trace: Trace): UStream[ServerSentEvent[String]] =
    ServerSentEvents
      .transformResponse(
        resp,
        v => ServerSentEvent(new String(encodeResponseValue(v), UTF_8), Some("next")),
        CompleteSse,
        sseConfig.heartbeatInterval.map(d => ZStream.succeed(ServerSentEvent.heartbeat).repeat(Schedule.fixed(d)))
      )
      .catchAllCause { cause =>
        if (responseLimitExceeded(cause))
          ZStream.succeed(responseLimitErrorSse) ++ ZStream.succeed(CompleteSse)
        else ZStream.failCause(cause)
      }

  private def responseLimitExceeded(cause: Cause[Any]): Boolean =
    cause.defects.contains(BoundedOutputStream.LimitExceeded)

  private def encodeResponseValue(value: ResponseValue): Array[Byte] =
    GraphQLResponseJsoniter.writeToArray(value, maxResponseBodyBytes, ValueJsoniter.responseValueCodec)

  private lazy val responseLimitErrorBytes: Array[Byte] =
    writeToArray(
      GraphQLResponse(
        NullValue,
        List(CalibanError.ExecutionError("Encoded GraphQL response exceeds the configured limit."))
      ).toResponseValue
    )(ValueJsoniter.responseValueCodec)

  private lazy val responseLimitErrorSse: ServerSentEvent[String] =
    ServerSentEvent(new String(responseLimitErrorBytes, UTF_8), Some("next"))

  private def isFtv1Request(req: Request) =
    req.headers.get(GraphQLRequest.`apollo-federation-include-trace`) match {
      case None    => false
      case Some(h) => h.equalsIgnoreCase(GraphQLRequest.ftv1)
    }

  private def readBody(body: Body)(implicit trace: Trace): IO[Response, Array[Byte]] =
    readBody(body, maxRequestBodyBytes)

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

  private def responseEncoding(request: Request): Option[ResponseEncoding] =
    request.headers.get(Header.Accept.name) match {
      case None                                                                                  => Some(ResponseEncoding.Json)
      case Some(value) if value.trim == "*/*" || value.trim.equalsIgnoreCase("application/json") =>
        Some(ResponseEncoding.Json)
      case Some(value)                                                                           =>
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
  private trait RequestExecution[-R] { self =>
    def execute[A](request: GraphQLRequest)(
      f: GraphQLResponseContext.Classified[GraphQLResponse[Any]] => A
    )(implicit trace: Trace): URIO[R, A]

    def configure(config: ExecutionConfiguration): RequestExecution[R] =
      new RequestExecution[R] {
        def execute[A](request: GraphQLRequest)(
          f: GraphQLResponseContext.Classified[GraphQLResponse[Any]] => A
        )(implicit trace: Trace): URIO[R, A] =
          Configurator.locally(config)(self.execute(request)(f))
      }

    def configure[R1](configurator: QuickAdapter.Configurator[R1]): RequestExecution[R & R1] =
      new RequestExecution[R & R1] {
        def execute[A](request: GraphQLRequest)(
          f: GraphQLResponseContext.Classified[GraphQLResponse[Any]] => A
        )(implicit trace: Trace): URIO[R & R1, A] =
          ZIO.scoped[R & R1](configurator *> self.execute(request)(f))
      }
  }

  private object RequestExecution {
    def apply[R](interpreter: GraphQLInterpreter[R, Any]): RequestExecution[R] =
      new RequestExecution[R] {
        def execute[A](request: GraphQLRequest)(
          f: GraphQLResponseContext.Classified[GraphQLResponse[Any]] => A
        )(implicit trace: Trace): URIO[R, A] =
          interpreter.executeRequestWith(request)(f)
      }
  }

  private[caliban] def apply[R](
    interpreter: GraphQLInterpreter[R, Any],
    wsConfig: quick.WebSocketConfig[R],
    sseConfig: quick.SseConfig,
    maxRequestBodyBytes: Int,
    maxUploadBodyBytes: Int,
    maxResponseBodyBytes: Int
  ): QuickRequestHandler[R] =
    new QuickRequestHandler(
      interpreter,
      RequestExecution(interpreter),
      wsConfig,
      sseConfig,
      maxRequestBodyBytes,
      maxUploadBodyBytes,
      maxResponseBodyBytes
    )

  private sealed trait ResponseEncoding

  private object ResponseEncoding {
    case object GraphQLJson extends ResponseEncoding
    case object Json        extends ResponseEncoding
    case object EventStream extends ResponseEncoding
    case object Multipart   extends ResponseEncoding

    private final case class Supported(value: ResponseEncoding, mediaType: MediaType)
    private final case class Negotiated(
      value: ResponseEncoding,
      quality: Double,
      specificity: Int,
      preference: Int
    )
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
            val quality = range.value.qFactor.getOrElse(1d)
            if (quality > 0d && quality <= 1d)
              Some(
                Negotiated(
                  candidate.value,
                  quality,
                  range.specificity,
                  if (range.specificity == 0 && candidate.value == Json) -1 else preference
                )
              )
            else None
          }
        }.reduceOption { (current, candidate) =>
          if (
            candidate.quality > current.quality ||
            (candidate.quality == current.quality && candidate.specificity > current.specificity) ||
            (candidate.quality == current.quality && candidate.specificity == current.specificity &&
              candidate.preference < current.preference)
          ) candidate
          else current
        }.map(_.value)

    private def bestMatch(
      candidate: MediaType,
      ranges: List[Header.Accept.MediaTypeWithQFactor]
    ): Option[MatchedRange] =
      ranges.zipWithIndex.collect {
        case (range, position) if matches(candidate, range.mediaType) =>
          MatchedRange(range, specificity(range.mediaType), position)
      }.reduceOption { (current, matched) =>
        if (
          matched.specificity > current.specificity ||
          (matched.specificity == current.specificity && matched.position < current.position)
        ) matched
        else current
      }

    private def matches(candidate: MediaType, range: MediaType): Boolean = {
      val ignoreBoundary = isMultipart(range)
      val parameters     = range.parameters.filterNot { case (name, _) =>
        name.equalsIgnoreCase("q") || ignoreBoundary && name.equalsIgnoreCase("boundary")
      }
      (range.mainType == "*" || range.mainType.equalsIgnoreCase(candidate.mainType)) &&
      (range.subType == "*" || range.subType.equalsIgnoreCase(candidate.subType)) &&
      parameters.forall {
        case (name, value) if name.equalsIgnoreCase("charset") =>
          val normalized = unquote(value)
          normalized.equalsIgnoreCase("utf-8") || normalized.equalsIgnoreCase("utf8")
        case (name, value)                                     =>
          candidate.parameters
            .get(name.toLowerCase)
            .exists(candidateValue => unquote(candidateValue).equalsIgnoreCase(unquote(value)))
      }
    }

    private def specificity(mediaType: MediaType): Int = {
      val typeSpecificity =
        if (mediaType.mainType == "*") 0
        else if (mediaType.subType == "*") 1
        else 2
      val ignoreBoundary  = isMultipart(mediaType)
      val parameters      = mediaType.parameters.keysIterator.count(name =>
        !name.equalsIgnoreCase("q") && !(ignoreBoundary && name.equalsIgnoreCase("boundary"))
      )
      typeSpecificity * 100 + parameters
    }

    private def isMultipart(mediaType: MediaType): Boolean =
      mediaType.mainType.equalsIgnoreCase("multipart")

    private def unquote(value: String): String =
      if (value.length >= 2 && value.head == '"' && value.last == '"') value.substring(1, value.length - 1)
      else value
  }

  private def badRequest(msg: String) =
    errorResponse(Status.BadRequest, msg)

  private def errorResponse(status: Status, message: String) =
    Response(status, body = Body.fromString(message))

  private def methodNotAllowed(allow: String) =
    errorResponse(Status.MethodNotAllowed, "Method not allowed.")
      .addHeader(Header.Custom("Allow", allow))

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
    errorResponse(Status.RequestEntityTooLarge, "GraphQL request body exceeds the configured limit.")

  private val NotAcceptableResponse =
    errorResponse(Status.NotAcceptable, "No acceptable GraphQL response encoding.")

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

  private val responseWithDataCodec: JsonValueCodec[GraphQLResponse[Any]]           =
    GraphQLResponseJsoniter.graphQLResponseCodec
  private val responseWithoutDataCodec: JsonValueCodec[GraphQLResponse[Any]]        =
    GraphQLResponseJsoniter.codec(keepDataOnErrors = false)
  private val responseWithoutCacheCodec: JsonValueCodec[GraphQLResponse[Any]]       =
    GraphQLResponseJsoniter.codec(excludeExtensions = Set(Caching.DirectiveName))
  private val responseWithoutDataOrCacheCodec: JsonValueCodec[GraphQLResponse[Any]] =
    GraphQLResponseJsoniter.codec(keepDataOnErrors = false, excludeExtensions = Set(Caching.DirectiveName))

  private def responseCodec(
    keepDataOnErrors: Boolean,
    excludeCacheDirective: Boolean
  ): JsonValueCodec[GraphQLResponse[Any]] =
    if (keepDataOnErrors) {
      if (excludeCacheDirective) responseWithoutCacheCodec else responseWithDataCodec
    } else if (excludeCacheDirective) responseWithoutDataOrCacheCodec
    else responseWithoutDataCodec

  private val readerConfig: ReaderConfig = ReaderConfig
    .withAppendHexDumpToParseException(false)
    .withMaxBufSize(Int.MaxValue - 2)
    .withMaxCharBufSize(Int.MaxValue - 2)
}
