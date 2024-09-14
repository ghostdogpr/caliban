package caliban

import caliban.Configurator.ExecutionConfiguration
import caliban.HttpUtils.{ DeferMultipart, ServerSentEvents }
import caliban.ResponseValue.StreamValue
import caliban.interop.jsoniter.ValueJsoniter
import caliban.uploads.{ FileMeta, GraphQLUploadRequest, Uploads }
import caliban.wrappers.Caching
import caliban.ws.Protocol
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
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
  wsConfig: quick.WebSocketConfig[R]
) {
  import QuickRequestHandler._

  def configure(config: ExecutionConfiguration)(implicit trace: Trace): QuickRequestHandler[R] =
    new QuickRequestHandler[R](
      interpreter.wrapExecutionWith[R, Any](Configurator.ref.locally(config)(_)),
      wsConfig
    )

  def configure[R1](configurator: QuickAdapter.Configurator[R1])(implicit
    trace: Trace
  ): QuickRequestHandler[R & R1] =
    new QuickRequestHandler[R & R1](
      interpreter.wrapExecutionWith[R & R1, Any](exec => ZIO.scoped[R1 & R](configurator *> exec)),
      wsConfig
    )

  def configureWebSocket[R1](config: quick.WebSocketConfig[R1]): QuickRequestHandler[R & R1] =
    new QuickRequestHandler[R & R1](interpreter, config)

  def handleHttpRequest(request: Request)(implicit trace: Trace): URIO[R, Response] = ZIO.suspendSucceed {
    transformHttpRequest(request)
      .flatMap(executeRequest(request.method, _))
      .foldZIO(
        Exit.succeed,
        resp => Exit.succeed(transformResponse(request, resp))
      )
  }

  def handleUploadRequest(request: Request)(implicit trace: Trace): URIO[R, Response] = ZIO.suspendSucceed {
    transformUploadRequest(request).flatMap { case (req, fileHandle) =>
      executeRequest(request.method, req).provideSomeLayer[R](fileHandle)
    }.foldZIO(
      Exit.succeed,
      v => Exit.succeed(transformResponse(request, v))
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
        try Right(queryParams.getAll(key).headOption.map(readFromString[InputValue.ObjectValue](_).fields))
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
        body.asString.mapBoth(_ => BodyDecodeErrorResponse, b => GraphQLRequest(Some(b)))

      def decodeJson(): ZIO[Any, Response, GraphQLRequest] =
        body.asArray.foldZIO(
          _ => Exit.fail(BodyDecodeErrorResponse),
          arr =>
            try checkNonEmptyRequest(readFromArray[GraphQLRequest](arr))
            catch { case NonFatal(_) => Exit.fail(BodyDecodeErrorResponse) }
        )

      val isApplicationGql =
        httpReq.body.mediaType.exists { mt =>
          mt.subType.equalsIgnoreCase("graphql") &&
          mt.mainType.equalsIgnoreCase("application")
        }

      if (isApplicationGql) decodeApplicationGql() else decodeJson()
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
        .flatMap(v => Exit.fromTry(Try(readFromArray[A](v.toArray))))
        .orElseFail(Response.badRequest)

    def parsePath(path: String): List[PathValue] = path.split('.').toList.map(PathValue.parse)

    for {
      partsMap   <- request.body.asMultipartForm.mapBoth(_ => Response.internalServerError, _.map)
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

  private def executeRequest(method: Method, req: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R, Response, GraphQLResponse[Any]] =
    interpreter.executeRequest(if (method == Method.GET) req.asHttpGetRequest else req)

  private def responseHeaders(headers: Headers, cacheDirective: Option[String]): Headers =
    cacheDirective match {
      case None    => headers
      case Some(h) => headers.addHeader(Header.CacheControl.name, h)
    }

  private def transformResponse(httpReq: Request, resp: GraphQLResponse[Any])(implicit trace: Trace): Response = {
    val accepts        = new HttpUtils.AcceptsGqlEncodings(httpReq.headers.get(Header.Accept.name))
    val cacheDirective = resp.extensions.flatMap(HttpUtils.computeCacheDirective)

    resp match {
      case resp @ GraphQLResponse(StreamValue(stream), _, _, _) =>
        Response(
          Status.Ok,
          headers = responseHeaders(ContentTypeMultipart, None),
          body = Body.fromStreamChunked(encodeMultipartMixedResponse(resp, stream))
        )
      case resp if accepts.serverSentEvents                     =>
        Response.fromServerSentEvents(encodeTextEventStream(resp))
      case resp if accepts.graphQLJson                          =>
        val isBadRequest = resp.errors.exists {
          case _: CalibanError.ParsingError | _: CalibanError.ValidationError => true
          case _                                                              => false
        }
        Response(
          status = if (isBadRequest) Status.BadRequest else Status.Ok,
          headers = responseHeaders(ContentTypeGql, cacheDirective),
          body =
            encodeSingleResponse(resp, keepDataOnErrors = !isBadRequest, hasCacheDirective = cacheDirective.isDefined)
        )
      case resp                                                 =>
        val isBadRequest = resp.errors.contains(HttpUtils.MutationOverGetError)
        Response(
          status = if (isBadRequest) Status.BadRequest else Status.Ok,
          headers = responseHeaders(ContentTypeJson, cacheDirective),
          body = encodeSingleResponse(resp, keepDataOnErrors = true, hasCacheDirective = cacheDirective.isDefined)
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
      CompleteSse
    )

  private def isFtv1Request(req: Request) =
    req.headers.get(GraphQLRequest.`apollo-federation-include-trace`) match {
      case None    => false
      case Some(h) => h.equalsIgnoreCase(GraphQLRequest.ftv1)
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
                   out.runForeach(frame => ch.send(ChannelEvent.Read(frame))).forkScoped
                 case ChannelEvent.Read(WebSocketFrame.Text(text))       =>
                   ZIO.suspend(queue.offer(readFromString[GraphQLWSInput](text)))
                 case _                                                  =>
                   ZIO.unit
               })
    } yield ()
}

object QuickRequestHandler {
  private def badRequest(msg: String) =
    Response(Status.BadRequest, body = Body.fromString(msg))

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

  private implicit val stringListCodec: JsonValueCodec[Map[String, List[String]]] = JsonCodecMaker.make
}
