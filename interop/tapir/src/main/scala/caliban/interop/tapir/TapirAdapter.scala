package caliban.interop.tapir

import caliban.GraphQLResponseContext.Outcome
import caliban.HttpUtils.{ Delivery, ResponseEncoding }
import caliban._
import caliban.wrappers.Caching
import sttp.capabilities.zio.ZioStreams
import sttp.capabilities.{ Streams, WebSockets }
import sttp.model.sse.ServerSentEvent
import sttp.model.{ headers => _, _ }
import sttp.monad.MonadError
import sttp.shared.Identity
import MaxCharBufSizeJsonJsoniter._
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.ztapir.ZioServerSentEvents
import sttp.tapir.{ headers, _ }
import zio._
import zio.stream.ZStream

import java.nio.charset.StandardCharsets
import scala.concurrent.Future

object TapirAdapter {
  import JsonCodecs.responseCodec

  type CalibanPipe   = caliban.ws.CalibanPipe
  type UploadRequest = (Seq[Part[Array[Byte]]], ServerRequest)
  type ZioWebSockets = ZioStreams with WebSockets

  /**
   * An interceptor is a layer that takes an environment R1 and a server request,
   * and that either fails with a TapirResponse or returns a new environment R
   */
  type Interceptor[-R1, +R] = ZLayer[R1 & ServerRequest, TapirResponse, R]

  /**
   * A configurator is an effect that can be run in the scope of a request and returns Unit.
   * It is usually used to change the value of a configuration fiber ref (see the Configurator object).
   */
  type Configurator[-R] = URIO[R & ServerRequest & Scope, Unit]

  object CalibanBody {
    type Single     = Left[ResponseValue, Nothing]
    type Stream[BS] = Right[Nothing, BS]
  }

  private type CalibanBody[BS]   = Either[ResponseValue, BS]
  type CalibanResponse[BS]       = (MediaType, StatusCode, Option[String], CalibanBody[BS])
  type CalibanEndpoint[R, BS, S] =
    ServerEndpoint.Full[Unit, Unit, (GraphQLRequest, ServerRequest), TapirResponse, CalibanResponse[BS], S, RIO[R, *]]

  type CalibanUploadsEndpoint[R, BS, S] =
    ServerEndpoint.Full[Unit, Unit, UploadRequest, TapirResponse, CalibanResponse[BS], S, RIO[R, *]]

  case class TapirResponse(
    code: StatusCode,
    body: String = "",
    headers: List[Header] = Nil
  ) {
    def withBody(body: String): TapirResponse =
      copy(body = body)

    def withHeader(key: String, value: String): TapirResponse =
      copy(headers = Header(key, value) :: headers)

    def withHeaders(_headers: List[Header]): TapirResponse =
      copy(headers = _headers ++ headers)
  }

  object TapirResponse {

    val ok: TapirResponse                             = TapirResponse(StatusCode.Ok)
    def status(statusCode: StatusCode): TapirResponse = TapirResponse(statusCode)
  }

  private val responseMapping = Mapping.from[(StatusCode, String, List[Header]), TapirResponse](
    (TapirResponse.apply _).tupled
  )(resp => (resp.code, resp.body, resp.headers))

  val errorBody = statusCode.and(stringBody).and(headers).map(responseMapping)

  def outputBody[S](stream: Streams[S]): EndpointOutput[CalibanBody[stream.BinaryStream]] =
    oneOf[CalibanBody[stream.BinaryStream]](
      oneOfVariantValueMatcher[CalibanBody.Single](customCodecJsonBody[ResponseValue].map(Left(_)) { case Left(value) =>
        value
      }) { case Left(_) => true },
      oneOfVariantValueMatcher[CalibanBody.Single]({
        stringBodyUtf8AnyFormat(responseCodec.format(GraphqlResponseJson)).map(Left(_)) { case Left(value) => value }
      }) { case Left(_) => true },
      oneOfVariantValueMatcher[CalibanBody.Stream[stream.BinaryStream]](
        streamTextBody(stream)(CodecFormat.Json(), Some(StandardCharsets.UTF_8)).toEndpointIO
          .map(Right(_)) { case Right(value) => value }
      ) { case Right(_) => true },
      oneOfVariantValueMatcher[CalibanBody.Stream[stream.BinaryStream]](
        streamBinaryBody(stream)(CodecFormat.TextEventStream()).toEndpointIO
          .map(Right(_)) { case Right(value) => value }
      ) { case Right(_) => true }
    )

  def buildHttpResponse[E, BS](
    request: ServerRequest
  )(
    response: GraphQLResponse[E]
  )(implicit
    streamConstructor: StreamConstructor[BS]
  ): (MediaType, StatusCode, Option[String], CalibanBody[BS]) =
    buildHttpResponse(
      request,
      ResponseEncoding.negotiate(request.header(HeaderNames.Accept)).getOrElse(ResponseEncoding.Json),
      StatusCodeConfig.default,
      response,
      GraphQLResponseContext.outcome(response)
    )

  private[tapir] def executeHttpRequest[R, E, BS](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest,
    serverRequest: ServerRequest,
    statusCodes: StatusCodeConfig = StatusCodeConfig.default
  )(implicit streamConstructor: StreamConstructor[BS]): ZIO[R, TapirResponse, CalibanResponse[BS]] =
    ResponseEncoding.negotiate(serverRequest.header(HeaderNames.Accept)) match {
      case None           => ZIO.fail(notAcceptable)
      case Some(encoding) =>
        IncomingRequestHeaders
          .locally(headerValues(serverRequest))(
            GraphQLResponseContext.capture(interpreter.executeRequest(request)) { (response, outcome) =>
              val built = buildHttpResponse[E, BS](serverRequest, encoding, statusCodes, response, outcome)
              built match {
                case (mediaType, status, _, Left(body)) if outcome == Outcome.MutationOverGet =>
                  Left(
                    TapirResponse(status, responseCodec.encode(body))
                      .withHeader(HeaderNames.Allow, "POST")
                      .withHeader(HeaderNames.ContentType, mediaType.toString)
                  )
                case _                                                                        => Right(built)
              }
            }
          )
          .absolve
    }

  private val notAcceptable =
    TapirResponse(StatusCode.NotAcceptable, "None of the media types in the Accept header are supported.")

  private def buildHttpResponse[E, BS](
    request: ServerRequest,
    encoding: ResponseEncoding,
    statusCodes: StatusCodeConfig,
    response: GraphQLResponse[E],
    outcome: Outcome
  )(implicit streamConstructor: StreamConstructor[BS]): (MediaType, StatusCode, Option[String], CalibanBody[BS]) = {
    val cacheDirective = response.extensions.flatMap(HttpUtils.computeCacheDirective)
    val httpStatus     = HttpUtils.httpStatus(response, outcome, encoding, statusCodes)
    val status         = StatusCode(httpStatus.code)

    def sse(events: ZStream[Any, Nothing, ResponseValue]) =
      (MediaType.TextEventStream, status, None, encodeTextEventStreamResponse(events))

    def single(mediaType: MediaType) =
      (
        mediaType,
        status,
        cacheDirective,
        encodeSingleResponse(
          response,
          keepDataOnErrors = httpStatus.keepDataOnErrors,
          excludeExtensions = cacheDirective.map(_ => Set(Caching.DirectiveName))
        )
      )

    HttpUtils.delivery(response, outcome) match {
      case Delivery.Incremental(responses)     =>
        (deferMultipartMediaType, StatusCode.Ok, None, encodeMultipartMixedResponse(responses))
      case subscription: Delivery.Subscription =>
        if (ResponseEncoding.acceptsEventStream(request.header(HeaderNames.Accept))) sse(subscription.recovered)
        else (MediaType.ApplicationJson, StatusCode.BadRequest, None, Left(HttpUtils.SubscriptionOverJsonError))
      case _                                   =>
        httpStatus.encoding match {
          case ResponseEncoding.GraphQLJson => single(GraphqlResponseJson.mediaType)
          case ResponseEncoding.Json        => single(MediaType.ApplicationJson)
          case ResponseEncoding.EventStream => sse(ZStream.succeed(response.toResponseValue))
          case ResponseEncoding.Multipart   =>
            (
              deferMultipartMediaType,
              status,
              cacheDirective,
              encodeMultipartMixedResponse(ZStream.succeed(response.toResponseValue))
            )
        }
    }
  }

  private val deferMultipartMediaType: MediaType =
    MediaType.MultipartMixed.copy(otherParameters = HttpUtils.DeferMultipart.DeferHeaderParams)

  private object GraphqlResponseJson extends CodecFormat {
    override val mediaType: MediaType = MediaType("application", "graphql-response+json")
  }

  private def encodeMultipartMixedResponse[BS](
    responses: ZStream[Any, Throwable, ResponseValue]
  )(implicit streamConstructor: StreamConstructor[BS]): CalibanBody[BS] = {
    import HttpUtils.DeferMultipart._

    // Write the delimiter that ends a part in the same chunk as the part, rather than
    // interspersing the whole boundary as a separator (emitted only when the NEXT part is
    // produced, so the initial `@defer` payload would stay unreadable until the first
    // deferred payload resolves). For any non-empty stream the bytes on the wire are the
    // same as with `intersperse`, and the body still closes on completion.
    Right(
      streamConstructor(
        (ZStream.succeed(PartDelimiter) ++
          responses.map(rv => PartHeader + responseCodec.encode(rv) + PartDelimiter) ++
          ZStream.succeed(CloseDelimiter))
          .mapConcat(_.getBytes(StandardCharsets.UTF_8))
      )
    )
  }

  private def encodeTextEventStreamResponse[BS](
    events: ZStream[Any, Nothing, ResponseValue]
  )(implicit streamConstructor: StreamConstructor[BS]): CalibanBody[BS] = {
    val response = HttpUtils.ServerSentEvents.fromEvents(
      events,
      v => ServerSentEvent(Some(responseCodec.encode(v)), Some("next")),
      ServerSentEvent(None, Some("complete")),
      None
    )
    Right(streamConstructor(ZioServerSentEvents.serialiseSSEToBytes(response)))
  }

  private def encodeSingleResponse[E](
    response: GraphQLResponse[E],
    keepDataOnErrors: Boolean,
    excludeExtensions: Option[Set[String]]
  ) =
    Left(response.toResponseValue(keepDataOnErrors, excludeExtensions))

  private val rightUnit: Right[Nothing, Unit] = Right(())

  private[caliban] def convertHttpEndpointToFuture[R, BS, S, I](
    endpoint: ServerEndpoint.Full[Unit, Unit, I, TapirResponse, CalibanResponse[BS], S, RIO[R, *]]
  )(implicit runtime: Runtime[R]): ServerEndpoint[S, Future] =
    ServerEndpoint[Unit, Unit, I, TapirResponse, CalibanResponse[BS], S, Future](
      endpoint.endpoint,
      _ => _ => Future.successful(rightUnit),
      _ =>
        _ =>
          req => Unsafe.unsafe(implicit u => runtime.unsafe.runToFuture(endpoint.logic(zioMonadError)(())(req)).future)
    )

  private[caliban] def convertHttpEndpointToIdentity[R, BS, S, I](
    endpoint: ServerEndpoint.Full[Unit, Unit, I, TapirResponse, CalibanResponse[BS], S, RIO[R, *]]
  )(implicit runtime: Runtime[R]): ServerEndpoint[S, Identity] =
    ServerEndpoint[Unit, Unit, I, TapirResponse, CalibanResponse[BS], S, Identity](
      endpoint.endpoint,
      _ => _ => rightUnit,
      _ =>
        _ => req => Unsafe.unsafe(implicit u => runtime.unsafe.run(endpoint.logic(zioMonadError)(())(req)).getOrThrow())
    )

  def zioMonadError[R]: MonadError[RIO[R, *]] = new MonadError[RIO[R, *]] {
    override def unit[T](t: T): RIO[R, T]                                                                            = ZIO.succeed(t)
    override def map[T, T2](fa: RIO[R, T])(f: T => T2): RIO[R, T2]                                                   = fa.map(f)
    override def flatMap[T, T2](fa: RIO[R, T])(f: T => RIO[R, T2]): RIO[R, T2]                                       = fa.flatMap(f)
    override def error[T](t: Throwable): RIO[R, T]                                                                   = ZIO.fail(t)
    override protected def handleWrappedError[T](rt: RIO[R, T])(h: PartialFunction[Throwable, RIO[R, T]]): RIO[R, T] =
      rt.catchSome(h)
    override def eval[T](t: => T): RIO[R, T]                                                                         = ZIO.attempt(t)
    override def suspend[T](t: => RIO[R, T]): RIO[R, T]                                                              = ZIO.suspend(t)
    override def flatten[T](ffa: RIO[R, RIO[R, T]]): RIO[R, T]                                                       = ffa.flatten
    override def ensure[T](f: RIO[R, T], e: => RIO[R, Unit]): RIO[R, T]                                              = f.ensuring(e.ignore)
  }

  def isFtv1Header(r: Header): Boolean =
    r.name == GraphQLRequest.`apollo-federation-include-trace` && r.value == GraphQLRequest.ftv1

  private[tapir] def headerValues(request: ServerRequest): List[(String, String)] =
    request.headers.map(header => header.name -> header.value).toList

}
