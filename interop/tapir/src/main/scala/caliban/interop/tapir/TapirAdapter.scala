package caliban.interop.tapir

import caliban.GraphQLResponseContext.{ Outcome, ServerFailure }
import caliban.HttpUtils.Delivery
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
    buildHttpResponse(request, response, GraphQLResponseContext.outcome(response))

  private[tapir] def executeHttpRequest[R, E, BS](
    interpreter: GraphQLInterpreter[R, E],
    request: GraphQLRequest,
    serverRequest: ServerRequest
  )(implicit streamConstructor: StreamConstructor[BS]): URIO[R, CalibanResponse[BS]] =
    IncomingRequestHeaders.locally(headerValues(serverRequest))(
      GraphQLResponseContext.capture(interpreter.executeRequest(request))(buildHttpResponse[E, BS](serverRequest, _, _))
    )

  private def buildHttpResponse[E, BS](
    request: ServerRequest,
    response: GraphQLResponse[E],
    outcome: Outcome
  )(implicit streamConstructor: StreamConstructor[BS]): (MediaType, StatusCode, Option[String], CalibanBody[BS]) = {
    val accepts        = new HttpUtils.AcceptsGqlEncodings(request.header(HeaderNames.Accept))
    val cacheDirective = response.extensions.flatMap(HttpUtils.computeCacheDirective)

    val status = outcome match {
      case ServerFailure.Internal                      => StatusCode.InternalServerError
      case ServerFailure.Unavailable                   => StatusCode.ServiceUnavailable
      case ServerFailure.TimedOut                      => StatusCode.GatewayTimeout
      case Outcome.MutationOverGet                     => StatusCode.BadRequest
      case Outcome.RequestError if accepts.graphQLJson => StatusCode.BadRequest
      case _                                           => StatusCode.Ok
    }

    def sse(events: ZStream[Any, Nothing, ResponseValue]) =
      (MediaType.TextEventStream, status, None, encodeTextEventStreamResponse(events))

    HttpUtils.delivery(response, outcome) match {
      case Delivery.Incremental(responses)                                 =>
        (deferMultipartMediaType, StatusCode.Ok, None, encodeMultipartMixedResponse(responses))
      case subscription: Delivery.Subscription if accepts.serverSentEvents => sse(subscription.recovered)
      case _: Delivery.Subscription                                        =>
        (MediaType.ApplicationJson, StatusCode.BadRequest, None, Left(HttpUtils.SubscriptionOverJsonError))
      case _ if accepts.graphQLJson                                        =>
        (
          GraphqlResponseJson.mediaType,
          status,
          cacheDirective,
          encodeSingleResponse(
            response,
            keepDataOnErrors = status == StatusCode.Ok,
            excludeExtensions = cacheDirective.map(_ => Set(Caching.DirectiveName))
          )
        )
      case _ if accepts.serverSentEvents                                   => sse(ZStream.succeed(response.toResponseValue))
      case _                                                               =>
        (
          MediaType.ApplicationJson,
          status,
          cacheDirective,
          encodeSingleResponse(
            response,
            keepDataOnErrors = true,
            excludeExtensions = cacheDirective.map(_ => Set(Caching.DirectiveName))
          )
        )
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

    Right(
      streamConstructor(
        responses
          .map(responseCodec.encode)
          .intersperse(InnerBoundary, InnerBoundary, EndBoundary)
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
