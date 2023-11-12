package caliban.interop.tapir

import caliban.ResponseValue.StreamValue
import caliban._
import caliban.wrappers.Caching
import sttp.capabilities.zio.ZioStreams
import sttp.capabilities.zio.ZioStreams.Pipe
import sttp.capabilities.{ Streams, WebSockets }
import sttp.model.{ headers => _, _ }
import sttp.monad.MonadError
import sttp.tapir.Codec.JsonCodec
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.{ headers, _ }
import zio._
import zio.stream.ZStream

import java.nio.charset.StandardCharsets
import scala.concurrent.Future

object TapirAdapter {

  type CalibanPipe   = Pipe[GraphQLWSInput, Either[GraphQLWSClose, GraphQLWSOutput]]
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

  def outputBody[S](stream: Streams[S])(implicit
    codec: JsonCodec[ResponseValue]
  ): EndpointOutput[CalibanBody[stream.BinaryStream]] =
    oneOf[CalibanBody[stream.BinaryStream]](
      oneOfVariantValueMatcher[CalibanBody.Single](customCodecJsonBody[ResponseValue].map(Left(_)) { case Left(value) =>
        value
      }) { case Left(_) => true },
      oneOfVariantValueMatcher[CalibanBody.Single]({
        stringBodyUtf8AnyFormat(codec.format(GraphqlResponseJson)).map(Left(_)) { case Left(value) => value }
      }) { case Left(_) => true },
      oneOfVariantValueMatcher[CalibanBody.Stream[stream.BinaryStream]](
        streamTextBody(stream)(CodecFormat.Json(), Some(StandardCharsets.UTF_8)).toEndpointIO
          .map(Right(_)) { case Right(value) => value }
      ) { case Right(_) => true }
    )

  def buildHttpResponse[E, BS](
    request: ServerRequest
  )(
    response: GraphQLResponse[E]
  )(implicit
    streamConstructor: StreamConstructor[BS],
    responseCodec: JsonCodec[ResponseValue]
  ): (MediaType, StatusCode, Option[String], CalibanBody[BS]) = {

    /**
     * NOTE: From  1st January 2025 this logic should be changed to use `application/graphql-response+json` as the
     * default content-type when the client does not specify an accept header.
     *
     * @see [[https://graphql.github.io/graphql-over-http/draft/#sec-Legacy-watershed]]
     */
    def acceptsGqlJson = request.acceptsContentTypes.fold(
      _ => false,
      _.exists {
        case ContentTypeRange("application", "graphql-response+json", _, _) => true
        case _                                                              => false
      }
    )
    response match {
      case resp @ GraphQLResponse(StreamValue(stream), _, _, _) =>
        (
          deferMultipartMediaType,
          StatusCode.Ok,
          None,
          encodeMultipartMixedResponse(resp, stream)
        )
      case resp if acceptsGqlJson                               =>
        val code           =
          response.errors.collectFirst { case _: CalibanError.ParsingError | _: CalibanError.ValidationError =>
            StatusCode.BadRequest
          }.getOrElse(StatusCode.Ok)
        val cacheDirective = HttpUtils.computeCacheDirective(response.extensions)
        (
          GraphqlResponseJson.mediaType,
          code,
          HttpUtils.computeCacheDirective(response.extensions),
          encodeSingleResponse(
            resp,
            keepDataOnErrors = false,
            excludeExtensions = cacheDirective.map(_ => Set(Caching.DirectiveName))
          )
        )
      case resp                                                 =>
        val code           =
          response.errors.collectFirst { case HttpRequestMethod.MutationOverGetError => StatusCode.BadRequest }
            .getOrElse(StatusCode.Ok)
        val cacheDirective = HttpUtils.computeCacheDirective(response.extensions)
        (
          MediaType.ApplicationJson,
          code,
          cacheDirective,
          encodeSingleResponse(
            resp,
            keepDataOnErrors = true,
            excludeExtensions = cacheDirective.map(_ => Set(Caching.DirectiveName))
          )
        )
    }
  }

  private val deferMultipartMediaType: MediaType =
    MediaType.MultipartMixed.copy(otherParameters = HttpUtils.DeferMultipart.DeferHeaderParams)

  @deprecated("Kept for binary compatibility purposes. To be removed in 2.5.0", "2.4.3")
  private object DeferMultipart {
    private val Newline        = "\r\n"
    private val ContentType    = "Content-Type: application/json; charset=utf-8"
    private val SubHeader      = s"$Newline$ContentType$Newline$Newline"
    private val Boundary       = "---"
    private val BoundaryHeader = "-"
    private val DeferSpec      = "20220824"

    val InnerBoundary = s"$Newline$Boundary$SubHeader"
    val EndBoundary   = s"$Newline-----$Newline"

    private val DeferHeaderParams: Map[String, String] = Map("boundary" -> BoundaryHeader, "deferSpec" -> DeferSpec)

    val mediaType: MediaType = MediaType.MultipartMixed.copy(otherParameters = DeferHeaderParams)
  }

  private object GraphqlResponseJson extends CodecFormat {
    override val mediaType: MediaType = MediaType("application", "graphql-response+json")
  }

  private def encodeMultipartMixedResponse[E, BS](
    resp: GraphQLResponse[E],
    stream: ZStream[Any, Throwable, ResponseValue]
  )(implicit streamConstructor: StreamConstructor[BS], responseCodec: JsonCodec[ResponseValue]): CalibanBody[BS] = {
    import HttpUtils.DeferMultipart._

    val pipeline = HttpUtils.DeferMultipart.createPipeline(resp)

    Right(
      streamConstructor(
        stream
          .via(pipeline)
          .map(responseCodec.encode)
          .intersperse(InnerBoundary, InnerBoundary, EndBoundary)
          .mapConcat(_.getBytes(StandardCharsets.UTF_8))
      )
    )
  }

  private def encodeSingleResponse[E](
    response: GraphQLResponse[E],
    keepDataOnErrors: Boolean,
    excludeExtensions: Option[Set[String]]
  ) =
    Left(response.toResponseValue(keepDataOnErrors, excludeExtensions))

  def convertHttpEndpointToFuture[R](
    endpoint: ServerEndpoint[ZioStreams, RIO[R, *]]
  )(implicit runtime: Runtime[R]): ServerEndpoint[ZioStreams, Future] =
    ServerEndpoint[
      endpoint.SECURITY_INPUT,
      endpoint.PRINCIPAL,
      endpoint.INPUT,
      endpoint.ERROR_OUTPUT,
      endpoint.OUTPUT,
      ZioStreams,
      Future
    ](
      endpoint.endpoint,
      _ =>
        a => Unsafe.unsafe(implicit u => runtime.unsafe.runToFuture(endpoint.securityLogic(zioMonadError)(a)).future),
      _ =>
        u =>
          req => Unsafe.unsafe(implicit un => runtime.unsafe.runToFuture(endpoint.logic(zioMonadError)(u)(req)).future)
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

}
