package caliban.gateway.internal.execution

import caliban.ResponseValue.ObjectValue
import caliban.Value.NullValue
import caliban.gateway.PhaseHooks.{ Event, Outcome, Result }
import caliban.gateway.internal.OperationCache.Weighted
import caliban.gateway.internal.{ GatewayHttpClient, OperationCache, RemoteTransport }
import caliban.gateway.{ PhaseHooks, RemoteGraphQLConfig, RemoteSubscriptionConfig }
import caliban.interop.jsoniter.GraphQLResponseJsoniter
import caliban.parsing.adt.OperationType
import caliban._
import com.github.plokhotnyuk.jsoniter_scala.core._
import zio._
import zio.http.{ Header, URL }
import zio.stream.ZStream

import java.util.Arrays
import scala.util.control.NonFatal

private[gateway] final class RemoteSubgraphExecutor[-R](
  name: String,
  endpoint: URL,
  http: GatewayHttpClient,
  config: RemoteGraphQLConfig[R],
  maxResponseDepth: Int,
  deduplicator: Option[RemoteSubgraphExecutor.QueryDeduplicator],
  hooks: PhaseHooks[R],
  remoteErrorMessages: Boolean,
  fixedHeaders: Option[List[Header]] = None
) extends SubgraphExecutor[R] {
  import RemoteSubgraphExecutor._

  val errorPolicy: SubgraphExecutor.ErrorPolicy = SubgraphExecutor.ErrorPolicy.Remote

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] = {
    def call(headers: List[Header]) =
      for {
        body      <- encode(request.copy(extensions = None))
        replaySafe = operationType == OperationType.Query
        rawCall    = executeAttempts(body, headers, replaySafe, attempt = 0)
        response  <- if (replaySafe)
                       deduplicator.fold(rawCall)(
                         _.getOrCompute(QueryDeduplicator.Key(body, headers))(
                           rawCall.timeoutFail(SubgraphExecutor.TimeoutFailure)(execution.timeout).map(Weighted(_, 0L))
                         )
                       )
                     else rawCall
      } yield response

    if (!hooks.subgraphCall.enabled)
      resolveHeaders.flatMap(call).timeoutFail(SubgraphExecutor.TimeoutFailure)(execution.timeout)
    else
      for {
        started  <- Clock.nanoTime
        headers  <- resolveHeaders.timeoutFail(SubgraphExecutor.TimeoutFailure)(execution.timeout)
        response <- hooks.subgraphCall.runWith(Event.SubgraphCall(name, operationType, headers)) { event =>
                      Clock.nanoTime.flatMap { now =>
                        val remaining = execution.timeout.minusNanos(now - started)
                        if (remaining.isNegative || remaining.isZero) ZIO.fail(SubgraphExecutor.TimeoutFailure)
                        else call(event.headers).timeoutFail(SubgraphExecutor.TimeoutFailure)(remaining)
                      }
                    }(SubgraphExecutor.resultFromExit)
      } yield response
  }

  override def forSubscription(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, SubgraphExecutor[R]] =
    resolveHeaders.map(withHeaders)

  override def subscribe(
    request: GraphQLRequest
  )(implicit trace: Trace): ZIO[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]] =
    ZIO.serviceWithZIO[Scope] { sourceScope =>
      def open(headers: List[Header]) =
        encode(request.copy(extensions = None)).flatMap { body =>
          val post = config.subscription.transport match {
            case RemoteSubscriptionConfig.Sse(useGet) => !useGet
            case RemoteSubscriptionConfig.WebSocket   => false
          }
          hooks.attempt.runWith(
            Event.Attempt(
              name,
              0,
              if (post) body.length.toLong else 0L,
              subscription.target,
              headers,
              if (post) "POST" else "GET"
            )
          )(event => sourceScope.extend(subscription.open(event.headers, request, body)))(
            Result.classifyExit(Outcome.TransportError)
          )
        }

      resolveHeaders.flatMap { headers =>
        hooks.subgraphCall.runWith(Event.SubgraphCall(name, OperationType.Subscription, headers))(event =>
          open(event.headers)
        )(Result.classifyExit(Outcome.TransportError))
      }
    }

  private val execution        = config.execution
  private val forwarded        = execution.forwardedHeaders.map(_.map(RemoteGraphQLConfig.lowercaseHeaderName))
  private val forwardsIncoming = forwarded.forall(_.nonEmpty)
  private val subscription     = new RemoteSubscription(
    endpoint,
    http,
    config.subscription,
    execution.maxResponseBytes,
    decodeBody,
    decodeValue,
    validateStructure,
    remoteErrorMessages
  )

  private def resolveHeaders(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, List[Header]] =
    fixedHeaders match {
      case Some(headers) => ZIO.succeed(headers)
      case None          =>
        for {
          incoming  <- if (forwardsIncoming) IncomingRequestHeaders.get.map(_.map { case (key, value) =>
                         Header.Custom(key, value)
                       })
                       else ZIO.succeed(List.empty[Header])
          effectful <- config.effectfulHeaders.mapError(SubgraphExecutor.HeaderFailure(_))
        } yield outboundHeaders(incoming, effectful)
    }

  private def withHeaders(headers: List[Header]): RemoteSubgraphExecutor[R] =
    new RemoteSubgraphExecutor(
      name,
      endpoint,
      http,
      config,
      maxResponseDepth,
      deduplicator,
      hooks,
      remoteErrorMessages,
      Some(headers)
    )

  private def executeAttempts(body: Array[Byte], headers: List[Header], replaySafe: Boolean, attempt: Int)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] = {
    val response =
      hooks.attempt.runWith(Event.Attempt(name, attempt, body.length.toLong, endpoint, headers))(event =>
        send(body, event.headers)
      )(
        Result.fromExit(_)(
          value =>
            Result
              .fromResponse(value.response)
              .copy(statusCode = Some(value.statusCode), responseBytes = Some(value.responseBytes)),
          failure =>
            Result(
              SubgraphExecutor.failureOutcome(failure.failure),
              statusCode = failure.statusCode,
              responseBytes = failure.responseBytes
            )
        )
      )
    val call     = response.map(_.response).mapError(_.failure)

    call.catchAll { failure =>
      if (replaySafe && attempt < execution.retries && retryable(failure))
        ZIO.sleep(execution.retryBackoff) *>
          executeAttempts(body, headers, replaySafe, attempt + 1)
      else ZIO.fail(failure)
    }
  }

  private def send(body: Array[Byte], headers: List[Header])(implicit
    trace: Trace
  ): ZIO[R, AttemptFailure, AttemptResponse] = {
    val response: ZIO[R, AttemptFailure, GatewayHttpClient.Reply] = http
      .post(endpoint, body, headers, execution.maxResponseBytes)
      .mapError(error => AttemptFailure(SubgraphExecutor.TransportFailure(error), None, None))

    response.flatMap { value =>
      value.body match {
        case None        => ZIO.fail(AttemptFailure(SubgraphExecutor.ResponseTooLarge, Some(value.status.code), None))
        case Some(bytes) =>
          ZIO
            .fromEither(decode(value, bytes))
            .mapBoth(
              AttemptFailure(_, Some(value.status.code), Some(bytes.length.toLong)),
              AttemptResponse(_, value.status.code, bytes.length.toLong)
            )
      }
    }
  }

  private def outboundHeaders(incoming: List[Header], effectful: List[Header]): List[Header] = {
    val selected = sanitizeHeaders(incoming).filter(header => forwarded.forall(_.contains(lowercaseName(header))))
    mergeHeaders(mergeHeaders(selected, execution.headers), sanitizeHeaders(effectful))
  }

  private def sanitizeHeaders(headers: List[Header]): List[Header] = {
    val connectionHeaders = headers.iterator
      .filter(header => lowercaseName(header) == "connection")
      .flatMap(_.renderedValue.split(',').iterator)
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(RemoteGraphQLConfig.lowercaseHeaderName)
      .toSet
    headers.filterNot(header =>
      RemoteGraphQLConfig.isProtocolHeader(header.headerName) || connectionHeaders(lowercaseName(header))
    )
  }

  private def mergeHeaders(lower: List[Header], higher: List[Header]): List[Header] =
    if (higher.isEmpty) lower
    else {
      val overridden = higher.iterator.map(lowercaseName).toSet
      lower.filterNot(header => overridden.contains(lowercaseName(header))) ::: higher
    }

  private def retryable(failure: SubgraphExecutor.Failure): Boolean =
    failure match {
      case SubgraphExecutor.TransportFailure(_)          => true
      case SubgraphExecutor.HttpFailure(502 | 503 | 504) => true
      case _                                             => false
    }

  private def encode(request: GraphQLRequest)(implicit trace: Trace): IO[SubgraphExecutor.Failure, Array[Byte]] =
    ZIO
      .attempt(GraphQLResponseJsoniter.writeToArray(request, execution.maxRequestBytes, GraphQLRequest.jsoniterCodec))
      .orElseFail(SubgraphExecutor.InvalidRequest)
      .someOrFail(SubgraphExecutor.RequestTooLarge)

  private def decode(
    response: GatewayHttpClient.Reply,
    bytes: Array[Byte]
  ): Either[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
    if (response.status.isRedirection) Left(SubgraphExecutor.RedirectResponse)
    else if (RemoteTransport.isJsonResponse(response.status, response.contentType))
      decodeBody(bytes) match {
        case Left(_) if !response.status.isSuccess => Left(SubgraphExecutor.HttpFailure(response.status.code))
        case result                                => result
      }
    else if (!response.status.isSuccess) Left(SubgraphExecutor.HttpFailure(response.status.code))
    else Left(SubgraphExecutor.UnsupportedMediaType)

  private def decodeBody(bytes: Array[Byte]): Either[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
    for {
      _         <- validateStructure(bytes)
      response  <-
        try Right(readFromArray[GraphQLResponse[CalibanError]](bytes))
        catch {
          case NonFatal(_) => Left(SubgraphExecutor.InvalidResponse)
        }
      validated <- validateResponse(response)
    } yield validated

  private def decodeValue(value: ResponseValue): Either[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
    GraphQLResponse
      .fromResponseValue(value)
      .toRight(SubgraphExecutor.InvalidResponse)
      .flatMap(validateResponse)

  private def validateStructure(bytes: Array[Byte]): Either[SubgraphExecutor.Failure, Unit] =
    Either.cond(RemoteTransport.withinJsonDepth(bytes, maxResponseDepth), (), SubgraphExecutor.ResponseNestingTooDeep)

  private def validateResponse(
    response: GraphQLResponse[CalibanError]
  ): Either[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] = {
    val validData = response.data match {
      case _: ObjectValue => true
      case NullValue      => response.errors.nonEmpty
      case _              => false
    }
    Either.cond(
      validData && response.hasNext.isEmpty,
      response.copy(errors = response.errors.map(RemoteError.sanitize(_, remoteErrorMessages))),
      SubgraphExecutor.InvalidResponse
    )
  }
}

private[gateway] object RemoteSubgraphExecutor {

  def make[R](
    name: String,
    endpoint: URL,
    http: GatewayHttpClient,
    config: RemoteGraphQLConfig[R],
    hooks: PhaseHooks[R],
    remoteErrorMessages: Boolean = false
  )(implicit trace: Trace): ZIO[Scope, Nothing, RemoteSubgraphExecutor[R]] =
    ZIO
      .when(config.execution.inFlightQueryDeduplication)(
        OperationCache.make[QueryDeduplicator.Key, SubgraphExecutor.Failure, GraphQLResponse[CalibanError], Any](
          0L,
          PhaseHooks.empty
        )
      )
      .map(
        new RemoteSubgraphExecutor(name, endpoint, http, config, DefaultMaxResponseDepth, _, hooks, remoteErrorMessages)
      )

  final val DefaultMaxResponseDepth = 128

  private final case class AttemptResponse(
    response: GraphQLResponse[CalibanError],
    statusCode: Int,
    responseBytes: Long
  )

  private final case class AttemptFailure(
    failure: SubgraphExecutor.Failure,
    statusCode: Option[Int],
    responseBytes: Option[Long]
  )

  private def lowercaseName(header: Header): String =
    RemoteGraphQLConfig.lowercaseHeaderName(header.headerName)

  private[internal] final class RequestBody(private val bytes: Array[Byte]) {
    private val hash = Arrays.hashCode(bytes)

    override def hashCode(): Int = hash

    override def equals(other: Any): Boolean =
      other match {
        case that: RequestBody => hash == that.hash && Arrays.equals(bytes, that.bytes)
        case _                 => false
      }
  }

  private[internal] type QueryDeduplicator =
    OperationCache[QueryDeduplicator.Key, SubgraphExecutor.Failure, GraphQLResponse[CalibanError], Any]

  private[internal] object QueryDeduplicator {
    final case class Key(body: RequestBody, headers: Vector[(String, String)])

    object Key {
      def apply(body: Array[Byte], headers: List[Header]): Key = {
        val sorted =
          headers.iterator.map(header => lowercaseName(header) -> header.renderedValue).toVector.sortBy(_._1)
        Key(new RequestBody(body), sorted)
      }
    }
  }

}
