package caliban.gateway.internal.execution

import caliban.ResponseValue.ObjectValue
import caliban.Value.NullValue
import caliban.gateway.PhaseHooks.{ Event, Outcome, Result }
import caliban.gateway.internal.{ GatewayHttpClient, SubscriptionTermination }
import caliban.gateway.{ PhaseHooks, RemoteGraphQLConfig }
import caliban.interop.jsoniter.BoundedOutputStream
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
  responseStructureLimits: RemoteSubgraphExecutor.ResponseStructureLimits,
  deduplicator: Option[RemoteSubgraphExecutor.QueryDeduplicator],
  hooks: PhaseHooks[R],
  remoteErrorMessages: Boolean = false,
  fixedHeaders: Option[List[Header]] = None
) extends SubgraphExecutor[R] {
  import RemoteSubgraphExecutor._
  import RemoteTransport._

  val errorPolicy: SubgraphExecutor.ErrorPolicy = SubgraphExecutor.ErrorPolicy.Remote

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] = {
    val logicalCall =
      for {
        body      <- encode(request.copy(extensions = None))
        headers   <- resolveHeaders
        replaySafe = operationType == OperationType.Query
        rawCall    = executeAttempts(body, headers, replaySafe, attempt = 0)
        response  <- if (replaySafe)
                       deduplicator.fold(rawCall)(
                         _.execute(body, headers)(
                           rawCall.timeoutFail(SubgraphExecutor.TimeoutFailure)(execution.timeout)
                         )
                       )
                     else rawCall
      } yield response

    logicalCall.timeoutFail(SubgraphExecutor.TimeoutFailure)(execution.timeout)
  }

  override def forSubscription(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, SubgraphExecutor[R]] =
    resolveHeaders.map(withHeaders)

  override def subscribe(
    request: GraphQLRequest
  )(implicit trace: Trace): ZIO[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]] =
    for {
      values <- resolveHeaders.mapError(_ => SubscriptionTermination.Source)
      traced <- if (!hooks.attemptHeaders.enabled) Exit.succeed(values)
                else
                  hooks.attemptHeaders.runWith(Event.AttemptHeaders(name, 0, values))(event =>
                    Exit.succeed(event.headers)
                  )((_: Exit[Nothing, List[Header]]) => ())
      body   <- encode(request.copy(extensions = None)).mapError(_ => SubscriptionTermination.Source)
      stream <- subscription.open(traced, request, body)
    } yield stream

  private val execution        = config.execution
  private val staticHeaders    = sanitizeHeaders(execution.headers)
  private val forwardsIncoming = execution.forwardsAllIncomingHeaders || execution.forwardedHeaders.nonEmpty
  private val subscription     = new RemoteSubscription(
    endpoint,
    http,
    config.subscription,
    execution.maxResponseBytes,
    bytes => decodeBody(bytes).map(_.copy(extensions = None)),
    value => decodeValue(value).map(_.copy(extensions = None)),
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
          headers   <- if (!hooks.outboundHeaders.enabled) Exit.succeed(outboundHeaders(incoming, effectful))
                       else
                         hooks.outboundHeaders.runWith(
                           Event.OutboundHeaders(name, outboundHeaders(incoming, effectful))
                         )(event => Exit.succeed(event.headers))((_: Exit[Nothing, List[Header]]) => ())
        } yield headers
    }

  private def withHeaders(headers: List[Header]): RemoteSubgraphExecutor[R] =
    new RemoteSubgraphExecutor(
      name,
      endpoint,
      http,
      config,
      responseStructureLimits,
      deduplicator,
      hooks,
      remoteErrorMessages,
      Some(headers)
    )

  private def executeAttempts(body: Array[Byte], headers: List[Header], replaySafe: Boolean, attempt: Int)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] = {
    val transport   =
      if (!hooks.attemptHeaders.enabled) send(body, headers)
      else
        hooks.attemptHeaders.runWith(Event.AttemptHeaders(name, attempt, headers))(event => send(body, event.headers))(
          _ => ()
        )
    val observed    =
      hooks.attempt.run(Event.Attempt(name, attempt, body.length.toLong, endpoint.host, endpoint.port))(transport)(
        Result.fromExit(_)(
          value =>
            Result(
              Outcome.fromResponse(value.response),
              errorCount = value.response.errors.size,
              statusCode = Some(value.statusCode),
              responseBytes = Some(value.responseBytes)
            ),
          failure =>
            Result(
              SubgraphExecutor.failureOutcome(failure.failure),
              statusCode = failure.statusCode,
              responseBytes = failure.responseBytes
            )
        )
      )
    val sendAttempt = observed.map(_.response).mapError(_.failure)
    val call        =
      if (attempt == 0) sendAttempt
      else
        hooks.retry.run(Event.Retry(name, attempt))(sendAttempt)(SubgraphExecutor.resultFromExit)

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
      val responseBytes = value.body.bytes.length.toLong
      ZIO
        .fromEither(decode(value))
        .mapBoth(
          AttemptFailure(_, Some(value.status.code), Some(responseBytes)),
          AttemptResponse(_, value.status.code, responseBytes)
        )
    }
  }

  private def outboundHeaders(incoming: List[Header], effectful: List[Header]): List[Header] = {
    val forwarded = sanitizeHeaders(incoming).filter { header =>
      execution.forwardsAllIncomingHeaders || execution.forwardedHeaders.contains(lowercaseName(header))
    }
    mergeHeaders(mergeHeaders(forwarded, staticHeaders), sanitizeHeaders(effectful))
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
    ZIO.suspendSucceed {
      val output = new BoundedOutputStream(execution.maxRequestBytes)
      ZIO.attempt {
        writeToStream(request, output)
        output.toByteArray
      }.refineOrDie {
        case BoundedOutputStream.LimitExceeded => SubgraphExecutor.RequestTooLarge
        case NonFatal(_)                       => SubgraphExecutor.InvalidRequest
      }
    }

  private def decode(
    response: GatewayHttpClient.Reply
  ): Either[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
    if (response.body.limitExceeded) Left(SubgraphExecutor.ResponseTooLarge)
    else if (response.status.isRedirection) Left(SubgraphExecutor.RedirectResponse)
    else {
      RemoteTransport.mediaType(response.contentType) match {
        case Some("application/graphql-response+json") =>
          decodeBody(response.body.bytes) match {
            case result @ Right(_)                     => result
            case Left(_) if !response.status.isSuccess => Left(SubgraphExecutor.HttpFailure(response.status.code))
            case failure                               => failure
          }
        case _ if !response.status.isSuccess           => Left(SubgraphExecutor.HttpFailure(response.status.code))
        case Some("application/json")                  => decodeBody(response.body.bytes)
        case _                                         => Left(SubgraphExecutor.UnsupportedMediaType)
      }
    }

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
    validateJsonStructure(
      bytes,
      responseStructureLimits.maxResponseDepth,
      responseStructureLimits.maxResponseTokens
    ).left.map {
      case RemoteTransport.JsonDepthExceeded  => SubgraphExecutor.ResponseNestingTooDeep
      case RemoteTransport.JsonTokensExceeded => SubgraphExecutor.ResponseStructureTooLarge
    }

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
    Scope.make.flatMap { deduplicationScope =>
      val deduplicator =
        if (config.execution.inFlightQueryDeduplication)
          QueryDeduplicator.make(deduplicationScope).map(Some(_))
        else ZIO.none
      ZIO.addFinalizer(deduplicationScope.close(Exit.unit)) *>
        deduplicator.map { deduplicator =>
          new RemoteSubgraphExecutor(
            name,
            endpoint,
            http,
            config,
            ResponseStructureLimits.default,
            deduplicator,
            hooks,
            remoteErrorMessages
          )
        }
    }

  final case class ResponseStructureLimits(maxResponseDepth: Int, maxResponseTokens: Int)

  object ResponseStructureLimits {
    val default: ResponseStructureLimits = ResponseStructureLimits(maxResponseDepth = 128, maxResponseTokens = 250000)
  }

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

  private final class RequestBody(private val bytes: Array[Byte]) {
    private val hash = Arrays.hashCode(bytes)

    override def hashCode(): Int = hash

    override def equals(other: Any): Boolean =
      other match {
        case that: RequestBody => hash == that.hash && Arrays.equals(bytes, that.bytes)
        case _                 => false
      }
  }

  private[internal] final class QueryDeduplicator private (
    scope: Scope,
    calls: Ref[Map[QueryDeduplicator.Key, QueryDeduplicator.CallPromise]]
  ) {
    import QueryDeduplicator._

    def execute[R](body: Array[Byte], headers: List[Header])(
      call: => ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]]
    )(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] = ZIO.uninterruptible {
      val key = Key(body, headers)
      calls.get.flatMap { current =>
        current.get(key) match {
          case Some(existing) =>
            await(existing).interruptible
          case None           =>
            Promise.make[Nothing, CallExit].flatMap { candidate =>
              register(key, candidate).flatMap {
                case None           =>
                  // Shared work belongs to the executor scope, so one waiter cannot cancel it for the others.
                  complete(key, candidate, call).interruptible.forkIn(scope) *> await(candidate).interruptible
                case Some(existing) =>
                  await(existing).interruptible
              }
            }
        }
      }
    }

    private def register(key: Key, candidate: CallPromise): UIO[Option[CallPromise]] =
      calls.modify { current =>
        current.get(key) match {
          case existing @ Some(_) => existing -> current
          case None               => None     -> current.updated(key, candidate)
        }
      }

    private def complete[R](
      key: Key,
      promise: CallPromise,
      call: => ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]]
    )(implicit trace: Trace): URIO[R, Unit] =
      ZIO.uninterruptibleMask { restore =>
        restore(call).exit.flatMap { exit =>
          calls.update(_ - key) *> promise.succeed(exit).unit
        }
      }

    private def await(promise: CallPromise)(implicit
      trace: Trace
    ): IO[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
      promise.await.flatMap(ZIO.suspendSucceed(_))
  }

  private object QueryDeduplicator {
    def make(scope: Scope)(implicit trace: Trace): UIO[QueryDeduplicator] =
      Ref.make(Map.empty[Key, CallPromise]).map(new QueryDeduplicator(scope, _))

    final case class Key(body: RequestBody, headers: Vector[(String, String)])

    object Key {
      def apply(body: Array[Byte], headers: List[Header]): Key = {
        val sorted =
          headers.iterator.map(header => lowercaseName(header) -> header.renderedValue).toVector.sortBy(_._1)
        Key(new RequestBody(body), sorted)
      }
    }

    type CallExit    = Exit[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]]
    type CallPromise = Promise[Nothing, CallExit]

  }

}
