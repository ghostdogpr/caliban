package caliban.gateway.internal.execution

import caliban.ResponseValue.ObjectValue
import caliban.Value.NullValue
import caliban.gateway.PhaseHooks.{ Event, Outcome, Result }
import caliban.gateway.internal.{ AdmissionGate, GatewayHttpClient, SubscriptionTermination }
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

private[gateway] object RemoteTransport {
  final case class BoundedBody(bytes: Array[Byte], limitExceeded: Boolean)

  sealed trait JsonStructureLimit
  case object JsonDepthExceeded  extends JsonStructureLimit
  case object JsonTokensExceeded extends JsonStructureLimit

  def mediaType(contentType: Option[String]): Option[String] =
    contentType.map(_.takeWhile(_ != ';').trim.toLowerCase(java.util.Locale.ROOT))

  def validateJsonStructure(
    bytes: Array[Byte],
    maxDepth: Int,
    maxTokens: Int
  ): Either[JsonStructureLimit, Unit] = {
    val length   = bytes.length
    var depth    = 0
    var index    = 0
    var escaped  = false
    var string   = false
    var tokens   = 0
    var previous = 0.toByte

    while (index < length) {
      val current = bytes(index)
      if (string) {
        if (escaped) escaped = false
        else if (current == '\\') escaped = true
        else if (current == '"') {
          string = false
          previous = current
        }
      } else
        current match {
          case '"'                                     =>
            string = true
            tokens += 1
          case '{' | '['                               =>
            depth += 1
            tokens += 1
            previous = current
            if (depth > maxDepth) return Left(JsonDepthExceeded)
          case '}' | ']'                               =>
            depth -= 1
            previous = current
          case value if isScalarStart(value, previous) =>
            tokens += 1
            previous = current
          case ' ' | '\t' | '\n' | '\r'                => ()
          case _                                       => previous = current
        }

      if (tokens > maxTokens) return Left(JsonTokensExceeded)
      index += 1
    }

    Right(())
  }

  private def isScalarStart(value: Byte, previous: Byte): Boolean = {
    val scalar   = value == '-' || value >= '0' && value <= '9' || value == 't' || value == 'f' || value == 'n'
    val boundary = previous == 0 || previous == '[' || previous == ',' || previous == ':'
    scalar && boundary
  }
}

private[gateway] final class RemoteSubgraphExecutor[-R](
  name: String,
  endpoint: URL,
  http: GatewayHttpClient,
  config: RemoteGraphQLConfig[R],
  responseStructureLimits: RemoteSubgraphExecutor.ResponseStructureLimits,
  queryCalls: Option[RemoteSubgraphExecutor.InFlightQueryDeduplicator],
  admission: Option[AdmissionGate[R]],
  hooks: PhaseHooks[R],
  remoteErrorMessages: Boolean = false,
  fixedHeaders: Option[List[Header]] = None
) extends SubgraphExecutor[R] {
  import RemoteSubgraphExecutor._
  import RemoteTransport._

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
    responseWithinLimits,
    remoteErrorMessages
  )

  val errorPolicy: SubgraphExecutor.ErrorPolicy = SubgraphExecutor.ErrorPolicy.Remote

  private def headers(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, List[Header]] =
    fixedHeaders.fold[ZIO[R, SubgraphExecutor.Failure, List[Header]]](for {
      incoming  <- if (forwardsIncoming) IncomingRequestHeaders.get.map(_.map { case (key, value) =>
                     Header.Custom(key, value)
                   })
                   else ZIO.succeed(List.empty[Header])
      effectful <- config.effectfulHeaders.mapError(SubgraphExecutor.HeaderFailure(_))
      headers   <- if (!hooks.outboundHeaders.enabled) Exit.succeed(outboundHeaders(incoming, effectful))
                   else
                     hooks.outboundHeaders.runWith(Event.OutboundHeaders(name, outboundHeaders(incoming, effectful)))(
                       ev => Exit.succeed(ev.headers)
                     )((_: Exit[Nothing, List[Header]]) => ())
    } yield headers)(ZIO.succeed(_))

  override def forSubscription(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, SubgraphExecutor[R]] =
    headers.map(values => copied(admission, hooks, Some(values)))

  override def subscribe(
    request: GraphQLRequest
  )(implicit trace: Trace): ZIO[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]] = {
    val open = for {
      values <- headers.mapError(_ => SubscriptionTermination.Source)
      traced <-
        hooks.attemptHeaders.runWith(Event.AttemptHeaders(name, 0, values))(Exit.succeed)((_: Exit[Nothing, Any]) => ())
      body   <- ZIO
                  .fromEither(encode(request.copy(extensions = None)))
                  .mapError(_ => SubscriptionTermination.Source)
      stream <- subscription.open(traced.headers, request, body)
    } yield stream
    admission.fold(open)(_.observed(open))
  }

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] = {
    val logicalCall =
      for {
        body      <- ZIO.fromEither(encode(request.copy(extensions = None)))
        headers   <- this.headers
        replaySafe = operationType == OperationType.Query
        rawCall    = executeAttempts(body, headers, replaySafe, attempt = 0)
        admitted   = admission.fold(rawCall)(_.observed(rawCall))
        response  <- if (replaySafe)
                       queryCalls.fold(admitted)(
                         _.execute(QueryDeduplicationKey(body, headers))(
                           admitted.timeoutFail(SubgraphExecutor.TimeoutFailure)(execution.timeout)
                         )
                       )
                     else admitted
      } yield response

    logicalCall.timeoutFail(SubgraphExecutor.TimeoutFailure)(execution.timeout)
  }

  private def copied[R1 <: R](
    admission: Option[AdmissionGate[R1]],
    hooks: PhaseHooks[R1],
    fixedHeaders: Option[List[Header]]
  ): RemoteSubgraphExecutor[R1] =
    new RemoteSubgraphExecutor(
      name,
      endpoint,
      http,
      config,
      responseStructureLimits,
      queryCalls,
      admission,
      hooks,
      remoteErrorMessages,
      fixedHeaders
    )

  private def executeAttempts(
    body: Array[Byte],
    headers: List[Header],
    replaySafe: Boolean,
    attempt: Int
  )(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] = {
    val transport   =
      hooks.attemptHeaders.runWith(Event.AttemptHeaders(name, attempt, headers))(ev => send(body, ev.headers))(_ => ())
    val observed    =
      if (!hooks.attempt.enabled) transport
      else
        hooks.attempt.run(Event.Attempt(name, attempt, body.length.toLong, endpoint.host, endpoint.port))(transport)(
          Result.fromExit(_)(
            value =>
              Result(
                if (value.response.errors.isEmpty) Outcome.Success else Outcome.GraphQLError,
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
      if (attempt == 0 || !hooks.retry.enabled) sendAttempt
      else
        hooks.retry.run(Event.Retry(name, attempt))(sendAttempt)(
          SubgraphExecutor.resultFromExit
        )

    call.catchAll { failure =>
      if (replaySafe && attempt < execution.retries && retryable(failure))
        ZIO.sleep(execution.retryBackoff) *>
          executeAttempts(body, headers, replaySafe, attempt + 1)
      else ZIO.fail(failure)
    }
  }

  private def send(
    body: Array[Byte],
    headers: List[Header]
  )(implicit trace: Trace): ZIO[R, AttemptFailure, AttemptResponse] = {
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
      execution.forwardsAllIncomingHeaders || execution.forwardedHeaders.contains(normalize(header))
    }
    mergeHeaders(mergeHeaders(forwarded, staticHeaders), sanitizeHeaders(effectful))
  }

  private def sanitizeHeaders(headers: List[Header]): List[Header] = {
    val connectionDeclaredHeaderNames = headers.iterator
      .filter(header => normalize(header) == "connection")
      .flatMap(_.renderedValue.split(',').iterator)
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(RemoteGraphQLConfig.normalize)
      .toSet
    headers.filterNot(header =>
      RemoteGraphQLConfig.isProtocolHeader(header.headerName) || connectionDeclaredHeaderNames(normalize(header))
    )
  }

  private def mergeHeaders(lower: List[Header], higher: List[Header]): List[Header] =
    if (higher.isEmpty) lower
    else {
      val overridden = higher.iterator.map(normalize).toSet
      lower.filterNot(header => overridden.contains(normalize(header))) ::: higher
    }

  private def normalize(header: Header): String =
    RemoteGraphQLConfig.normalize(header.headerName)

  private def retryable(failure: SubgraphExecutor.Failure): Boolean =
    failure match {
      case SubgraphExecutor.TransportFailure(_)          => true
      case SubgraphExecutor.HttpFailure(502 | 503 | 504) => true
      case _                                             => false
    }

  private def encode(request: GraphQLRequest): Either[SubgraphExecutor.Failure, Array[Byte]] = {
    val output = new BoundedOutputStream(execution.maxRequestBytes)
    try {
      writeToStream(request, output)
      Right(output.toByteArray)
    } catch {
      case BoundedOutputStream.LimitExceeded => Left(SubgraphExecutor.RequestTooLarge)
      case NonFatal(_)                       => Left(SubgraphExecutor.InvalidRequest)
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
      _         <- responseWithinLimits(bytes)
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

  private def responseWithinLimits(bytes: Array[Byte]): Either[SubgraphExecutor.Failure, Unit] =
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
      response.copy(errors = response.errors.map(RemoteError.disclose(_, remoteErrorMessages))),
      SubgraphExecutor.InvalidResponse
    )
  }
}

private[gateway] object RemoteSubgraphExecutor {

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

  def make[R](
    name: String,
    endpoint: URL,
    http: GatewayHttpClient,
    config: RemoteGraphQLConfig[R],
    hooks: PhaseHooks[R],
    remoteErrorMessages: Boolean = false,
    admission: Option[AdmissionGate[R]] = None
  )(implicit trace: Trace): ZIO[Scope, Nothing, RemoteSubgraphExecutor[R]] =
    Scope.make.flatMap { deduplicationScope =>
      val deduplicator =
        if (config.execution.inFlightQueryDeduplication)
          InFlightQueryDeduplicator.make(deduplicationScope, config.execution.maxConcurrentCalls).map(Some(_))
        else ZIO.none
      ZIO.addFinalizer(deduplicationScope.close(Exit.unit)) *>
        deduplicator
          .zip(
            admission.fold(
              AdmissionGate.make(
                config.execution.maxConcurrentCalls,
                PhaseHooks.AdmissionKind.Subgraph,
                hooks
              )
            )(ZIO.succeed(_))
          )
          .map { case (calls, admission) =>
            new RemoteSubgraphExecutor(
              name,
              endpoint,
              http,
              config,
              ResponseStructureLimits.default,
              calls,
              Some(admission),
              hooks,
              remoteErrorMessages
            )
          }
    }

  final case class ResponseStructureLimits(
    maxResponseDepth: Int,
    maxResponseTokens: Int
  )

  object ResponseStructureLimits {
    val default: ResponseStructureLimits = ResponseStructureLimits(
      maxResponseDepth = 128,
      maxResponseTokens = 250000
    )
  }

  private[internal] final case class QueryDeduplicationKey(
    body: RequestBody,
    headers: Vector[(String, String)]
  )

  private object QueryDeduplicationKey {
    def apply(body: Array[Byte], headers: List[Header]): QueryDeduplicationKey = {
      val sorted = headers.iterator
        .map(header => RemoteGraphQLConfig.normalize(header.headerName) -> header.renderedValue)
        .toVector
        .sortBy(_._1)
      QueryDeduplicationKey(new RequestBody(body), sorted)
    }
  }

  private[internal] final class RequestBody(private val bytes: Array[Byte]) {
    private val hash = Arrays.hashCode(bytes)

    override def hashCode(): Int = hash

    override def equals(other: Any): Boolean =
      other match {
        case that: RequestBody => hash == that.hash && Arrays.equals(bytes, that.bytes)
        case _                 => false
      }
  }

  private[internal] final class InFlightQueryDeduplicator private (
    scope: Scope,
    state: Ref[QueryCallState]
  ) {
    def execute[R](key: QueryDeduplicationKey)(
      call: => ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]]
    )(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
      ZIO.uninterruptible(loop(key, call))

    private def loop[R](
      key: QueryDeduplicationKey,
      call: => ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]]
    )(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
      state.get.flatMap { current =>
        current.entries.get(key) match {
          case Some(existing) =>
            await(existing).interruptible
          case None           =>
            Promise.make[Nothing, QueryCallExit].flatMap { candidate =>
              decide(key, candidate).flatMap {
                case QueryCallDecision.Start          =>
                  complete(key, candidate, call).interruptible.forkIn(scope) *> await(candidate).interruptible
                case QueryCallDecision.Join(existing) =>
                  await(existing).interruptible
                case QueryCallDecision.Wait(signal)   =>
                  signal.await.interruptible *> loop(key, call)
              }
            }
        }
      }

    private def decide(key: QueryDeduplicationKey, candidate: QueryCallPromise): UIO[QueryCallDecision] =
      state.modify { current =>
        current.entries.get(key) match {
          case Some(existing)                               =>
            QueryCallDecision.Join(existing) -> current
          case None if current.entries.size < current.limit =>
            QueryCallDecision.Start -> current.copy(entries = current.entries.updated(key, candidate))
          case None                                         =>
            QueryCallDecision.Wait(current.space) -> current
        }
      }

    private def complete[R](
      key: QueryDeduplicationKey,
      promise: QueryCallPromise,
      call: => ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]]
    )(implicit trace: Trace): URIO[R, Unit] =
      ZIO.uninterruptibleMask { restore =>
        restore(call).exit.flatMap { exit =>
          Promise.make[Nothing, Unit].flatMap { nextSpace =>
            state.modify { current =>
              current.space -> current.copy(entries = current.entries - key, space = nextSpace)
            }.flatMap(_.succeed(())) *>
              promise.succeed(exit).unit
          }
        }
      }

    private def await(
      promise: QueryCallPromise
    )(implicit trace: Trace): IO[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
      promise.await.flatMap(ZIO.suspendSucceed(_))
  }

  private type QueryCallExit    = Exit[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]]
  private type QueryCallPromise = Promise[Nothing, QueryCallExit]

  private sealed trait QueryCallDecision
  private object QueryCallDecision {
    final case class Join(existing: QueryCallPromise)     extends QueryCallDecision
    final case class Wait(signal: Promise[Nothing, Unit]) extends QueryCallDecision
    case object Start                                     extends QueryCallDecision
  }

  private final case class QueryCallState(
    entries: Map[QueryDeduplicationKey, QueryCallPromise],
    limit: Int,
    space: Promise[Nothing, Unit]
  )

  private object InFlightQueryDeduplicator {
    def make(scope: Scope, limit: Int)(implicit trace: Trace): UIO[InFlightQueryDeduplicator] =
      for {
        space <- Promise.make[Nothing, Unit]
        state <- Ref.make(QueryCallState(Map.empty, limit, space))
      } yield new InFlightQueryDeduplicator(scope, state)
  }

}
