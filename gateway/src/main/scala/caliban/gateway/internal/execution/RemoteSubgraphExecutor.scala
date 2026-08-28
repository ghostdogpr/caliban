package caliban.gateway.internal.execution

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, IncomingRequestHeaders, ResponseValue }
import caliban.gateway.{ GatewayWrapper, RemoteGraphQLConfig, SubscriptionTermination }
import caliban.gateway.GatewayWrapper.{ DeduplicationResult, Event, Outcome, Result }
import caliban.gateway.internal.AdmissionGate
import caliban.interop.jsoniter.BoundedOutputStream
import caliban.parsing.adt.OperationType
import caliban.ResponseValue.ObjectValue
import caliban.Value.NullValue
import com.github.plokhotnyuk.jsoniter_scala.core._
import sttp.capabilities.zio.ZioStreams
import sttp.client4._
import sttp.client4.httpclient.zio.SttpClient
import sttp.model.{ Header, Uri }
import zio._
import zio.stream.ZStream

import java.util.Arrays
import scala.collection.mutable
import scala.util.control.NonFatal

private[gateway] final class RemoteSubgraphExecutor[-R](
  name: String,
  endpoint: Uri,
  backend: SttpClient,
  config: RemoteGraphQLConfig[R],
  responseStructureLimits: RemoteSubgraphExecutor.ResponseStructureLimits,
  queryCalls: Option[RemoteSubgraphExecutor.InFlightQueryDeduplicator],
  admission: Option[AdmissionGate],
  wrapper: GatewayWrapper[R],
  fixedHeaders: Option[List[Header]] = None
) extends SubgraphExecutor[R] {
  import RemoteSubgraphExecutor._

  private val execution        = config.execution
  private val disclosure       = config.errorDisclosure.getOrElse(RemoteGraphQLConfig.ErrorDisclosure.default)
  private val staticHeaders    = sanitizeHeaders(execution.headers)
  private val forwardsIncoming = execution.forwardsAllIncomingHeaders || execution.forwardedHeaders.nonEmpty

  val errorPolicy: SubgraphExecutor.ErrorPolicy = SubgraphExecutor.ErrorPolicy.Remote

  private def headers(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, List[Header]] =
    fixedHeaders.fold[ZIO[R, SubgraphExecutor.Failure, List[Header]]](for {
      incoming  <- if (forwardsIncoming) IncomingRequestHeaders.get.map(_.map { case (key, value) =>
                     Header(key, value)
                   })
                   else ZIO.succeed(Nil)
      effectful <- config.effectfulHeaders.mapError(SubgraphExecutor.HeaderFailure(_))
      values    <- wrapper.outboundHeaders(name, outboundHeaders(incoming, effectful))
    } yield values)(ZIO.succeed(_))

  override def forSubscription(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, SubgraphExecutor[R]] =
    headers.map(values =>
      new RemoteSubgraphExecutor(
        name,
        endpoint,
        backend,
        config,
        responseStructureLimits,
        queryCalls,
        admission,
        wrapper,
        Some(values)
      )
    )

  override def subscribe(
    request: GraphQLRequest
  )(implicit trace: Trace): ZIO[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]] = {
    val open = for {
      values <- headers.mapError(_ => SubscriptionTermination.Source)
      traced <- wrapper.attemptHeaders(name, 0, values)
      body   <- ZIO
                  .fromEither(encode(request.copy(extensions = None)))
                  .mapError(_ => SubscriptionTermination.Source)
      stream <- RemoteSubscription.open(
                  endpoint,
                  backend,
                  config.subscription,
                  traced,
                  request,
                  body,
                  execution.maxResponseBytes,
                  bytes => decodeBody(bytes).map(_.copy(extensions = None)),
                  responseWithinLimits,
                  disclosure
                )
    } yield stream
    admission.fold(open)(
      _.observed[R with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]](wrapper)(open)
    )
  }

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] = {
    val logicalCall =
      for {
        body      <- ZIO.fromEither(encode(request.copy(extensions = None)))
        headers   <- this.headers
        replaySafe = execution.retries > 0 && operationType == OperationType.Query
        rawCall    = executeAttempts(body, headers, replaySafe, execution.retries, attempt = 0)
        admitted   = admission.fold(rawCall)(_.observed(wrapper)(rawCall))
        response  <- if (operationType == OperationType.Query)
                       queryCalls.fold(admitted)(
                         _.execute(QueryDeduplicationKey(body, headers), wrapper)(
                           admitted.timeoutFail(SubgraphExecutor.TimeoutFailure)(execution.timeout)
                         )
                       )
                     else admitted
      } yield response

    logicalCall.timeoutFail(SubgraphExecutor.TimeoutFailure)(execution.timeout)
  }

  override def admittedBy[R1 <: R](gate: AdmissionGate, observer: GatewayWrapper[R1]): SubgraphExecutor[R1] =
    new RemoteSubgraphExecutor(
      name,
      endpoint,
      backend,
      config,
      responseStructureLimits,
      queryCalls,
      Some(gate),
      observer,
      fixedHeaders
    )

  private def executeAttempts(
    body: Array[Byte],
    headers: List[Header],
    replaySafe: Boolean,
    retries: Int,
    attempt: Int
  )(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] = {
    val transport   = wrapper.attemptHeaders(name, attempt, headers).flatMap(send(body, _))
    val observed    =
      if (!wrapper.enabled) transport
      else
        wrapper.wrap(Event.Attempt(name, attempt, body.length.toLong, endpoint.host, endpoint.port))(transport)(
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
      if (attempt == 0 || !wrapper.enabled) sendAttempt
      else
        wrapper.wrap(Event.Retry(name, attempt))(sendAttempt)(
          Result.fromExit(_)(Result.fromResponse, failure => Result(SubgraphExecutor.failureOutcome(failure)))
        )

    call.catchAll { failure =>
      if (replaySafe && retries > 0 && retryable(failure))
        ZIO.sleep(execution.retryBackoff) *>
          executeAttempts(body, headers, replaySafe, retries - 1, attempt + 1)
      else ZIO.fail(failure)
    }
  }

  private def send(
    body: Array[Byte],
    headers: List[Header]
  )(implicit trace: Trace): ZIO[R, AttemptFailure, AttemptResponse] = {
    val request = headers.foldLeft(
      basicRequest
        .post(endpoint)
        .body(body)
    )((current, header) => current.header(header, DuplicateHeaderBehavior.Add))

    val response: ZIO[R, AttemptFailure, Response[BoundedBody]] = request
      .contentType("application/json; charset=utf-8")
      .header("Accept", "application/graphql-response+json, application/json;q=0.9")
      .followRedirects(false)
      .response(asStreamAlways(ZioStreams)(readBounded))
      .send(backend)
      .mapError(error => AttemptFailure(SubgraphExecutor.TransportFailure(error), None, None))

    response.flatMap { value =>
      val responseBytes = value.body.bytes.length.toLong
      ZIO
        .fromEither(decode(value))
        .mapBoth(
          AttemptFailure(_, Some(value.code.code), Some(responseBytes)),
          AttemptResponse(_, value.code.code, responseBytes)
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
      .flatMap(_.value.split(',').iterator)
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(RemoteGraphQLConfig.normalize)
      .toSet
    headers.filterNot(header =>
      RemoteGraphQLConfig.isProtocolHeader(header.name) || connectionDeclaredHeaderNames.contains(normalize(header))
    )
  }

  private def mergeHeaders(lower: List[Header], higher: List[Header]): List[Header] =
    if (higher.isEmpty) lower
    else {
      val overridden = higher.iterator.map(normalize).toSet
      lower.filterNot(header => overridden.contains(normalize(header))) ::: higher
    }

  private def normalize(header: Header): String =
    RemoteGraphQLConfig.normalize(header.name)

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

  private def readBounded(stream: ZStream[Any, Throwable, Byte])(implicit trace: Trace): Task[BoundedBody] =
    stream
      .take(execution.maxResponseBytes.toLong + 1L)
      .runCollect
      .map(bytes => BoundedBody(bytes.toArray, bytes.length > execution.maxResponseBytes))

  private def decode(
    response: Response[BoundedBody]
  ): Either[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
    if (response.body.limitExceeded) Left(SubgraphExecutor.ResponseTooLarge)
    else if (response.code.isRedirect) Left(SubgraphExecutor.RedirectResponse)
    else {
      val mediaType = response.contentType.map(_.takeWhile(_ != ';').trim.toLowerCase(java.util.Locale.ROOT))
      mediaType match {
        case Some("application/graphql-response+json") =>
          decodeBody(response.body.bytes) match {
            case result @ Right(_)                   => result
            case Left(_) if !response.code.isSuccess => Left(SubgraphExecutor.HttpFailure(response.code.code))
            case failure                             => failure
          }
        case _ if !response.code.isSuccess             => Left(SubgraphExecutor.HttpFailure(response.code.code))
        case Some("application/json")                  => decodeBody(response.body.bytes)
        case _                                         => Left(SubgraphExecutor.UnsupportedMediaType)
      }
    }

  private def decodeBody(bytes: Array[Byte]): Either[SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
    for {
      _        <- responseWithinLimits(bytes)
      envelope <-
        try Right(readFromArray[ResponseValue](bytes))
        catch {
          case NonFatal(_) => Left(SubgraphExecutor.InvalidResponse)
        }
      response <- envelope match {
                    case value: ObjectValue => decodeEnvelope(value).toRight(SubgraphExecutor.InvalidResponse)
                    case _                  => Left(SubgraphExecutor.InvalidResponse)
                  }
    } yield response

  private def responseWithinLimits(bytes: Array[Byte]): Either[SubgraphExecutor.Failure, Unit] = {
    val maxDepth  = responseStructureLimits.maxResponseDepth
    val maxTokens = responseStructureLimits.maxResponseTokens
    val length    = bytes.length
    var depth     = 0
    var index     = 0
    var escaped   = false
    var string    = false
    var tokens    = 0
    var previous  = 0.toByte

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
            if (depth > maxDepth) return Left(SubgraphExecutor.ResponseNestingTooDeep)
          case '}' | ']'                               =>
            depth -= 1
            previous = current
          case value if isScalarStart(value, previous) =>
            tokens += 1
            previous = current
          case ' ' | '\t' | '\n' | '\r'                => ()
          case _                                       => previous = current
        }

      if (tokens > maxTokens) return Left(SubgraphExecutor.ResponseStructureTooLarge)
      index += 1
    }

    Right(())
  }

  private def isScalarStart(value: Byte, previous: Byte): Boolean = {
    val scalar   = value == '-' || value >= '0' && value <= '9' || value == 't' || value == 'f' || value == 'n'
    val boundary = previous == 0 || previous == '[' || previous == ',' || previous == ':'
    scalar && boundary
  }

  private def decodeEnvelope(value: ObjectValue): Option[GraphQLResponse[CalibanError]] = {
    val data      = value.fields.collectFirst { case ("data", value) => value }
    val hasErrors = value.fields.exists(_._1 == "errors")
    val validData = data.forall {
      case _: ObjectValue => true
      case NullValue      => true
      case _              => false
    }

    GraphQLResponse
      .fromResponseValue(value)
      .filter(response =>
        (data.nonEmpty || hasErrors) && validData && (!hasErrors || response.errors.nonEmpty) &&
          !(data.contains(NullValue) && response.errors.isEmpty) && response.hasNext.isEmpty
      )
      .map(response => response.copy(errors = response.errors.map(RemoteError.disclose(_, disclosure))))
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
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteGraphQLConfig[R],
    wrapper: GatewayWrapper[R]
  )(implicit trace: Trace): ZIO[Scope, Nothing, RemoteSubgraphExecutor[R]] =
    Scope.make.flatMap { deduplicationScope =>
      val deduplicator =
        if (config.execution.inFlightQueryDeduplication)
          InFlightQueryDeduplicator.make(deduplicationScope, config.execution.maxConcurrentCalls).map(Some(_))
        else ZIO.none
      ZIO.addFinalizer(deduplicationScope.close(Exit.unit)) *>
        deduplicator.map(
          new RemoteSubgraphExecutor(name, endpoint, backend, config, ResponseStructureLimits.default, _, None, wrapper)
        )
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
    headers: Vector[(String, Vector[String])]
  )

  private object QueryDeduplicationKey {
    def apply(body: Array[Byte], headers: List[Header]): QueryDeduplicationKey = {
      val grouped = mutable.HashMap.empty[String, Vector[String]]
      headers.foreach { header =>
        val name = RemoteGraphQLConfig.normalize(header.name)
        grouped.update(name, grouped.getOrElse(name, Vector.empty) :+ header.value)
      }
      QueryDeduplicationKey(new RequestBody(body), grouped.toVector.sortBy(_._1))
    }
  }

  private final case class BoundedBody(bytes: Array[Byte], limitExceeded: Boolean)

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
    def execute[R](key: QueryDeduplicationKey, wrapper: GatewayWrapper[R])(
      call: => ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]]
    )(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
      ZIO.uninterruptible(loop(key, wrapper, call))

    private def loop[R](
      key: QueryDeduplicationKey,
      wrapper: GatewayWrapper[R],
      call: => ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]]
    )(implicit trace: Trace): ZIO[R, SubgraphExecutor.Failure, GraphQLResponse[CalibanError]] =
      state.get.flatMap { current =>
        current.entries.get(key) match {
          case Some(existing) =>
            observe(DeduplicationResult.Join, wrapper)(await(existing).interruptible)
          case None           =>
            Promise.make[Nothing, QueryCallExit].flatMap { candidate =>
              decide(key, candidate).flatMap {
                case QueryCallDecision.Start          =>
                  observe(DeduplicationResult.Start, wrapper)(
                    complete(key, candidate, call).interruptible.forkIn(scope) *> await(candidate).interruptible
                  )
                case QueryCallDecision.Join(existing) =>
                  observe(DeduplicationResult.Join, wrapper)(await(existing).interruptible)
                case QueryCallDecision.Wait(signal)   =>
                  observe(DeduplicationResult.Wait, wrapper)(signal.await.interruptible *> loop(key, wrapper, call))
              }
            }
        }
      }

    private def observe[R, E, A](value: DeduplicationResult, wrapper: GatewayWrapper[R])(effect: ZIO[R, E, A])(implicit
      trace: Trace
    ): ZIO[R, E, A] =
      if (!wrapper.enabled) effect
      else wrapper.wrap(Event.Deduplication(value))(effect)(Result.classifyExit)

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
