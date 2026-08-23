package caliban.gateway.internal

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.NullValue
import caliban.gateway.RemoteGraphQLConfig
import caliban.parsing.adt.OperationType
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, IncomingRequestHeaders, ResponseValue }
import com.github.plokhotnyuk.jsoniter_scala.core._
import sttp.capabilities.zio.ZioStreams
import sttp.client4._
import sttp.client4.httpclient.zio.SttpClient
import sttp.model.{ Header, Uri }
import zio._
import zio.stream.ZStream

import java.io.{ ByteArrayOutputStream, OutputStream }
import java.util.Arrays
import scala.collection.mutable
import scala.util.control.{ NoStackTrace, NonFatal }

private[gateway] final class RemoteGraphQLSource[-R](
  endpoint: Uri,
  backend: SttpClient,
  config: RemoteGraphQLConfig[R],
  structuralLimits: RemoteGraphQLSource.StructuralLimits,
  queryCalls: Option[RemoteGraphQLSource.InFlightQueryDeduplicator],
  admission: Option[ExecutionGate]
) extends GraphQLSource[R] {
  import RemoteGraphQLSource._

  private val execution  = config.execution
  private val disclosure = config.errorDisclosure.getOrElse(RemoteGraphQLConfig.ErrorDisclosure.default)

  val errorPolicy: GraphQLSource.ErrorPolicy = GraphQLSource.ErrorPolicy.Remote

  def execute(request: GraphQLRequest, operationType: OperationType)(implicit
    trace: Trace
  ): ZIO[R, GraphQLSource.Failure, GraphQLResponse[CalibanError]] = {
    val logicalCall =
      for {
        body      <- ZIO.fromEither(encode(request.copy(extensions = None)))
        incoming  <- IncomingRequestHeaders.get.map(_.map { case (name, value) => Header(name, value) })
        effectful <- config.effectfulHeaders.mapError(_ => GraphQLSource.HeaderFailure)
        headers    = outboundHeaders(incoming, effectful)
        replaySafe = execution.retries > 0 && operationType == OperationType.Query
        rawCall    = executeAttempts(body, headers, replaySafe, execution.retries)
        response  <-
          if (operationType == OperationType.Query)
            queryCalls.fold(admission.fold(rawCall)(_(rawCall)))(
              _.execute(QueryCallIdentity(body, headers), admission)(
                rawCall.timeoutFail(GraphQLSource.TimeoutFailure)(execution.timeout)
              )
            )
          else admission.fold(rawCall)(_(rawCall))
      } yield response

    logicalCall.timeoutFail(GraphQLSource.TimeoutFailure)(execution.timeout)
  }

  override def admittedBy(gate: ExecutionGate): GraphQLSource[R] =
    new RemoteGraphQLSource(endpoint, backend, config, structuralLimits, queryCalls, Some(gate))

  private def executeAttempts(
    body: Array[Byte],
    headers: List[Header],
    replaySafe: Boolean,
    retries: Int
  )(implicit trace: Trace): IO[GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
    send(body, headers).catchAll { failure =>
      if (replaySafe && retries > 0 && retryable(failure))
        ZIO.sleep(execution.retryBackoff) *> executeAttempts(body, headers, replaySafe, retries - 1)
      else ZIO.fail(failure)
    }

  private def send(
    body: Array[Byte],
    headers: List[Header]
  )(implicit trace: Trace): IO[GraphQLSource.Failure, GraphQLResponse[CalibanError]] = {
    val request = headers.foldLeft(
      basicRequest
        .post(endpoint)
        .body(body)
    )((current, header) => current.header(header, DuplicateHeaderBehavior.Add))

    request
      .contentType("application/json; charset=utf-8")
      .header("Accept", "application/graphql-response+json, application/json;q=0.9")
      .followRedirects(false)
      .response(asStreamAlways(ZioStreams)(readBounded))
      .send(backend)
      .mapError(_ => GraphQLSource.TransportFailure)
      .flatMap(response => ZIO.fromEither(decode(response)))
  }

  private def outboundHeaders(incoming: List[Header], effectful: List[Header]): List[Header] = {
    val connectionDeclaredHeaderNames = (incoming ::: execution.headers ::: effectful).iterator
      .filter(header => normalize(header) == "connection")
      .flatMap(_.value.split(',').iterator)
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(RemoteGraphQLConfig.normalize)
      .toSet

    val forwarded = incoming.filter { header =>
      execution.forwardAll || execution.forwardedHeaders.contains(normalize(header))
    }
    mergeHeaders(mergeHeaders(mergeHeaders(Nil, forwarded), execution.headers), effectful)
      .filterNot(header =>
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

  private def retryable(failure: GraphQLSource.Failure): Boolean =
    failure match {
      case GraphQLSource.TransportFailure             => true
      case GraphQLSource.HttpFailure(502 | 503 | 504) => true
      case _                                          => false
    }

  private def encode(request: GraphQLRequest): Either[GraphQLSource.Failure, Array[Byte]] = {
    val output = new BoundedOutputStream(execution.maxRequestBytes)
    try {
      writeToStream(request, output)
      Right(output.toByteArray)
    } catch {
      case RequestLimitExceeded => Left(GraphQLSource.RequestTooLarge)
      case NonFatal(_)          => Left(GraphQLSource.InvalidRequest)
    }
  }

  private def readBounded(stream: ZStream[Any, Throwable, Byte])(implicit trace: Trace): Task[BoundedBody] =
    stream
      .take(execution.maxResponseBytes.toLong + 1L)
      .runCollect
      .map(bytes => BoundedBody(bytes.toArray, bytes.length > execution.maxResponseBytes))

  private def decode(
    response: Response[BoundedBody]
  ): Either[GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
    if (response.body.limitExceeded) Left(GraphQLSource.ResponseTooLarge)
    else if (response.code.isRedirect) Left(GraphQLSource.RedirectResponse)
    else {
      val mediaType = response.contentType.map(_.takeWhile(_ != ';').trim.toLowerCase(java.util.Locale.ROOT))
      mediaType match {
        case Some("application/graphql-response+json") =>
          decodeBody(response.body.bytes) match {
            case result @ Right(_)                   => result
            case Left(_) if !response.code.isSuccess => Left(GraphQLSource.HttpFailure(response.code.code))
            case failure                             => failure
          }
        case _ if !response.code.isSuccess             => Left(GraphQLSource.HttpFailure(response.code.code))
        case Some("application/json")                  => decodeBody(response.body.bytes)
        case _                                         => Left(GraphQLSource.UnsupportedMediaType)
      }
    }

  private def decodeBody(bytes: Array[Byte]): Either[GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
    for {
      _        <- responseWithinLimits(bytes)
      envelope <-
        try Right(readFromArray[ResponseValue](bytes))
        catch {
          case NonFatal(_) => Left(GraphQLSource.InvalidResponse)
        }
      response <- envelope match {
                    case value: ObjectValue => decodeEnvelope(value).toRight(GraphQLSource.InvalidResponse)
                    case _                  => Left(GraphQLSource.InvalidResponse)
                  }
    } yield response

  private def responseWithinLimits(bytes: Array[Byte]): Either[GraphQLSource.Failure, Unit] = {
    val maxDepth  = structuralLimits.maxResponseDepth
    val maxTokens = structuralLimits.maxResponseTokens
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
            if (depth > maxDepth) return Left(GraphQLSource.ResponseNestingTooDeep)
          case '}' | ']'                               =>
            depth -= 1
            previous = current
          case value if isScalarStart(value, previous) =>
            tokens += 1
            previous = current
          case ' ' | '\t' | '\n' | '\r'                => ()
          case _                                       => previous = current
        }

      if (tokens > maxTokens) return Left(GraphQLSource.ResponseStructureTooLarge)
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
    val data            = value.fields.collectFirst { case ("data", value) => value }
    val errors          = value.fields.collectFirst { case ("errors", value) => value }
    val extensions      = value.fields.collectFirst { case ("extensions", value) => value }
    val validData       = data.forall {
      case _: ObjectValue => true
      case NullValue      => true
      case _              => false
    }
    val validErrors     = errors.forall {
      case ListValue(values) => values.nonEmpty
      case _                 => false
    }
    val validExtensions = extensions.forall(_.isInstanceOf[ObjectValue])

    if (
      (data.nonEmpty || errors.nonEmpty) && validData && validErrors && validExtensions &&
      !(data.contains(NullValue) && errors.isEmpty)
    )
      GraphQLResponse
        .fromResponseValue(value)
        .filter(_.hasNext.isEmpty)
        .map(response => response.copy(errors = response.errors.map(RemoteError.disclose(_, disclosure))))
    else None
  }
}

private[gateway] object RemoteGraphQLSource {

  def apply(endpoint: Uri, backend: SttpClient): RemoteGraphQLSource[Any] =
    new RemoteGraphQLSource(endpoint, backend, RemoteGraphQLConfig.default, StructuralLimits.default, None, None)

  def apply[R](
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteGraphQLConfig[R]
  ): RemoteGraphQLSource[R] =
    new RemoteGraphQLSource(endpoint, backend, config, StructuralLimits.default, None, None)

  def apply[R](
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteGraphQLConfig[R],
    structuralLimits: StructuralLimits
  ): RemoteGraphQLSource[R] =
    new RemoteGraphQLSource(endpoint, backend, config, structuralLimits, None, None)

  def make[R](
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteGraphQLConfig[R]
  )(implicit trace: Trace): ZIO[Scope, Nothing, RemoteGraphQLSource[R]] =
    ZIO.scopeWith { scope =>
      val deduplicator =
        if (config.execution.inFlightQueryDeduplication)
          InFlightQueryDeduplicator.make(scope, config.execution.maxConcurrentCalls).map(Some(_))
        else ZIO.none
      deduplicator.map(new RemoteGraphQLSource(endpoint, backend, config, StructuralLimits.default, _, None))
    }

  final case class StructuralLimits(
    maxResponseDepth: Int,
    maxResponseTokens: Int
  )

  object StructuralLimits {
    val default: StructuralLimits = StructuralLimits(
      maxResponseDepth = 128,
      maxResponseTokens = 250000
    )
  }

  private[internal] final case class QueryCallIdentity(
    body: RequestBody,
    headers: Vector[(String, Vector[String])]
  )

  private object QueryCallIdentity {
    def apply(body: Array[Byte], headers: List[Header]): QueryCallIdentity = {
      val grouped = mutable.HashMap.empty[String, Vector[String]]
      headers.foreach { header =>
        val name = RemoteGraphQLConfig.normalize(header.name)
        grouped.update(name, grouped.getOrElse(name, Vector.empty) :+ header.value)
      }
      QueryCallIdentity(new RequestBody(body), grouped.toVector.sortBy(_._1))
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
    def execute(
      key: QueryCallIdentity,
      admission: Option[ExecutionGate]
    )(
      call: => IO[GraphQLSource.Failure, GraphQLResponse[CalibanError]]
    )(implicit trace: Trace): IO[GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
      ZIO.uninterruptible(loop(key, admission, call))

    private def loop(
      key: QueryCallIdentity,
      admission: Option[ExecutionGate],
      call: => IO[GraphQLSource.Failure, GraphQLResponse[CalibanError]]
    )(implicit trace: Trace): IO[GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
      state.get.flatMap { current =>
        current.entries.get(key) match {
          case Some(existing) => await(existing).interruptible
          case None           =>
            Promise.make[Nothing, QueryCallExit].flatMap { candidate =>
              decide(key, candidate).flatMap {
                case QueryCallDecision.Start          =>
                  runCandidate(key, candidate, admission, call).interruptible.forkIn(scope) *>
                    await(candidate).interruptible
                case QueryCallDecision.Join(existing) => await(existing).interruptible
                case QueryCallDecision.Wait(signal)   => signal.await.interruptible *> loop(key, admission, call)
              }
            }
        }
      }

    private def runCandidate(
      key: QueryCallIdentity,
      candidate: QueryCallPromise,
      admission: Option[ExecutionGate],
      call: => IO[GraphQLSource.Failure, GraphQLResponse[CalibanError]]
    )(implicit trace: Trace): UIO[Unit] =
      complete(key, candidate, admission.fold(call)(_(call)))

    private def decide(key: QueryCallIdentity, candidate: QueryCallPromise): UIO[QueryCallDecision] =
      state.modify { current =>
        current.entries.get(key) match {
          case Some(existing)                               => QueryCallDecision.Join(existing)      -> current
          case None if current.entries.size < current.limit =>
            QueryCallDecision.Start -> current.copy(entries = current.entries.updated(key, candidate))
          case None                                         => QueryCallDecision.Wait(current.space) -> current
        }
      }

    private def complete(
      key: QueryCallIdentity,
      promise: QueryCallPromise,
      call: => IO[GraphQLSource.Failure, GraphQLResponse[CalibanError]]
    )(implicit trace: Trace): UIO[Unit] =
      ZIO.uninterruptibleMask { restore =>
        restore(call).exit.flatMap { exit =>
          Promise.make[Nothing, Unit].flatMap { nextSpace =>
            state.modify { current =>
              current.space -> current.copy(entries = current.entries - key, space = nextSpace)
            }
              .flatMap(_.succeed(())) *>
              promise.succeed(exit).unit
          }
        }
      }

    private def await(
      promise: QueryCallPromise
    )(implicit trace: Trace): IO[GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
      promise.await.flatMap(ZIO.suspendSucceed(_))
  }

  private type QueryCallExit    = Exit[GraphQLSource.Failure, GraphQLResponse[CalibanError]]
  private type QueryCallPromise = Promise[Nothing, QueryCallExit]

  private sealed trait QueryCallDecision
  private object QueryCallDecision {
    final case class Join(existing: QueryCallPromise)     extends QueryCallDecision
    final case class Wait(signal: Promise[Nothing, Unit]) extends QueryCallDecision
    case object Start                                     extends QueryCallDecision
  }

  private final case class QueryCallState(
    entries: Map[QueryCallIdentity, QueryCallPromise],
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

  private case object RequestLimitExceeded extends RuntimeException with NoStackTrace

  private final class BoundedOutputStream(maxBytes: Int) extends OutputStream {
    private val underlying = new ByteArrayOutputStream(math.min(maxBytes, 8192))

    override def write(value: Int): Unit = {
      ensureCapacity(1)
      underlying.write(value)
    }

    override def write(values: Array[Byte], offset: Int, length: Int): Unit = {
      ensureCapacity(length)
      underlying.write(values, offset, length)
    }

    def toByteArray: Array[Byte] = underlying.toByteArray

    private def ensureCapacity(additionalBytes: Int): Unit =
      if (additionalBytes > maxBytes - underlying.size()) throw RequestLimitExceeded
  }
}
