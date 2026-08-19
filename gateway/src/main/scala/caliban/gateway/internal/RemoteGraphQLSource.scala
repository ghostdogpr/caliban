package caliban.gateway.internal

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, NullValue, StringValue }
import caliban.gateway.{ IncomingRequestHeaders, RemoteGraphQLConfig }
import caliban.parsing.Parser
import caliban.parsing.adt.LocationInfo
import caliban.parsing.adt.OperationType
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, PathValue, ResponseValue }
import com.github.plokhotnyuk.jsoniter_scala.core._
import sttp.capabilities.zio.ZioStreams
import sttp.client4._
import sttp.client4.httpclient.zio.SttpClient
import sttp.model.{ Header, Uri }
import zio._
import zio.stream.ZStream

import java.io.{ ByteArrayOutputStream, OutputStream }
import scala.util.control.{ NoStackTrace, NonFatal }

private[gateway] final class RemoteGraphQLSource[-R](
  endpoint: Uri,
  backend: SttpClient,
  config: RemoteGraphQLConfig[R],
  structuralLimits: RemoteGraphQLSource.StructuralLimits
) extends GraphQLSource[R] {
  import RemoteGraphQLSource._

  private val execution = config.execution

  val errorPolicy: GraphQLSource.ErrorPolicy = GraphQLSource.ErrorPolicy.Remote

  def execute(request: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[R, GraphQLSource.Failure, GraphQLResponse[CalibanError]] = {
    val logicalCall =
      for {
        body      <- ZIO.fromEither(encode(request.copy(extensions = None)))
        incoming  <- IncomingRequestHeaders.get
        effectful <- config.effectfulHeaders.mapError(_ => GraphQLSource.HeaderFailure)
        headers    = outboundHeaders(incoming, effectful)
        replaySafe = execution.retries > 0 && isReplaySafe(request)
        response  <- executeAttempts(body, headers, replaySafe, execution.retries)
      } yield response

    logicalCall.timeoutFail(GraphQLSource.TimeoutFailure)(execution.timeout)
  }

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

  private def isReplaySafe(request: GraphQLRequest): Boolean =
    request.query.exists { query =>
      Parser
        .parseQuery(query)
        .fold(
          _ => false,
          document => {
            val operation = request.operationName match {
              case Some(name) => document.operationDefinitions.find(_.name.contains(name))
              case None       =>
                document.operationDefinitions match {
                  case value :: Nil => Some(value)
                  case _            => None
                }
            }
            operation.exists(_.operationType == OperationType.Query)
          }
        )
    }

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
      .map(bytes => BoundedBody(bytes, bytes.length > execution.maxResponseBytes))

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

  private def decodeBody(bytes: Chunk[Byte]): Either[GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
    for {
      _        <- responseWithinLimits(bytes)
      envelope <-
        try Right(readFromArray[ResponseValue](bytes.toArray))
        catch {
          case NonFatal(_) => Left(GraphQLSource.InvalidResponse)
        }
      response <- envelope match {
                    case value: ObjectValue if validEnvelope(value) => Right(decodeEnvelope(value))
                    case _                                          => Left(GraphQLSource.InvalidResponse)
                  }
    } yield response

  private def responseWithinLimits(bytes: Chunk[Byte]): Either[GraphQLSource.Failure, Unit] = {
    var depth    = 0
    var index    = 0
    var escaped  = false
    var string   = false
    var tokens   = 0
    var previous = 0.toByte
    var failure  = Option.empty[GraphQLSource.Failure]

    while (index < bytes.length && failure.isEmpty) {
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
          case '}' | ']'                               =>
            depth -= 1
            previous = current
          case value if isScalarStart(value, previous) =>
            tokens += 1
            previous = current
          case value if !value.toChar.isWhitespace     => previous = current
          case _                                       => ()
        }

      if (depth > structuralLimits.maxResponseDepth) failure = Some(GraphQLSource.ResponseNestingTooDeep)
      else if (tokens > structuralLimits.maxResponseTokens) failure = Some(GraphQLSource.ResponseStructureTooLarge)
      index += 1
    }

    failure.toLeft(())
  }

  private def isScalarStart(value: Byte, previous: Byte): Boolean = {
    val scalar   = value == '-' || value >= '0' && value <= '9' || value == 't' || value == 'f' || value == 'n'
    val boundary = previous == 0 || previous == '[' || previous == ',' || previous == ':'
    scalar && boundary
  }

  private def validEnvelope(value: ResponseValue): Boolean =
    value match {
      case ObjectValue(fields) =>
        val data            = fields.collectFirst { case ("data", value) => value }
        val errors          = fields.collectFirst { case ("errors", value) => value }
        val extensions      = fields.collectFirst { case ("extensions", value) => value }
        val validData       = data.forall {
          case _: ObjectValue => true
          case NullValue      => true
          case _              => false
        }
        val validErrors     = errors.forall {
          case ListValue(values) if values.nonEmpty => values.forall(validError)
          case _                                    => false
        }
        val validExtensions = extensions.forall(_.isInstanceOf[ObjectValue])
        (data.nonEmpty || errors.nonEmpty) && validData && validErrors && validExtensions &&
        !(data.contains(NullValue) && errors.isEmpty)
      case _                   => false
    }

  private def decodeEnvelope(envelope: ObjectValue): GraphQLResponse[CalibanError] = {
    val data       = envelope.fields.collectFirst { case ("data", value) => value }.getOrElse(NullValue)
    val errors     = envelope.fields.collectFirst { case ("errors", ListValue(values)) => values.map(decodeError) }
    val extensions = envelope.fields.collectFirst { case ("extensions", value: ObjectValue) => value }
    GraphQLResponse(data, errors.getOrElse(Nil), extensions)
  }

  private def decodeError(value: ResponseValue): CalibanError = {
    val fields     = value.asInstanceOf[ObjectValue].fields
    val message    = fields.collectFirst { case ("message", StringValue(value)) => value }.get
    val path       = fields.collectFirst { case ("path", ListValue(values)) => decodePath(values) }.flatten.getOrElse(Nil)
    val location   = fields.collectFirst { case ("locations", ListValue((value: ObjectValue) :: _)) => value }
      .flatMap(decodeLocation)
    val extensions = fields.collectFirst { case ("extensions", value: ObjectValue) => value }
    CalibanError.ExecutionError(message, path, location, extensions = extensions)
  }

  private def pathValue(value: ResponseValue): Option[PathValue] =
    value match {
      case value: StringValue        => Some(value)
      case value: IntValue.IntNumber => Some(value)
      case _                         => None
    }

  private def decodePath(values: List[ResponseValue]): Option[List[PathValue]] = {
    val decoded = values.map(pathValue)
    if (decoded.forall(_.nonEmpty)) Some(decoded.flatten) else None
  }

  private def decodeLocation(value: ObjectValue): Option[LocationInfo] = {
    val line   = value.fields.collectFirst { case ("line", IntValue.IntNumber(value)) => value }
    val column = value.fields.collectFirst { case ("column", IntValue.IntNumber(value)) => value }
    for {
      line   <- line
      column <- column
    } yield LocationInfo(column, line)
  }

  private def validError(value: ResponseValue): Boolean =
    value match {
      case ObjectValue(fields) => fields.exists { case ("message", _: StringValue) => true; case _ => false }
      case _                   => false
    }
}

private[gateway] object RemoteGraphQLSource {

  def apply(endpoint: Uri, backend: SttpClient): RemoteGraphQLSource[Any] =
    new RemoteGraphQLSource(endpoint, backend, RemoteGraphQLConfig.default, StructuralLimits.default)

  def apply[R](
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteGraphQLConfig[R]
  ): RemoteGraphQLSource[R] =
    new RemoteGraphQLSource(endpoint, backend, config, StructuralLimits.default)

  def apply[R](
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteGraphQLConfig[R],
    structuralLimits: StructuralLimits
  ): RemoteGraphQLSource[R] =
    new RemoteGraphQLSource(endpoint, backend, config, structuralLimits)

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

  private final case class BoundedBody(bytes: Chunk[Byte], limitExceeded: Boolean)

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
