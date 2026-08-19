package caliban.gateway.internal

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, NullValue, StringValue }
import caliban.parsing.adt.LocationInfo
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, PathValue, ResponseValue }
import com.github.plokhotnyuk.jsoniter_scala.core._
import sttp.capabilities.zio.ZioStreams
import sttp.client4._
import sttp.client4.httpclient.zio.SttpClient
import sttp.model.Uri
import zio._
import zio.stream.ZStream

import java.io.{ ByteArrayOutputStream, OutputStream }
import scala.util.control.{ NoStackTrace, NonFatal }

private[gateway] final class RemoteGraphQLSource(
  endpoint: Uri,
  backend: SttpClient,
  limits: RemoteGraphQLSource.Limits = RemoteGraphQLSource.Limits.default
) extends GraphQLSource[Any] {
  import RemoteGraphQLSource._

  val errorPolicy: GraphQLSource.ErrorPolicy = GraphQLSource.ErrorPolicy.Remote

  def execute(request: GraphQLRequest)(implicit
    trace: Trace
  ): ZIO[Any, GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
    ZIO
      .fromEither(encode(request.copy(extensions = None)))
      .flatMap { body =>
        basicRequest
          .post(endpoint)
          .body(body)
          .contentType("application/json; charset=utf-8")
          .header("Accept", "application/graphql-response+json, application/json;q=0.9")
          .followRedirects(false)
          .response(asStreamAlways(ZioStreams)(readBounded))
          .send(backend)
          .mapError(_ => GraphQLSource.TransportFailure)
          .timeoutFail(GraphQLSource.TimeoutFailure)(limits.timeout)
      }
      .flatMap(response => ZIO.fromEither(decode(response)))

  private def encode(request: GraphQLRequest): Either[GraphQLSource.Failure, Array[Byte]] = {
    val output = new BoundedOutputStream(limits.maxRequestBytes)
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
      .take(limits.maxResponseBytes.toLong + 1L)
      .runCollect
      .map(bytes => BoundedBody(bytes, bytes.length > limits.maxResponseBytes))

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

      if (depth > limits.maxResponseDepth) failure = Some(GraphQLSource.ResponseNestingTooDeep)
      else if (tokens > limits.maxResponseTokens) failure = Some(GraphQLSource.ResponseStructureTooLarge)
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

  final case class Limits(
    timeout: Duration,
    maxRequestBytes: Int,
    maxResponseBytes: Int,
    maxResponseDepth: Int,
    maxResponseTokens: Int
  )

  object Limits {
    val default: Limits = Limits(
      timeout = Duration.fromSeconds(30),
      maxRequestBytes = 1024 * 1024,
      maxResponseBytes = 16 * 1024 * 1024,
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
