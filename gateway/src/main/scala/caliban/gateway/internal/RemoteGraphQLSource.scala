package caliban.gateway.internal

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, NullValue, StringValue }
import caliban.parsing.adt.LocationInfo
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, PathValue, ResponseValue }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, writeToArray }
import sttp.client4._
import sttp.client4.httpclient.zio.SttpClient
import sttp.model.Uri
import zio.{ IO, Trace, ZIO }

import scala.util.control.{ NoStackTrace, NonFatal }

private[gateway] final class RemoteGraphQLSource(endpoint: Uri, backend: SttpClient) {

  def execute(request: GraphQLRequest)(implicit
    trace: Trace
  ): IO[RemoteGraphQLSource.Failure, GraphQLResponse[CalibanError]] =
    ZIO
      .attempt(writeToArray(request))
      .orDie
      .flatMap { body =>
        basicRequest
          .post(endpoint)
          .body(body)
          .contentType("application/json; charset=utf-8")
          .header("Accept", "application/graphql-response+json, application/json;q=0.9")
          .followRedirects(false)
          .response(asByteArrayAlways)
          .send(backend)
          .mapError(_ => RemoteGraphQLSource.TransportFailure)
      }
      .flatMap(response => ZIO.fromEither(decode(response)))

  private def decode(
    response: Response[Array[Byte]]
  ): Either[RemoteGraphQLSource.InvalidResponse.type, GraphQLResponse[CalibanError]] = {
    val mediaType = response.contentType.map(_.takeWhile(_ != ';').trim.toLowerCase(java.util.Locale.ROOT))
    val allowed   = mediaType.contains("application/graphql-response+json") ||
      (response.code.isSuccess && mediaType.contains("application/json"))

    if (!allowed) Left(RemoteGraphQLSource.InvalidResponse)
    else
      try {
        val envelope = readFromArray[ResponseValue](response.body)
        if (validEnvelope(envelope)) Right(decodeEnvelope(envelope.asInstanceOf[ObjectValue]))
        else Left(RemoteGraphQLSource.InvalidResponse)
      } catch {
        case NonFatal(_) => Left(RemoteGraphQLSource.InvalidResponse)
      }
  }

  private def validEnvelope(value: ResponseValue): Boolean =
    value match {
      case ObjectValue(fields) =>
        val data        = fields.collectFirst { case ("data", value) => value }
        val errors      = fields.collectFirst { case ("errors", value) => value }
        val validData   = data.forall {
          case _: ObjectValue => true
          case NullValue      => true
          case _              => false
        }
        val validErrors = errors.forall {
          case ListValue(values) if values.nonEmpty => values.forall(validError)
          case _                                    => false
        }
        (data.nonEmpty || errors.nonEmpty) && validData && validErrors && !(data.contains(NullValue) && errors.isEmpty)
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
  sealed trait Failure         extends NoStackTrace
  case object TransportFailure extends Failure
  case object InvalidResponse  extends Failure
}
