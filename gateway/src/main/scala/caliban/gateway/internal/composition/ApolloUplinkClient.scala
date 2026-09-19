package caliban.gateway.internal.composition

import caliban.gateway.TypenameField
import caliban.{ GraphQLRequest, InputValue, ResponseValue }
import caliban.gateway.{ SupergraphAcquisitionError, SupergraphUplinkConfig }
import caliban.gateway.SupergraphAcquisitionError._
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.execution.RemoteTransport
import caliban.gateway.SupergraphAcquisitionError.InvalidUplinkResponse._
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ FloatValue, IntValue, NullValue, StringValue }

import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, writeToArray }
import zio.{ IO, Trace, ZIO }
import zio.http.URL

private[gateway] object ApolloUplinkClient {

  def fetch(
    endpoint: URL,
    config: SupergraphUplinkConfig,
    ifAfterId: Option[String],
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SupergraphAcquisitionError, UplinkResponse] = {
    val acquisition = config.acquisition
    val body        = writeToArray(request(config.apiKey.stringValue, config.graphRef, ifAfterId))

    http
      .post(endpoint, body, acquisition.headers, acquisition.maxResponseBytes)
      .mapError[SupergraphAcquisitionError](RequestFailed(_))
      .flatMap { reply =>
        if (reply.body.limitExceeded) ZIO.fail(ResponseTooLarge(acquisition.maxResponseBytes))
        else if (!reply.status.isSuccess || RemoteSchemaAcquisition.isHtml(reply.contentType))
          ZIO.fail(UnexpectedResponse(reply.status, reply.contentType))
        else if (
          RemoteTransport.validateJsonStructure(reply.body.bytes, acquisition.maxParsingDepth, Int.MaxValue).isLeft
        )
          ZIO.fail(ParsingDepthExceeded(acquisition.maxParsingDepth))
        else
          // Remote error text must not reach diagnostics, including JSON decoding exceptions.
          ZIO
            .attempt(readFromArray[ResponseValue](reply.body.bytes))
            .mapError(_ => InvalidUplinkResponse.DecodingFailed)
            .flatMap(response => ZIO.fromEither(decode(response)))
            .mapError(InvalidUplinkResponse(_))
      }
  }

  sealed trait UplinkResponse

  object UplinkResponse {
    final case class Success(id: String, supergraphSDL: Option[String], minDelaySeconds: Double) extends UplinkResponse
    final case class Failure(code: String, message: String)                                      extends UplinkResponse
  }

  private def request(apiKey: String, ref: String, ifAfterId: Option[String]): GraphQLRequest =
    GraphQLRequest(
      query = Some(Query),
      operationName = Some(OperationName),
      variables = Some(
        Map(
          "apiKey"    -> StringValue(apiKey),
          "ref"       -> StringValue(ref),
          "ifAfterId" -> ifAfterId.fold[InputValue](NullValue)(StringValue(_))
        )
      )
    )

  private def decode(response: ResponseValue): Either[InvalidUplinkResponse.Reason, UplinkResponse] =
    response match {
      case envelope: ObjectValue =>
        val hasErrors = envelope.getOrNull("errors") match {
          case ListValue(values) => values.nonEmpty
          case _                 => false
        }
        (objectField(envelope, "data"), hasErrors) match {
          case (Some(data), false) =>
            objectField(data, "routerConfig").toRight(MissingRouterConfig).flatMap(routerConfig)
          case (Some(_), true)     => Left(MissingRouterConfig)
          case (None, true)        => Left(MissingData)
          case (None, false)       => Left(DecodingFailed)
        }
      case _                     => Left(DecodingFailed)
    }

  private def routerConfig(value: ObjectValue): Either[InvalidUplinkResponse.Reason, UplinkResponse] =
    string(value, TypenameField).toRight(DecodingFailed).flatMap {
      case "RouterConfigResult" =>
        for {
          id    <- string(value, "id").toRight(MissingId)
          sdl   <- string(value, "supergraphSDL").toRight(MissingSupergraphSdl)
          delay <- number(value, "minDelaySeconds").toRight(DecodingFailed)
        } yield UplinkResponse.Success(id, Some(sdl), delay)
      case "FetchError"         =>
        for {
          code    <- string(value, "code").toRight(DecodingFailed)
          message <- string(value, "message").toRight(DecodingFailed)
        } yield UplinkResponse.Failure(code, message)
      case "Unchanged"          =>
        for {
          id    <- string(value, "id").toRight(MissingId)
          delay <- number(value, "minDelaySeconds").toRight(DecodingFailed)
        } yield UplinkResponse.Success(id, None, delay)
      case _                    => Left(UnknownTypename)
    }

  private def objectField(value: ObjectValue, field: String): Option[ObjectValue] =
    value.getOrNull(field) match {
      case result: ObjectValue => Some(result)
      case _                   => None
    }

  private def string(value: ObjectValue, field: String): Option[String] =
    value.getOrNull(field) match {
      case StringValue(result) => Some(result)
      case _                   => None
    }

  private def number(value: ObjectValue, field: String): Option[Double] =
    value.getOrNull(field) match {
      case result: IntValue   => Some(result.toBigInt.toDouble)
      case result: FloatValue => Some(result.toDouble)
      case _                  => None
    }

  private final val OperationName = "SupergraphSdl"

  private val Query: String =
    s"""query $OperationName($$apiKey: String!, $$ref: String!, $$ifAfterId: ID) {
       |  routerConfig(apiKey: $$apiKey, ref: $$ref, ifAfterId: $$ifAfterId) {
       |    __typename
       |    ... on RouterConfigResult {
       |      id
       |      supergraphSDL
       |      minDelaySeconds
       |    }
       |    ... on FetchError {
       |      code
       |      message
       |    }
       |    ... on Unchanged {
       |      id
       |      minDelaySeconds
       |    }
       |  }
       |}""".stripMargin
}
