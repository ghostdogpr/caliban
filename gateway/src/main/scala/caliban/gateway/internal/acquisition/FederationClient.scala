package caliban.gateway.internal.acquisition

import caliban.ResponseValue
import caliban.gateway.{ RemoteGraphQLConfig, ServiceField, SubgraphAcquisitionError }
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.SubgraphAcquisitionError._
import caliban.gateway.SubgraphAcquisitionError.InvalidFederationResponse._
import caliban.parsing.adt.Document
import caliban.parsing.Parser
import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import zio.{ IO, Trace, ZIO }
import zio.http.URL

private[acquisition] object FederationClient {

  def fetch(
    endpoint: URL,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SubgraphAcquisitionError, Document] =
    for {
      bytes    <- RemoteSchemaAcquisition.fetchBytes(endpoint, Query, OperationName, config, http)
      decoded  <- ZIO.attempt(readFromArray[ResponseValue](bytes)).mapError(FederationResponseDecodingFailed(_))
      sdl      <- ZIO.fromEither(decode(decoded))
      _        <- ZIO
                    .fail(ParsingDepthExceeded(config.maxParsingDepth))
                    .unless(RemoteSchemaAcquisition.withinGraphQLDepth(sdl, config.maxParsingDepth))
      document <- ZIO.fromEither(Parser.parseQuery(sdl)).mapError(FederationSchemaParsingFailed(_))
    } yield document

  private def decode(value: ResponseValue): Either[SubgraphAcquisitionError, String] =
    value match {
      case envelope: ObjectValue =>
        for {
          errors  <-
            RemoteSchemaAcquisition.responseErrors(envelope).toRight(InvalidFederationResponse(InvalidErrors))
          _       <- if (errors.isEmpty) Right(()) else Left(FederationErrors(errors))
          data    <- objectField(envelope, "data", MissingData)
          service <- objectField(data, ServiceField, MissingService)
          sdl     <- service.fields.collectFirst { case ("sdl", StringValue(value)) => value }
                       .toRight(InvalidFederationResponse(MissingSdl))
        } yield sdl
      case _                     => Left(InvalidFederationResponse(ExpectedResponseObject))
    }

  private def objectField(
    value: ObjectValue,
    name: String,
    missing: InvalidFederationResponse.Reason
  ): Either[SubgraphAcquisitionError, ObjectValue] =
    value.fields.collectFirst { case (`name`, nested: ObjectValue) => nested }
      .toRight(InvalidFederationResponse(missing))

  private final val OperationName = "__CalibanGatewayServiceSchema"
  private val Query               = s"query $OperationName { _service { sdl } }"
}
