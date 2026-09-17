package caliban.gateway.internal.composition

import caliban.ResponseValue
import caliban.gateway.{ RemoteGraphQLConfig, SchemaAcquisitionError }
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.SchemaAcquisitionError._
import caliban.gateway.SchemaAcquisitionError.InvalidFederationResponse._
import caliban.parsing.adt.Document
import caliban.parsing.Parser
import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import zio.{ IO, Trace, ZIO }
import zio.http.URL

private[composition] object FederationClient {

  def fetch(
    endpoint: URL,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Document] =
    RemoteSchemaAcquisition
      .fetchBytes(endpoint, Query, OperationName, config, http)
      .flatMap { bytes =>
        for {
          decoded  <- ZIO
                        .attempt(readFromArray[ResponseValue](bytes))
                        .mapError(FederationResponseDecodingFailed(_))
          sdl      <- ZIO.fromEither(decode(decoded))
          _        <- ZIO
                        .fail(ParsingDepthExceeded(config.maxParsingDepth))
                        .unless(RemoteSchemaAcquisition.withinGraphQLDepth(sdl, config.maxParsingDepth))
          document <- ZIO.fromEither(Parser.parseQuery(sdl)).mapError(InvalidFederationSchema(_))
        } yield document
      }

  private def decode(value: ResponseValue): Either[SchemaAcquisitionError, String] =
    value match {
      case objectValue: ObjectValue =>
        for {
          errors  <-
            RemoteSchemaAcquisition.responseErrors(objectValue).toRight(InvalidFederationResponse(InvalidErrors))
          _       <- if (errors.isEmpty) Right(()) else Left(FederationErrors(errors))
          data    <- objectField(objectValue, "data", MissingData)
          service <- objectField(data, "_service", MissingService)
          sdl     <- service.fields.collectFirst { case ("sdl", StringValue(value)) => value }
                       .toRight(InvalidFederationResponse(MissingSdl))
        } yield sdl
      case _                        => Left(InvalidFederationResponse(ExpectedResponseObject))
    }

  private def objectField(
    value: ObjectValue,
    name: String,
    missing: InvalidFederationResponse.Reason
  ): Either[SchemaAcquisitionError, ObjectValue] =
    value.fields.collectFirst { case (`name`, nested: ObjectValue) => nested }
      .toRight(InvalidFederationResponse(missing))

  private val OperationName = "__CalibanGatewayServiceSchema"
  private val Query         = s"query $OperationName { _service { sdl } }"
}
