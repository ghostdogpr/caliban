package caliban.gateway.internal.composition

import caliban.{ CalibanError, GraphQLRequest, ResponseValue }
import caliban.gateway.{ RemoteGraphQLConfig, SchemaAcquisitionError, SchemaInput }
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.execution.RemoteTransport
import caliban.gateway.SchemaAcquisitionError._
import caliban.gateway.SchemaAcquisitionError.InvalidFederationResponse._
import caliban.parsing.adt.Document
import caliban.parsing.Parser
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ NullValue, StringValue }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, writeToArray }
import zio.{ IO, Trace, ZIO }
import zio.http.URL

private[gateway] object RemoteSchemaAcquisition {

  private val ServiceOperationName = "__CalibanGatewayServiceSchema"
  private val ServiceQuery         = s"query $ServiceOperationName { _service { sdl } }"

  def load(
    input: SchemaInput,
    endpoint: URL,
    federation: Boolean,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Document] =
    input match {
      case SchemaInput.Sdl(value)    => ZIO.fromEither(Parser.parseQuery(value)).mapError(InvalidProvidedSchema(_))
      case SchemaInput.Parsed(value) => ZIO.succeed(value)
      case SchemaInput.Acquired      => acquire(endpoint, federation, config, http)
    }

  private def acquire(
    endpoint: URL,
    federation: Boolean,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Document] = {
    val acquisition =
      if (federation) acquireFederation(endpoint, config, http)
      else acquireIntrospection(endpoint, config, http)

    acquisition.timeoutFail(TimedOut(config.timeout))(config.timeout)
  }

  private def acquireIntrospection(
    endpoint: URL,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Document] = {
    val request = GraphQLRequest(
      query = Some(IntrospectionDocument.Query),
      operationName = Some(IntrospectionDocument.OperationName)
    )

    send(endpoint, writeToArray(request), config, http).flatMap { bytes =>
      for {
        response <- ZIO
                      .attempt(readFromArray[ResponseValue](bytes))
                      .mapError(IntrospectionResponseDecodingFailed(_))
        _        <- validateDepth(defaultValuesWithinDepth(response, config.maxParsingDepth), config.maxParsingDepth)
        envelope <- ZIO
                      .fromEither(IntrospectionDocument.asObject(response, "response"))
                      .mapError(IntrospectionResponseDecodingFailed(_))
        errors   <- ZIO
                      .fromOption(responseErrors(envelope))
                      .orElseFail(IntrospectionResponseDecodingFailed(IntrospectionDocument.Invalid("errors")))
        _        <- ZIO.fail(IntrospectionErrors(errors)).when(errors.nonEmpty)
        document <- ZIO
                      .fromEither(IntrospectionDocument.decode(envelope.getOrNull("data")))
                      .mapError(IntrospectionResponseDecodingFailed(_))
      } yield document
    }
  }

  private def acquireFederation(
    endpoint: URL,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Document] = {
    val request = GraphQLRequest(query = Some(ServiceQuery), operationName = Some(ServiceOperationName))

    send(endpoint, writeToArray(request), config, http).flatMap { bytes =>
      for {
        decoded  <- ZIO
                      .attempt(readFromArray[ResponseValue](bytes))
                      .mapError(FederationResponseDecodingFailed(_))
        sdl      <- ZIO.fromEither(decodeServiceSdl(decoded))
        _        <- validateDepth(withinGraphQLDepth(sdl, config.maxParsingDepth), config.maxParsingDepth)
        document <- ZIO.fromEither(Parser.parseQuery(sdl)).mapError(InvalidFederationSchema(_))
      } yield document
    }
  }

  private def send(
    endpoint: URL,
    body: Array[Byte],
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Array[Byte]] =
    http
      .post(endpoint, body, config.headers, config.maxResponseBytes)
      .mapError(RequestFailed(_))
      .flatMap { reply =>
        if (reply.body.limitExceeded)
          ZIO.fail(ResponseTooLarge(config.maxResponseBytes))
        else if (reply.status.isRedirection || !allowedMediaType(reply))
          ZIO.fail(UnexpectedResponse(reply.status, reply.contentType))
        else
          ZIO
            .fromEither(
              RemoteTransport
                .validateJsonStructure(reply.body.bytes, config.maxParsingDepth, Int.MaxValue)
                .left
                .map(_ => ParsingDepthExceeded(config.maxParsingDepth))
            )
            .as(reply.body.bytes)
      }

  private def allowedMediaType(reply: GatewayHttpClient.Reply): Boolean = {
    val mediaType = RemoteTransport.mediaType(reply.contentType)
    mediaType.contains("application/graphql-response+json") ||
    reply.status.isSuccess && mediaType.contains("application/json")
  }

  private def responseErrors(value: ObjectValue): Option[List[CalibanError]] =
    value.getOrNull("errors") match {
      case null | NullValue => Some(Nil)
      case ListValue(items) =>
        val decoded = items.map(CalibanError.fromResponseValue)
        if (decoded.forall(_.nonEmpty)) Some(decoded.flatten) else None
      case _                => None
    }

  private def decodeServiceSdl(value: ResponseValue): Either[SchemaAcquisitionError, String] =
    value match {
      case objectValue: ObjectValue =>
        for {
          errors  <- responseErrors(objectValue).toRight(InvalidFederationResponse(InvalidErrors))
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

  private def validateDepth(valid: Boolean, maxDepth: Int): IO[SchemaAcquisitionError, Unit] =
    if (valid) ZIO.unit else ZIO.fail(ParsingDepthExceeded(maxDepth))

  private def defaultValuesWithinDepth(value: ResponseValue, maxDepth: Int): Boolean =
    value match {
      case ObjectValue(fields) =>
        fields.forall {
          case ("defaultValue", StringValue(defaultValue)) =>
            withinGraphQLDepth(defaultValue, maxDepth)
          case (_, nested)                                 => defaultValuesWithinDepth(nested, maxDepth)
        }
      case ListValue(values)   => values.forall(defaultValuesWithinDepth(_, maxDepth))
      case _                   => true
    }

  // Bound parser recursion in schema text embedded inside JSON strings. Syntax validation stays with Parser.
  private[composition] def withinGraphQLDepth(value: String, maxDepth: Int): Boolean = {
    var index   = 0
    var depth   = 0
    var quote   = ""
    var comment = false
    while (index < value.length && depth <= maxDepth) {
      val current = value.charAt(index)
      if (comment) {
        if (current == '\n' || current == '\r') comment = false
      } else if (quote.nonEmpty) {
        if (quote == "\"" && current == '\\') index += 1
        else if (quote.length == 3 && value.startsWith("\\\"\"\"", index)) index += 3
        else if (value.startsWith(quote, index)) {
          index += quote.length - 1
          quote = ""
        }
      } else
        current match {
          case '#'             => comment = true
          case '"'             =>
            quote = if (value.startsWith("\"\"\"", index)) "\"\"\"" else "\""
            index += quote.length - 1
          case '{' | '[' | '(' => depth += 1
          case '}' | ']' | ')' => depth = math.max(0, depth - 1)
          case _               => ()
        }
      index += 1
    }
    depth <= maxDepth
  }

}
