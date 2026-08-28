package caliban.gateway.internal.composition

import caliban.{ CalibanError, GraphQLRequest, ResponseValue }
import caliban.client.CalibanClientError.ServerError
import caliban.client.Operations.RootQuery
import caliban.client.SelectionBuilder
import caliban.gateway.{ RemoteGraphQLConfig, SchemaAcquisitionError, SchemaInput }
import caliban.gateway.SchemaAcquisitionError._
import caliban.gateway.SchemaAcquisitionError.InvalidFederationResponse._
import caliban.parsing.adt.Document
import caliban.parsing.Parser
import caliban.ResponseValue.ObjectValue
import caliban.tools.IntrospectionClient
import caliban.Value.{ NullValue, StringValue }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, writeToArray }
import sttp.capabilities.zio.ZioStreams
import sttp.client4._
import sttp.client4.httpclient.zio.SttpClient
import sttp.model.Uri
import zio.{ IO, Task, Trace, ZIO }
import zio.stream.ZStream

import java.nio.charset.StandardCharsets

private[gateway] object RemoteSchemaAcquisition {

  private val ServiceQuery =
    "query __CalibanGatewayServiceSchema { _service { sdl } }"

  def load(
    input: SchemaInput,
    endpoint: Uri,
    federation: Boolean,
    config: RemoteGraphQLConfig.Acquisition,
    backend: SttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Document] =
    input match {
      case SchemaInput.Sdl(value)    => ZIO.fromEither(Parser.parseQuery(value)).mapError(InvalidProvidedSchema(_))
      case SchemaInput.Parsed(value) => ZIO.succeed(value)
      case SchemaInput.Acquired      => acquire(endpoint, federation, config, backend)
    }

  private def acquire(
    endpoint: Uri,
    federation: Boolean,
    config: RemoteGraphQLConfig.Acquisition,
    backend: SttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Document] = {
    val acquisition =
      if (federation) acquireFederation(endpoint, config, backend)
      else acquireIntrospection(endpoint, config, backend)

    acquisition.timeoutFail(TimedOut(config.timeout))(config.timeout)
  }

  private def acquireIntrospection(
    endpoint: Uri,
    config: RemoteGraphQLConfig.Acquisition,
    backend: SttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Document] = {
    implicit val introspectionConfig: IntrospectionClient.Config = IntrospectionClient.Config.default
    val selection: SelectionBuilder[RootQuery, Document]         = IntrospectionClient.introspection
    val request                                                  = selection.toGraphQL(dropNullInputValues = true)

    send(endpoint, writeToArray(request), config, backend).flatMap { bytes =>
      if (!withinJsonDepth(bytes, config.maxParsingDepth)) parsingDepthFailure(config.maxParsingDepth)
      else {
        for {
          response             <- ZIO
                                    .attempt(readFromArray[ResponseValue](bytes))
                                    .mapError(IntrospectionResponseDecodingFailed(_))
          _                    <- if (defaultValuesWithinDepth(response, config.maxParsingDepth)) ZIO.unit
                                  else parsingDepthFailure(config.maxParsingDepth)
          decoded              <- ZIO
                                    .attempt(selection.decode(new String(bytes, StandardCharsets.UTF_8)))
                                    .mapError(IntrospectionResponseDecodingFailed(_))
          result               <- ZIO.fromEither(decoded).mapError {
                                    case ServerError(errors) => IntrospectionErrors(errors)
                                    case error               => InvalidIntrospectionResponse(error)
                                  }
          (document, errors, _) = result
          _                    <- ZIO.fail(IntrospectionErrors(errors)).when(errors.nonEmpty)
        } yield document
      }
    }
  }

  private def acquireFederation(
    endpoint: Uri,
    config: RemoteGraphQLConfig.Acquisition,
    backend: SttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Document] = {
    val request = GraphQLRequest(query = Some(ServiceQuery), operationName = Some("__CalibanGatewayServiceSchema"))

    send(endpoint, writeToArray(request), config, backend).flatMap { bytes =>
      for {
        _        <- if (withinJsonDepth(bytes, config.maxParsingDepth)) ZIO.unit
                    else parsingDepthFailure(config.maxParsingDepth)
        decoded  <- ZIO
                      .attempt(readFromArray[ResponseValue](bytes))
                      .mapError(FederationResponseDecodingFailed(_))
        sdl      <- ZIO.fromEither(decodeServiceSdl(decoded))
        _        <- if (withinGraphQLDepth(sdl, config.maxParsingDepth)) ZIO.unit
                    else parsingDepthFailure(config.maxParsingDepth)
        document <- ZIO.fromEither(Parser.parseQuery(sdl)).mapError(InvalidFederationSchema(_))
      } yield document
    }
  }

  private def send(
    endpoint: Uri,
    body: Array[Byte],
    config: RemoteGraphQLConfig.Acquisition,
    backend: SttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Array[Byte]] = {
    val request =
      config.headers.foldLeft(basicRequest.post(endpoint).body(body))((current, header) => current.header(header))

    request
      .contentType("application/json; charset=utf-8")
      .header("Accept", "application/graphql-response+json, application/json;q=0.9")
      .followRedirects(false)
      .response(asStreamAlways(ZioStreams)(readBounded(config.maxResponseBytes)))
      .send(backend)
      .mapError(RequestFailed(_))
      .flatMap { response =>
        if (response.body.limitExceeded)
          ZIO.fail(ResponseTooLarge(config.maxResponseBytes))
        else if (response.code.isRedirect || !allowedMediaType(response))
          ZIO.fail(UnexpectedResponse(response.code, response.contentType))
        else ZIO.succeed(response.body.bytes)
      }
  }

  private def readBounded(
    maxBytes: Int
  )(stream: ZStream[Any, Throwable, Byte])(implicit trace: Trace): Task[BoundedBody] =
    stream
      .take(maxBytes.toLong + 1L)
      .runCollect
      .map { bytes =>
        val array = bytes.toArray
        BoundedBody(array, array.length > maxBytes)
      }

  private def allowedMediaType(response: Response[BoundedBody]): Boolean = {
    val mediaType = response.contentType.map(_.takeWhile(_ != ';').trim.toLowerCase(java.util.Locale.ROOT))
    mediaType.contains("application/graphql-response+json") ||
    response.code.isSuccess && mediaType.contains("application/json")
  }

  private def decodeServiceSdl(value: ResponseValue): Either[SchemaAcquisitionError, String] =
    value match {
      case objectValue: ObjectValue =>
        for {
          errors  <- federationErrors(objectValue)
          _       <- if (errors.isEmpty) Right(()) else Left(FederationErrors(errors))
          data    <- objectField(objectValue, "data", MissingData)
          service <- objectField(data, "_service", MissingService)
          sdl     <- service.fields.collectFirst { case ("sdl", StringValue(value)) => value }
                       .toRight(InvalidFederationResponse(MissingSdl))
        } yield sdl
      case _                        => Left(InvalidFederationResponse(ExpectedResponseObject))
    }

  private def federationErrors(value: ObjectValue): Either[SchemaAcquisitionError, List[CalibanError]] =
    value.fields.collectFirst {
      case ("errors", ResponseValue.ListValue(values)) =>
        val decoded = values.map(CalibanError.fromResponseValue)
        if (decoded.forall(_.nonEmpty)) Right(decoded.flatten)
        else Left(InvalidFederationResponse(InvalidErrors))
      case ("errors", NullValue)                       => Right(Nil)
      case ("errors", _)                               => Left(InvalidFederationResponse(InvalidErrors))
    }.getOrElse(Right(Nil))

  private def objectField(
    value: ObjectValue,
    name: String,
    missing: InvalidFederationResponse.Reason
  ): Either[SchemaAcquisitionError, ObjectValue] =
    value.fields.collectFirst { case (`name`, nested: ObjectValue) => nested }
      .toRight(InvalidFederationResponse(missing))

  private def parsingDepthFailure(maxDepth: Int): IO[SchemaAcquisitionError, Nothing] =
    ZIO.fail(ParsingDepthExceeded(maxDepth))

  private def defaultValuesWithinDepth(value: ResponseValue, maxDepth: Int): Boolean =
    value match {
      case ObjectValue(fields)             =>
        fields.forall {
          case ("defaultValue", StringValue(defaultValue)) =>
            withinGraphQLDepth(defaultValue, maxDepth)
          case (_, nested)                                 => defaultValuesWithinDepth(nested, maxDepth)
        }
      case ResponseValue.ListValue(values) => values.forall(defaultValuesWithinDepth(_, maxDepth))
      case _                               => true
    }

  // Bound parser recursion in schema text embedded inside JSON strings. Syntax validation stays with Parser.
  private def withinGraphQLDepth(value: String, maxDepth: Int): Boolean = {
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

  private def withinJsonDepth(bytes: Array[Byte], maxDepth: Int): Boolean = {
    var depth   = 0
    var index   = 0
    var escaped = false
    var string  = false
    while (index < bytes.length && depth <= maxDepth) {
      bytes(index).toChar match {
        case _ if escaped         => escaped = false
        case '\\' if string       => escaped = true
        case '"'                  => string = !string
        case '{' | '[' if !string => depth += 1
        case '}' | ']' if !string => depth -= 1
        case _                    => ()
      }
      index += 1
    }
    depth <= maxDepth
  }

  private final case class BoundedBody(bytes: Array[Byte], limitExceeded: Boolean)
}
