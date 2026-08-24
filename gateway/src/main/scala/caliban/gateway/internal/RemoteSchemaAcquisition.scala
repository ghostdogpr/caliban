package caliban.gateway.internal

import caliban.ResponseValue.ObjectValue
import caliban.Value.{ NullValue, StringValue }
import caliban.client.Operations.RootQuery
import caliban.client.SelectionBuilder
import caliban.gateway.{ RemoteGraphQLConfig, SchemaInput }
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import caliban.tools.IntrospectionClient
import caliban.{ GraphQLRequest, ResponseValue }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, writeToArray }
import sttp.capabilities.zio.ZioStreams
import sttp.client4._
import sttp.client4.httpclient.zio.SttpClient
import sttp.model.Uri
import zio.stream.ZStream
import zio.{ Chunk, IO, Task, Trace, ZIO }

import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal

private[gateway] object RemoteSchemaAcquisition {

  private val ServiceQuery =
    "query __CalibanGatewayServiceSchema { _service { sdl } }"

  def document(
    input: SchemaInput,
    endpoint: Uri,
    federation: Boolean,
    config: RemoteGraphQLConfig.Acquisition,
    backend: SttpClient
  )(implicit trace: Trace): IO[String, Document] =
    input match {
      case SchemaInput.Sdl(value)    => ZIO.fromEither(Parser.parseQuery(value)).mapError(_.getMessage)
      case SchemaInput.Parsed(value) => ZIO.succeed(value)
      case SchemaInput.Acquired      =>
        acquire(endpoint, federation, config, backend)
    }

  private def acquire(
    endpoint: Uri,
    federation: Boolean,
    config: RemoteGraphQLConfig.Acquisition,
    backend: SttpClient
  )(implicit trace: Trace): IO[String, Document] = {
    val acquisition =
      if (federation) acquireFederation(endpoint, config, backend)
      else acquireIntrospection(endpoint, config, backend)

    acquisition.timeoutFail("Schema acquisition timed out.")(config.timeout)
  }

  private def acquireIntrospection(
    endpoint: Uri,
    config: RemoteGraphQLConfig.Acquisition,
    backend: SttpClient
  )(implicit trace: Trace): IO[String, Document] = {
    implicit val introspectionConfig: IntrospectionClient.Config = IntrospectionClient.Config.default
    val selection: SelectionBuilder[RootQuery, Document]         = IntrospectionClient.introspection
    val request                                                  = selection.toGraphQL(dropNullInputValues = true)

    send(endpoint, writeToArray(request), config, backend).flatMap { bytes =>
      if (!withinJsonDepth(bytes, config.maxParsingDepth)) parsingDepthFailure(config.maxParsingDepth)
      else {
        val array = bytes.toArray
        for {
          response <- ZIO
                        .attemptBlockingInterrupt(readFromArray[ResponseValue](array))
                        .mapError(_ => "Introspection response could not be decoded.")
          _        <- if (defaultValuesWithinDepth(response, config.maxParsingDepth)) ZIO.unit
                      else parsingDepthFailure(config.maxParsingDepth)
          decoded  <- ZIO
                        .attemptBlockingInterrupt(
                          selection.decode(new String(array, StandardCharsets.UTF_8))
                        )
                        .mapError(_ => "Introspection response could not be decoded.")
          document <- ZIO
                        .fromEither(decoded.map(_._1))
                        .mapError(_ => "Introspection response could not be decoded.")
        } yield document
      }
    }
  }

  private def acquireFederation(
    endpoint: Uri,
    config: RemoteGraphQLConfig.Acquisition,
    backend: SttpClient
  )(implicit trace: Trace): IO[String, Document] = {
    val request = GraphQLRequest(query = Some(ServiceQuery), operationName = Some("__CalibanGatewayServiceSchema"))

    send(endpoint, writeToArray(request), config, backend).flatMap { bytes =>
      for {
        _        <- if (withinJsonDepth(bytes, config.maxParsingDepth)) ZIO.unit
                    else parsingDepthFailure(config.maxParsingDepth)
        decoded  <- ZIO
                      .attemptBlockingInterrupt(decodeServiceSdl(bytes))
                      .mapError(_ => "Federation service response was invalid.")
        sdl      <- ZIO.fromEither(decoded).mapError(_ => "Federation service response was invalid.")
        _        <- if (OperationLimits.graphQLNestingWithinLimit(sdl, config.maxParsingDepth)) ZIO.unit
                    else parsingDepthFailure(config.maxParsingDepth)
        parsed   <- ZIO
                      .attemptBlockingInterrupt(Parser.parseQuery(sdl))
                      .mapError(_ => "Federation service schema could not be parsed.")
        document <- ZIO
                      .fromEither(parsed)
                      .mapError(error => s"Federation service schema could not be parsed: ${error.getMessage}")
      } yield document
    }
  }

  private def send(
    endpoint: Uri,
    body: Array[Byte],
    config: RemoteGraphQLConfig.Acquisition,
    backend: SttpClient
  )(implicit trace: Trace): IO[String, Chunk[Byte]] = {
    val request = config.headers.foldLeft(
      basicRequest
        .post(endpoint)
        .body(body)
    )((current, header) => current.header(header))

    request
      .contentType("application/json; charset=utf-8")
      .header("Accept", "application/graphql-response+json, application/json;q=0.9")
      .followRedirects(false)
      .response(asStreamAlways(ZioStreams)(readBounded(config.maxResponseBytes)))
      .send(backend)
      .mapError(_ => "Schema acquisition request failed.")
      .flatMap { response =>
        if (response.body.limitExceeded)
          ZIO.fail(s"Schema acquisition response exceeded ${config.maxResponseBytes} bytes.")
        else if (response.code.isRedirect)
          ZIO.fail("Schema acquisition response had an unsupported status or media type.")
        else if (!allowedMediaType(response))
          ZIO.fail("Schema acquisition response had an unsupported status or media type.")
        else ZIO.succeed(response.body.bytes)
      }
  }

  private def readBounded(maxBytes: Int)(stream: ZStream[Any, Throwable, Byte])(implicit
    trace: Trace
  ): Task[BoundedBody] =
    stream
      .take(maxBytes.toLong + 1L)
      .runCollect
      .map(bytes => BoundedBody(bytes, bytes.length > maxBytes))

  private def allowedMediaType(response: Response[BoundedBody]): Boolean = {
    val mediaType = response.contentType.map(_.takeWhile(_ != ';').trim.toLowerCase(java.util.Locale.ROOT))
    mediaType.contains("application/graphql-response+json") ||
    response.code.isSuccess && mediaType.contains("application/json")
  }

  private def decodeServiceSdl(bytes: Chunk[Byte]): Either[Unit, String] =
    try
      readFromArray[ResponseValue](bytes.toArray) match {
        case value: ObjectValue if !hasErrors(value) =>
          for {
            data    <- objectField(value, "data")
            service <- objectField(data, "_service")
            sdl     <- service.fields.collectFirst { case ("sdl", StringValue(value)) => value }.toRight(())
          } yield sdl
        case _                                       => Left(())
      }
    catch {
      case NonFatal(_) => Left(())
    }

  private def objectField(value: ObjectValue, name: String): Either[Unit, ObjectValue] =
    value.fields.collectFirst { case (`name`, nested: ObjectValue) => nested }.toRight(())

  private def hasErrors(value: ObjectValue): Boolean =
    value.fields.collectFirst {
      case ("errors", ResponseValue.ListValue(errors)) => errors.nonEmpty
      case ("errors", NullValue)                       => false
      case ("errors", _)                               => true
    }.getOrElse(false)

  private def parsingDepthFailure(maxDepth: Int): IO[String, Nothing] =
    ZIO.fail(s"Schema acquisition parsing depth exceeded $maxDepth.")

  private def defaultValuesWithinDepth(value: ResponseValue, maxDepth: Int): Boolean =
    value match {
      case ObjectValue(fields)             =>
        fields.forall {
          case ("defaultValue", StringValue(defaultValue)) =>
            OperationLimits.graphQLNestingWithinLimit(defaultValue, maxDepth)
          case (_, nested)                                 => defaultValuesWithinDepth(nested, maxDepth)
        }
      case ResponseValue.ListValue(values) => values.forall(defaultValuesWithinDepth(_, maxDepth))
      case _                               => true
    }

  private def withinJsonDepth(bytes: Chunk[Byte], maxDepth: Int): Boolean = {
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

  private final case class BoundedBody(bytes: Chunk[Byte], limitExceeded: Boolean)
}
