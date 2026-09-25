package caliban.gateway.internal.acquisition

import caliban.{ CalibanError, GraphQLRequest, ResponseValue }
import caliban.CalibanError.ParsingError
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ NullValue, StringValue }
import caliban.gateway._
import caliban.gateway.SchemaAcquisitionError._
import caliban.gateway.Subgraph.SchemaInput
import caliban.gateway.internal.{ GatewayHttpClient, RemoteTransport }
import caliban.parsing.adt.Document
import caliban.parsing.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, writeToArray }
import zio.{ IO, Trace, ZIO }
import zio.http.URL

private[gateway] object RemoteSchemaAcquisition {

  def load(remote: Subgraph.Source.Remote[_], http: GatewayHttpClient)(implicit
    trace: Trace
  ): IO[SubgraphAcquisitionError, Document] =
    remote.schema match {
      case SchemaInput.Sdl(value)    => ZIO.fromEither(Parser.parseQuery(value)).mapError(SchemaParsingFailed(_))
      case SchemaInput.Parsed(value) => ZIO.succeed(value)
      case SchemaInput.Acquired      =>
        val config      = remote.config.acquisition
        val acquisition =
          if (remote.federation) FederationClient.fetch(remote.endpoint, config, http)
          else IntrospectionClient.fetch(remote.endpoint, config, http)

        acquisition.timeoutFail(TimedOut(config.timeout))(config.timeout)
    }

  /**
   * Posts `request` and returns the `data` object when `accepts` the reply and it fits the size and depth limits.
   * A response carrying GraphQL errors fails with `onErrors`.
   */
  private[acquisition] def fetchData[E >: SchemaAcquisitionError](
    endpoint: URL,
    request: GraphQLRequest,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(accepts: GatewayHttpClient.Reply => Boolean, onErrors: List[CalibanError] => E)(implicit
    trace: Trace
  ): IO[E, ObjectValue] =
    http
      .post(endpoint, writeToArray(request), config.headers, config.maxResponseBytes)
      .mapError[SchemaAcquisitionError](RequestFailed(_))
      .flatMap { reply =>
        if (reply.body.limitExceeded)
          ZIO.fail(ResponseTooLarge(config.maxResponseBytes))
        else if (!accepts(reply))
          ZIO.fail(UnexpectedResponse(reply.status, reply.contentType))
        else if (!RemoteTransport.withinJsonDepth(reply.body.bytes, config.maxParsingDepth))
          ZIO.fail(ParsingDepthExceeded(config.maxParsingDepth))
        else ZIO.attempt(readFromArray[ResponseValue](reply.body.bytes)).mapError(ResponseDecodingFailed(_))
      }
      .flatMap { response =>
        ZIO.fromEither(for {
          envelope <- asObject(response, "$")
          errors   <- responseErrors(envelope)
          _        <- if (errors.isEmpty) Right(()) else Left(onErrors(errors))
          data     <- objectField(envelope, "data", "$")
        } yield data)
      }

  private[acquisition] def isGraphQLResponse(reply: GatewayHttpClient.Reply): Boolean =
    !reply.status.isRedirection && RemoteTransport.isJsonResponse(reply.status, reply.contentType)

  /**
   * Fails for malformed errors, or returns Nil when the response has no errors.
   */
  private def responseErrors(value: ObjectValue): Either[InvalidResponse, List[CalibanError]] =
    value.getOrNull("errors") match {
      case null | NullValue => Right(Nil)
      case ListValue(items) =>
        traverseOption(items)(CalibanError.fromResponseValue).toRight(InvalidResponse("$.errors"))
      case _                => Left(InvalidResponse("$.errors"))
    }

  private[acquisition] def asObject(value: ResponseValue, path: String): Either[InvalidResponse, ObjectValue] =
    value match {
      case obj: ObjectValue => Right(obj)
      case _                => Left(InvalidResponse(path))
    }

  private[acquisition] def objectField(
    obj: ObjectValue,
    field: String,
    path: String
  ): Either[InvalidResponse, ObjectValue] =
    asObject(obj.getOrNull(field), s"$path.$field")

  private[acquisition] def string(obj: ObjectValue, field: String, path: String): Either[InvalidResponse, String] =
    obj.getOrNull(field) match {
      case StringValue(value) => Right(value)
      case _                  => Left(InvalidResponse(s"$path.$field"))
    }

  /**
   * Parses schema text embedded in a response after checking its nesting against `maxDepth`.
   */
  private[acquisition] def parseWithinDepth[A](text: String, maxDepth: Int)(
    parse: String => Either[ParsingError, A]
  ): Either[SchemaAcquisitionError, A] =
    if (withinGraphQLDepth(text, maxDepth)) parse(text).left.map(SchemaParsingFailed(_))
    else Left(ParsingDepthExceeded(maxDepth))

  // Bound parser recursion in schema text embedded inside JSON strings. Syntax validation stays with Parser.
  private def withinGraphQLDepth(value: String, maxDepth: Int): Boolean = {
    var index           = 0
    var depth           = 0
    var stringDelimiter = ""
    var inComment       = false
    while (index < value.length && depth <= maxDepth) {
      val current = value.charAt(index)
      if (inComment) {
        if (current == '\n' || current == '\r') inComment = false
      } else if (stringDelimiter.nonEmpty) {
        if (stringDelimiter == "\"" && current == '\\') index += 1
        else if (stringDelimiter.length == 3 && value.startsWith("\\\"\"\"", index)) index += 3
        else if (value.startsWith(stringDelimiter, index)) {
          index += stringDelimiter.length - 1
          stringDelimiter = ""
        }
      } else
        current match {
          case '#'             => inComment = true
          case '"'             =>
            stringDelimiter = if (value.startsWith("\"\"\"", index)) "\"\"\"" else "\""
            index += stringDelimiter.length - 1
          case '{' | '[' | '(' => depth += 1
          case '}' | ']' | ')' => depth = math.max(0, depth - 1)
          case _               => ()
        }
      index += 1
    }
    depth <= maxDepth
  }
}
