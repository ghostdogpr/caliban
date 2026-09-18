package caliban.gateway.internal.composition

import caliban.{ CalibanError, GraphQLRequest }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.NullValue
import caliban.gateway.{ RemoteGraphQLConfig, SchemaAcquisitionError, SchemaInput, Subgraph }
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.execution.RemoteTransport
import caliban.gateway.SchemaAcquisitionError._
import caliban.parsing.adt.Document
import caliban.parsing.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import zio.{ IO, Trace, ZIO }
import zio.http.URL

private[gateway] object RemoteSchemaAcquisition {

  def load(remote: Subgraph.Source.Remote[_], http: GatewayHttpClient)(implicit
    trace: Trace
  ): IO[SchemaAcquisitionError, Document] =
    remote.schema match {
      case SchemaInput.Sdl(value)    => ZIO.fromEither(Parser.parseQuery(value)).mapError(InvalidProvidedSchema(_))
      case SchemaInput.Parsed(value) => ZIO.succeed(value)
      case SchemaInput.Acquired      =>
        val config      = remote.config.acquisition
        val acquisition =
          if (remote.federation) FederationClient.fetch(remote.endpoint, config, http)
          else IntrospectionClient.fetch(remote.endpoint, config, http)

        acquisition.timeoutFail(TimedOut(config.timeout))(config.timeout)
    }

  private[composition] def fetchBytes(
    endpoint: URL,
    query: String,
    operationName: String,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Array[Byte]] = {
    val request = GraphQLRequest(query = Some(query), operationName = Some(operationName))

    http
      .post(endpoint, writeToArray(request), config.headers, config.maxResponseBytes)
      .mapError[SchemaAcquisitionError](RequestFailed(_))
      .flatMap(validateResponse(_, config))
  }

  private def validateResponse(reply: GatewayHttpClient.Reply, config: RemoteGraphQLConfig.Acquisition)(implicit
    trace: Trace
  ): IO[SchemaAcquisitionError, Array[Byte]] =
    if (reply.body.limitExceeded)
      ZIO.fail(ResponseTooLarge(config.maxResponseBytes))
    else if (reply.status.isRedirection || !isJsonResponse(reply))
      ZIO.fail(UnexpectedResponse(reply.status, reply.contentType))
    else
      ZIO
        .fromEither(RemoteTransport.validateJsonStructure(reply.body.bytes, config.maxParsingDepth, Int.MaxValue))
        .mapError(_ => ParsingDepthExceeded(config.maxParsingDepth))
        .as(reply.body.bytes)

  /**
   * Returns None for malformed errors, or Some(Nil) when the response has no errors.
   */
  private[composition] def responseErrors(value: ObjectValue): Option[List[CalibanError]] =
    value.getOrNull("errors") match {
      case null | NullValue => Some(Nil)
      case ListValue(items) =>
        val decoded = items.map(CalibanError.fromResponseValue)
        if (decoded.forall(_.nonEmpty)) Some(decoded.flatten) else None
      case _                => None
    }

  // Bound parser recursion in schema text embedded inside JSON strings. Syntax validation stays with Parser.
  private[composition] def withinGraphQLDepth(value: String, maxDepth: Int): Boolean = {
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

  private[composition] def isHtml(contentType: Option[String]): Boolean =
    RemoteTransport.mediaType(contentType).exists(_.startsWith("text/html"))

  private def isJsonResponse(reply: GatewayHttpClient.Reply): Boolean = {
    val mediaType = RemoteTransport.mediaType(reply.contentType)
    // GraphQL response JSON can carry errors on non-success statuses; ordinary JSON requires success.
    mediaType.contains("application/graphql-response+json") ||
    reply.status.isSuccess && mediaType.contains("application/json")
  }
}
