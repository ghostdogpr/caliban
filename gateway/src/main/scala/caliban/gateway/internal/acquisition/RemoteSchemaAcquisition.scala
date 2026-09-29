package caliban.gateway.internal.acquisition

import caliban.{ CalibanError, GraphQLRequest, ResponseValue }
import caliban.CalibanError.ParsingError
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ NullValue, StringValue }
import caliban.gateway._
import caliban.gateway.SchemaAcquisitionError._
import caliban.gateway.Subgraph.SchemaInput
import caliban.gateway.internal.{ GatewayHttpClient, RemoteTransport }
import caliban.gateway.internal.GatewayHttpClient.Reply
import caliban.parsing.adt.Document
import caliban.parsing.Parser
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, writeToArray }
import zio.{ IO, Task, Trace, ZIO }
import zio.http.{ Header, QueryParams, Scheme, Status, URL }

import java.net.URI
import java.util.Locale
import scala.util.Try

private[gateway] object RemoteSchemaAcquisition {

  def load(remote: Subgraph.Source.Remote[_], http: GatewayHttpClient)(implicit
    trace: Trace
  ): IO[SubgraphAcquisitionError, Document] =
    remote.schema match {
      case SchemaInput.Pinned(document) => ZIO.fromEither(document)
      case SchemaInput.Acquired(config) =>
        val acquisition =
          if (remote.federation) FederationClient.fetch(remote.endpoint, config, http)
          else IntrospectionClient.fetch(remote.endpoint, config, http)

        acquisition.timeoutFail(TimedOut(config.timeout))(config.timeout)
    }

  /**
   * Posts `request` and returns the `data` object when `accepts` its status and it fits the size and depth limits.
   * A response carrying GraphQL errors fails with `onErrors`.
   */
  private[acquisition] def fetchData[E >: SchemaAcquisitionError](
    endpoint: URL,
    request: GraphQLRequest,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient,
    scope: RedirectScope
  )(accepts: Status => Boolean, onErrors: List[CalibanError] => E)(implicit
    trace: Trace
  ): IO[E, ObjectValue] =
    followRedirects(endpoint, config, scope)(http.post(_, writeToArray(request), _, config.maxResponseBytes)).flatMap {
      reply =>
        reply.body match {
          case None        => ZIO.fail(ResponseTooLarge(config.maxResponseBytes))
          case Some(bytes) =>
            if (!accepts(reply.status) || !RemoteTransport.isJsonResponse(reply.status, reply.contentType))
              ZIO.fail(UnexpectedResponse(reply.status, reply.contentType))
            else if (!RemoteTransport.withinJsonDepth(bytes, config.maxParsingDepth))
              ZIO.fail(ParsingDepthExceeded(config.maxParsingDepth))
            else ZIO.attempt(readFromArray[ResponseValue](bytes)).mapError(ResponseDecodingFailed(_))
        }
    }.flatMap { response =>
      ZIO.fromEither(for {
        envelope <- asObject(response, "$")
        errors   <- responseErrors(envelope)
        _        <- if (errors.isEmpty) Right(()) else Left(onErrors(errors))
        data     <- objectField(envelope, "data", "$")
      } yield data)
    }

  /**
   * The redirects an acquisition request follows. Neither scope goes from https to http. `SameOrigin` is for a
   * request body that carries a credential, such as Uplink's API key.
   */
  private[acquisition] sealed trait RedirectScope

  private[acquisition] object RedirectScope {
    case object AnyOrigin  extends RedirectScope
    case object SameOrigin extends RedirectScope
  }

  // A redirect it does not follow is returned as is, for the caller's status check to reject.
  private[acquisition] def followRedirects(
    endpoint: URL,
    config: RemoteGraphQLConfig.Acquisition,
    scope: RedirectScope
  )(
    send: (URL, List[Header]) => Task[Reply]
  )(implicit trace: Trace): IO[SchemaAcquisitionError, Reply] = {
    def loop(url: URL, redirects: Int): IO[SchemaAcquisitionError, Reply] =
      // Redirect targets never receive configured headers, which may contain CDN credentials.
      send(url, if (redirects == 0) config.headers else Nil).mapError(RequestFailed(_)).flatMap { reply =>
        val target =
          if (!reply.status.isRedirection || reply.status == Status.NotModified || redirects >= config.maxRedirects)
            None
          else reply.headers.rawHeader(Header.Location).flatMap(resolveRedirect(url, _)).filter(follows(scope, url, _))
        target.fold[IO[SchemaAcquisitionError, Reply]](ZIO.succeed(reply))(loop(_, redirects + 1))
      }

    loop(endpoint, 0)
  }

  private def follows(scope: RedirectScope, from: URL, to: URL): Boolean =
    scope match {
      case RedirectScope.AnyOrigin  => !from.scheme.contains(Scheme.HTTPS) || to.scheme.contains(Scheme.HTTPS)
      case RedirectScope.SameOrigin => origin(from) == origin(to)
    }

  private def origin(url: URL): (Option[Scheme], Option[String], Option[Int]) =
    (url.scheme, url.host.map(_.toLowerCase(Locale.ROOT)), url.portOrDefault)

  private def resolveRedirect(base: URL, location: String): Option[URL] =
    Try(new URI(location)).toOption.flatMap { reference =>
      // Keep the resource path for query-only redirects; URI.resolve would drop its final segment.
      if (reference.getScheme == null && reference.getRawAuthority == null && reference.getRawPath.isEmpty)
        Some(
          base.copy(
            queryParams =
              Option(reference.getRawQuery).filter(_.nonEmpty).fold(base.queryParams)(QueryParams.decode(_)),
            fragment = None
          )
        )
      else Try(base.toJavaURI.resolve(reference)).toOption.flatMap(URL.fromURI)
    }

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
    asString(obj.getOrNull(field), s"$path.$field")

  private[acquisition] def asString(value: ResponseValue, path: String): Either[InvalidResponse, String] =
    value match {
      case StringValue(value) => Right(value)
      case _                  => Left(InvalidResponse(path))
    }

  private[gateway] def parseSdl(sdl: String): Either[SchemaParsingFailed, Document] =
    Parser.parseQuery(sdl).left.map(SchemaParsingFailed(_))

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
