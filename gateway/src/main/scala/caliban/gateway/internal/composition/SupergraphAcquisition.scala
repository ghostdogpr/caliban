package caliban.gateway.internal.composition

import caliban.gateway.SupergraphAcquisitionError._
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.composition.ApolloUplinkClient.UplinkResponse
import caliban.gateway.{ RemoteGraphQLConfig, Supergraph, SupergraphAcquisitionError, SupergraphUplinkConfig }
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import zio.{ IO, NonEmptyChunk, Ref, Trace, UIO, ZIO }
import zio.http.{ Header, QueryParams, Status, URL }

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }
import scala.util.Try

/**
 * Loads the supergraph document a gateway is built from.
 *
 * A loader is reused across reloads. Refreshable sources must observe changes at the source.
 */
private[gateway] object SupergraphAcquisition {
  trait Loader {
    def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document]
  }

  def make(source: Supergraph.Source, http: Option[GatewayHttpClient]): UIO[Loader] =
    source match {
      case Supergraph.Source.Sdl(value)             => ZIO.succeed(constant(parse(value)))
      case Supergraph.Source.Parsed(value)          => ZIO.succeed(constant(Right(value)))
      case Supergraph.Source.File(path)             => ZIO.succeed(file(path))
      case Supergraph.Source.Http(endpoint, config) => httpClientOrDie(http).flatMap(remote(endpoint, config, _))
      case Supergraph.Source.Uplink(config)         => httpClientOrDie(http).flatMap(uplink(config, _))
    }

  /**
   * Static SDL is parsed once when the loader is created; any parse error is reported by `load`.
   */
  private def constant(result: Either[SupergraphAcquisitionError, Document]): Loader =
    new Loader {
      def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] = ZIO.fromEither(result)
    }

  /**
   * Re-read on every load so the next reload picks up a supergraph replaced on disk.
   */
  private def file(path: Path): Loader =
    new Loader {
      def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] =
        ZIO
          .attemptBlocking(new String(Files.readAllBytes(path), StandardCharsets.UTF_8))
          .mapError(FileUnreadable(_))
          .flatMap(value => ZIO.fromEither(parse(value)))
    }

  /**
   * Conditional requests reuse the last fetched document, even if it failed to compose. The caller
   * must be able to retry that build while an older document is still active.
   *
   * Keep the initial endpoint's ETag across redirects and send it only to that endpoint. A storage
   * server's ETag does not identify the same artifact at the CDN.
   */
  private def remote(endpoint: URL, config: RemoteGraphQLConfig.Acquisition, http: GatewayHttpClient): UIO[Loader] =
    Ref.make(HttpState(None, None)).map { cache =>
      new Loader {
        def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] = {
          def loop(url: URL, redirects: Int, initialEtag: Option[String]): IO[SupergraphAcquisitionError, Document] =
            cache.get.flatMap { state =>
              val initialRequest = redirects == 0
              // Redirect targets never receive configured headers, which may contain CDN credentials.
              val headers        =
                (if (initialRequest) config.headers else Nil) :::
                  Header.Custom("Accept", "application/graphql, text/plain;q=0.9") ::
                  state.etag.filter(_ => initialRequest).map(tag => Header.IfNoneMatch.ETags(NonEmptyChunk(tag))).toList

              http
                .get(url, headers, config.maxResponseBytes)
                .mapError[SupergraphAcquisitionError](RequestFailed(_))
                .timeoutFail(TimedOut(config.timeout))(config.timeout)
                .flatMap { reply =>
                  // Keep only the initial endpoint's ETag; a redirect target's tag is ignored.
                  val tag        = if (initialRequest) reply.response.rawHeader(Header.ETag) else initialEtag
                  val unexpected = UnexpectedResponse(reply.status, reply.contentType)

                  if (reply.body.limitExceeded) ZIO.fail(ResponseTooLarge(config.maxResponseBytes))
                  else if (reply.status == Status.NotModified)
                    // Only the initial endpoint received If-None-Match; a redirected 304 is invalid.
                    if (initialRequest) ZIO.fromOption(state.document).orElseFail(unexpected)
                    else ZIO.fail(unexpected)
                  else if (reply.status.isRedirection && redirects < config.maxRedirects)
                    ZIO
                      .fromOption(reply.response.rawHeader(Header.Location).flatMap(resolveRedirect(url, _)))
                      .orElseFail(unexpected)
                      .flatMap(location => loop(location, redirects + 1, tag))
                  else if (reply.status.isRedirection || !isSdlResponse(reply.status, reply.contentType))
                    ZIO.fail(unexpected)
                  else
                    // Save the tag only with a parsed document, so a later 304 can be answered.
                    parseWithinDepth(new String(reply.body.bytes, StandardCharsets.UTF_8), config.maxParsingDepth)
                      .tap(document => cache.set(HttpState(tag, Some(document))))
                }
            }

          loop(endpoint, 0, None)
        }
      }
    }

  /**
   * Try endpoints in order, each with its own timeout. Only request failures, unexpected HTTP
   * responses, and timeouts allow failover; service errors and invalid schemas stop the load.
   *
   * Every attempt uses the same cursor. Advance it only after fetching and parsing a new document,
   * and retain that document even if the caller cannot activate it.
   */
  private def uplink(config: SupergraphUplinkConfig, http: GatewayHttpClient): UIO[Loader] =
    Ref.make(UplinkState(None, None)).map { cache =>
      new Loader {
        def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] =
          cache.get.flatMap(attempt(config.endpoints, _))

        private def attempt(endpoints: List[URL], state: UplinkState)(implicit
          trace: Trace
        ): IO[SupergraphAcquisitionError, Document] =
          endpoints match {
            // Configuration validation rejects an empty list before the loader is created.
            case Nil                  =>
              ZIO.die(new IllegalStateException("An uplink supergraph source requires at least one endpoint."))
            case endpoint :: Nil      => acquire(endpoint, state)
            case endpoint :: fallback =>
              acquire(endpoint, state).catchSome { case _: RequestFailed | _: UnexpectedResponse | _: TimedOut =>
                attempt(fallback, state)
              }
          }

        private def acquire(endpoint: URL, state: UplinkState)(implicit
          trace: Trace
        ): IO[SupergraphAcquisitionError, Document] = {
          val acquisition = config.acquisition

          ApolloUplinkClient
            .fetch(endpoint, config, state.cursor, http)
            .flatMap {
              case UplinkResponse.Success(id, Some(sdl), _) =>
                parseWithinDepth(sdl, acquisition.maxParsingDepth)
                  .tap(document => cache.set(UplinkState(Some(id), Some(document))))
              case UplinkResponse.Success(_, None, _)       =>
                // An unchanged response answers a cursor we sent, so an empty cache means the server
                // answered one we never stored. Fail without advancing the cursor.
                ZIO
                  .fromOption(state.document)
                  .orElseFail(InvalidUplinkResponse(InvalidUplinkResponse.MissingSupergraphSdl))
              case UplinkResponse.Failure(code, _)          =>
                // The code is a fixed enum and safe to render; the message beside it is remote free text.
                ZIO.fail(UplinkFetchFailed(code))
            }
            .timeoutFail(TimedOut(acquisition.timeout))(acquisition.timeout)
        }
      }
    }

  // The caller chooses whether to create a client, so its absence is a wiring defect.
  private def httpClientOrDie(http: Option[GatewayHttpClient]): UIO[GatewayHttpClient] =
    ZIO
      .fromOption(http)
      .orDieWith(_ => new IllegalStateException("A remote supergraph source requires an HTTP client."))

  private def parse(value: String): Either[SupergraphAcquisitionError, Document] =
    Parser.parseQuery(value).left.map(InvalidSupergraphSchema(_))

  private def parseWithinDepth(sdl: String, maxDepth: Int)(implicit
    trace: Trace
  ): IO[SupergraphAcquisitionError, Document] =
    if (RemoteSchemaAcquisition.withinGraphQLDepth(sdl, maxDepth)) ZIO.fromEither(parse(sdl))
    else ZIO.fail(ParsingDepthExceeded(maxDepth))

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
   * SDL servers may omit Content-Type or use application/octet-stream. Reject HTML login/error
   * pages explicitly; let the parser validate other successful responses.
   */
  private def isSdlResponse(status: Status, contentType: Option[String]): Boolean =
    status.isSuccess && !RemoteSchemaAcquisition.isHtml(contentType)

  private final case class HttpState(etag: Option[String], document: Option[Document])
  private final case class UplinkState(cursor: Option[String], document: Option[Document])
}
