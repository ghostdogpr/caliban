package caliban.gateway.internal.acquisition

import caliban.gateway.SchemaAcquisitionError._
import caliban.gateway.SupergraphAcquisitionError.{ FileReadFailed, UplinkFetchFailed }
import caliban.gateway.internal.{ GatewayHttpClient, RemoteTransport }
import caliban.gateway.internal.acquisition.ApolloUplinkClient.UplinkResponse
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

  def make(source: Supergraph.Source, http: GatewayHttpClient)(implicit
    trace: Trace
  ): UIO[IO[SupergraphAcquisitionError, Document]] =
    source match {
      case Supergraph.Source.Sdl(value)             => ZIO.succeed(constant(parse(value)))
      case Supergraph.Source.Parsed(value)          => ZIO.succeed(constant(Right(value)))
      case Supergraph.Source.File(path)             => ZIO.succeed(file(path))
      case Supergraph.Source.Http(endpoint, config) => remote(endpoint, config, http)
      case Supergraph.Source.Uplink(config)         => uplink(config, http)
    }

  /**
   * Static SDL is parsed once when the loader is created; any parse error is reported by every load.
   */
  private def constant(result: Either[SupergraphAcquisitionError, Document])(implicit
    trace: Trace
  ): IO[SupergraphAcquisitionError, Document] =
    ZIO.fromEither(result)

  /**
   * Re-read on every load so the next reload picks up a supergraph replaced on disk.
   */
  private def file(path: Path)(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] =
    ZIO
      .attemptBlocking(new String(Files.readAllBytes(path), StandardCharsets.UTF_8))
      .mapError(FileReadFailed(_))
      .flatMap(value => ZIO.fromEither(parse(value)))

  /**
   * Conditional requests reuse the last fetched document, even if it failed to compose. The caller
   * must be able to retry that build while an older document is still active.
   */
  private def remote(endpoint: URL, config: RemoteGraphQLConfig.Acquisition, http: GatewayHttpClient)(implicit
    trace: Trace
  ): UIO[IO[SupergraphAcquisitionError, Document]] =
    Ref.make(Option.empty[Cached]).map { cache =>
      def loop(url: URL, redirects: Int, cached: Option[Cached]): IO[SupergraphAcquisitionError, Document] = {
        // Redirect targets never receive configured headers, which may contain CDN credentials.
        val headers =
          (if (redirects == 0) config.headers else Nil) :::
            Header.Custom("Accept", "application/graphql, text/plain;q=0.9") ::
            cached.flatMap(_.tag).map(tag => Header.IfNoneMatch.ETags(NonEmptyChunk(tag))).toList

        http
          .get(url, headers, config.maxResponseBytes)
          .mapError[SupergraphAcquisitionError](RequestFailed(_))
          .flatMap { reply =>
            val unexpected = UnexpectedResponse(reply.status, reply.contentType)

            if (reply.body.limitExceeded) ZIO.fail(ResponseTooLarge(config.maxResponseBytes))
            else if (reply.status == Status.NotModified) ZIO.fromOption(cached.map(_.document)).orElseFail(unexpected)
            else if (reply.status.isRedirection && redirects < config.maxRedirects)
              ZIO
                .fromOption(reply.response.rawHeader(Header.Location).flatMap(resolveRedirect(url, _)))
                .orElseFail(unexpected)
                .flatMap(location => loop(location, redirects + 1, cached))
            else if (!isSdlResponse(reply.status, reply.contentType))
              ZIO.fail(unexpected)
            else
              // Save the tag only with a parsed document, so a later 304 can be answered.
              parseRemote(new String(reply.body.bytes, StandardCharsets.UTF_8), config.maxParsingDepth)
                .tap(document => cache.set(Some(Cached(reply.response.rawHeader(Header.ETag), document))))
          }
      }

      cache.get.flatMap(loop(endpoint, 0, _)).timeoutFail(TimedOut(config.timeout))(config.timeout)
    }

  /**
   * Try endpoints in order, each with its own timeout. Only request failures, unexpected HTTP
   * responses, and timeouts allow failover; service errors and invalid schemas stop the load.
   *
   * Every attempt uses the same cursor. Advance it only after fetching and parsing a new document,
   * and retain that document even if the caller cannot activate it.
   */
  private def uplink(config: SupergraphUplinkConfig, http: GatewayHttpClient)(implicit
    trace: Trace
  ): UIO[IO[SupergraphAcquisitionError, Document]] =
    Ref.make(Option.empty[Cached]).map { cache =>
      val acquisition = config.acquisition

      def acquire(endpoint: URL, cached: Option[Cached]): IO[SupergraphAcquisitionError, Document] =
        ApolloUplinkClient
          .fetch(endpoint, config, cached.flatMap(_.tag), http)
          .flatMap {
            case UplinkResponse.Updated(id, sdl) =>
              parseRemote(sdl, acquisition.maxParsingDepth)
                .tap(document => cache.set(Some(Cached(Some(id), document))))
            case UplinkResponse.Unchanged        =>
              // An unchanged response answers a cursor we sent, so an empty cache means the server
              // answered one we never stored. Fail without advancing the cursor.
              ZIO
                .fromOption(cached.map(_.document))
                .orElseFail(InvalidResponse("$.data.routerConfig.supergraphSDL"))
            case UplinkResponse.Failure(code)    =>
              // The code is a fixed enum and safe to render; the message beside it is remote free text.
              ZIO.fail(UplinkFetchFailed(code))
          }
          .timeoutFail(TimedOut(acquisition.timeout))(acquisition.timeout)

      cache.get.flatMap { cached =>
        // Configuration validation rejects an empty list before the loader is created.
        config.endpoints.map(acquire(_, cached)).reduceLeft { (attempt, fallback) =>
          attempt.catchSome { case _: RequestFailed | _: UnexpectedResponse | _: TimedOut => fallback }
        }
      }
    }

  private def parse(value: String): Either[SupergraphAcquisitionError, Document] =
    Parser.parseQuery(value).left.map(SchemaParsingFailed(_))

  private def parseRemote(sdl: String, maxDepth: Int)(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] =
    ZIO.fromEither(RemoteSchemaAcquisition.parseWithinDepth(sdl, maxDepth)(Parser.parseQuery))

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
    status.isSuccess && !RemoteTransport.mediaType(contentType).exists(_.startsWith("text/html"))

  // The ETag or uplink id of the last fetched document.
  private final case class Cached(tag: Option[String], document: Document)
}
