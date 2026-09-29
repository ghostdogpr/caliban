package caliban.gateway.internal.acquisition

import caliban.gateway.SchemaAcquisitionError._
import caliban.gateway.SupergraphAcquisitionError.FileReadFailed
import caliban.gateway.internal.{ GatewayHttpClient, RemoteTransport }
import caliban.gateway.internal.acquisition.ApolloUplinkClient.UplinkResponse
import caliban.gateway.internal.acquisition.RemoteSchemaAcquisition._
import caliban.gateway.{ RemoteGraphQLConfig, Supergraph, SupergraphAcquisitionError, SupergraphUplinkConfig }
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import zio.{ IO, NonEmptyChunk, Ref, Trace, UIO, ZIO }
import zio.http.{ Header, Status, URL }

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }

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
      case Supergraph.Source.Sdl(value)             => ZIO.succeed(constant(parseSdl(value)))
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
      .flatMap(value => ZIO.fromEither(parseSdl(value)))

  /**
   * Conditional requests reuse the last fetched document, even if it failed to compose. The caller
   * must be able to retry that build while an older document is still active.
   */
  private def remote(
    endpoint: URL,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): UIO[IO[SupergraphAcquisitionError, Document]] =
    Ref.make(Option.empty[Cached]).map { cache =>
      def load(cached: Option[Cached]): IO[SupergraphAcquisitionError, Document] = {
        val headers =
          Header.Custom("Accept", "application/graphql, text/plain;q=0.9") ::
            cached.map(entry => Header.IfNoneMatch.ETags(NonEmptyChunk(entry.tag))).toList

        followRedirects(endpoint, config, RedirectScope.AnyOrigin)((url, configured) =>
          http.get(url, configured ::: headers, config.maxResponseBytes)
        ).flatMap { reply =>
          val unexpected = UnexpectedResponse(reply.status, reply.contentType)

          reply.body match {
            case None        => ZIO.fail(ResponseTooLarge(config.maxResponseBytes))
            case Some(bytes) =>
              if (reply.status == Status.NotModified) ZIO.fromOption(cached.map(_.document)).orElseFail(unexpected)
              else if (!isSdlResponse(reply.status, reply.contentType))
                ZIO.fail(unexpected)
              else
                // Save the tag only with a parsed document, so a later 304 can be answered.
                parseRemote(new String(bytes, StandardCharsets.UTF_8), config.maxParsingDepth)
                  .tap(document => cache.set(reply.headers.rawHeader(Header.ETag).map(Cached(_, document))))
          }
        }
      }

      cache.get.flatMap(load).timeoutFail(TimedOut(config.timeout))(config.timeout)
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
          .fetch(endpoint, config, cached.map(_.tag), http)
          .flatMap {
            case UplinkResponse.Updated(id, sdl) =>
              parseRemote(sdl, acquisition.maxParsingDepth)
                .tap(document => cache.set(Some(Cached(id, document))))
            case UplinkResponse.Unchanged        =>
              // An unchanged response answers a cursor we sent, so an empty cache means the server
              // answered one we never stored. Fail without advancing the cursor.
              ZIO
                .fromOption(cached.map(_.document))
                .orElseFail(InvalidResponse("$.data.routerConfig.supergraphSDL"))
          }
          .timeoutFail(TimedOut(acquisition.timeout))(acquisition.timeout)

      cache.get.flatMap { cached =>
        config.endpoints.tail.foldLeft(acquire(config.endpoints.head, cached)) { (attempt, fallback) =>
          attempt.catchSome { case _: RequestFailed | _: UnexpectedResponse | _: TimedOut => acquire(fallback, cached) }
        }
      }
    }

  private def parseRemote(sdl: String, maxDepth: Int)(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] =
    ZIO.fromEither(parseWithinDepth(sdl, maxDepth)(Parser.parseQuery))

  /**
   * SDL servers may omit Content-Type or use application/octet-stream. Reject HTML login/error
   * pages explicitly; let the parser validate other successful responses.
   */
  private def isSdlResponse(status: Status, contentType: Option[String]): Boolean =
    status.isSuccess && !RemoteTransport.mediaType(contentType).exists(_.startsWith("text/html"))

  // The ETag or uplink id of the last fetched document.
  private final case class Cached(tag: String, document: Document)
}
