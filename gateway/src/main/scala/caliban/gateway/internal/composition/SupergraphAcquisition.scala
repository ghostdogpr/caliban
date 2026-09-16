package caliban.gateway.internal.composition

import caliban.gateway.SupergraphAcquisitionError._
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.composition.ApolloUplinkClient.UplinkResponse
import caliban.gateway.internal.execution.RemoteTransport
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
 * A loader is created once and read on every reload, so a refreshable source must observe changes
 * at the source rather than caching its first result.
 */
object SupergraphAcquisition {
  trait Loader {
    def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document]
  }

  private[gateway] def make(source: Supergraph.Source, http: Option[GatewayHttpClient]): UIO[Loader] =
    source match {
      case Supergraph.Source.Sdl(value)             => ZIO.succeed(constant(parse(value)))
      case Supergraph.Source.Parsed(value)          => ZIO.succeed(constant(Right(value)))
      case Supergraph.Source.File(path)             => ZIO.succeed(file(path))
      case Supergraph.Source.Http(endpoint, config) => httpClientOrDie(http).flatMap(remote(endpoint, config, _))
      case Supergraph.Source.Uplink(config)         => httpClientOrDie(http).flatMap(uplink(config, _))
    }

  // A defect rather than a failure: the caller decides whether the source is remote, so a
  // missing client is a wiring bug that should surface where it is made, not as a load error.
  private def httpClientOrDie(http: Option[GatewayHttpClient]): UIO[GatewayHttpClient] =
    ZIO
      .fromOption(http)
      .orDieWith(_ => new IllegalStateException("A remote supergraph source requires an HTTP client."))

  /** Parsing is deferred to `load` because `make` has no error channel, but it still happens once. */
  private def constant(result: Either[SupergraphAcquisitionError, Document]): Loader =
    new Loader {
      def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] = ZIO.fromEither(result)
    }

  private def parse(value: String): Either[SupergraphAcquisitionError, Document] =
    Parser.parseQuery(value).left.map(InvalidSupergraphSchema(_))

  /** Re-read on every load, so a supergraph rotated on disk is picked up by the next reload. */
  private def file(path: Path): Loader =
    new Loader {
      def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] =
        ZIO
          .attemptBlocking(new String(Files.readAllBytes(path), StandardCharsets.UTF_8))
          .mapError(FileUnreadable(_))
          .flatMap(value => ZIO.fromEither(parse(value)))
    }

  /**
   * Hive's CDN answers `302` to a short-lived presigned storage url and honours `ETag` /
   * `If-None-Match`, so a poll of an unchanged supergraph costs one conditional request rather than a
   * full body and a full parse. It is an optimization only: `ReloadableGatewayInterpreterImpl.cycle`
   * already suppresses the swap by fingerprint, so nothing observable may depend on it firing.
   *
   * Two details the shape of this loop exists for:
   *
   *  - The tag stored is the one the **first** host in the chain returned, and `If-None-Match` goes only
   *    to that host. A presigned object's own tag means nothing to the CDN, and sending it back would
   *    earn a `200` on every future poll, so the optimization would silently never fire.
   *  - The cached document is the last **fetched** one, not the last **activated** one. A supergraph that
   *    fetched but failed to compose keeps its tag, so the next poll answers `304`, re-offers the same
   *    document, and the caller re-attempts the build against the still-older active generation.
   *    Caching the activated document instead would wedge the gateway on the schema it never replaced.
   */
  private def remote(endpoint: URL, config: RemoteGraphQLConfig.Acquisition, http: GatewayHttpClient): UIO[Loader] =
    Ref.make(HttpState(None, None)).map { ref =>
      new Loader {
        def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] = {
          def loop(url: URL, redirects: Int, firstTag: Option[String]): IO[SupergraphAcquisitionError, Document] =
            ref.get.flatMap { state =>
              val first   = redirects == 0
              // A redirect target is treated as a different host: a presigned storage url does not
              // need the CDN token, and forwarding it would hand the token to a third party.
              val headers =
                (if (first) config.headers else Nil) :::
                  Header.Custom("Accept", "application/graphql, text/plain;q=0.9") ::
                  state.etag.filter(_ => first).map(tag => Header.IfNoneMatch.ETags(NonEmptyChunk(tag))).toList

              http
                .get(url, headers, config.maxResponseBytes)
                .mapError[SupergraphAcquisitionError](RequestFailed(_))
                .timeoutFail(TimedOut(config.timeout))(config.timeout)
                .flatMap { reply =>
                  // Whether the first host answered with the body or redirected to it, its tag is
                  // the one that names the artifact.
                  val tag        = if (first) reply.response.rawHeader(Header.ETag) else firstTag
                  val unexpected = UnexpectedResponse(reply.status, reply.contentType)

                  if (reply.body.limitExceeded) ZIO.fail(ResponseTooLarge(config.maxResponseBytes))
                  else if (reply.status == Status.NotModified)
                    // Only the first host was asked a conditional question. A `304` from a redirect
                    // target answers one nobody posed, and honouring it would return a document the
                    // first host has just said to go and fetch again.
                    if (first) ZIO.fromOption(state.last).orElseFail(unexpected)
                    else ZIO.fail(unexpected)
                  else if (reply.status.isRedirection && redirects < config.maxRedirects)
                    ZIO
                      .fromOption(reply.response.rawHeader(Header.Location).flatMap(resolveRedirect(url, _)))
                      .orElseFail(unexpected)
                      .flatMap(location => loop(location, redirects + 1, tag))
                  else if (reply.status.isRedirection || !allowedMediaType(reply.status, reply.contentType))
                    ZIO.fail(unexpected)
                  else {
                    val sdl = new String(reply.body.bytes, StandardCharsets.UTF_8)
                    if (RemoteSchemaAcquisition.withinGraphQLDepth(sdl, config.maxParsingDepth))
                      // Tag and document are stored together, and only once the document parses: a
                      // tag held without the document it names earns a `304` nothing can answer.
                      ZIO.fromEither(parse(sdl)).tap(document => ref.set(HttpState(tag, Some(document))))
                    else ZIO.fail(ParsingDepthExceeded(config.maxParsingDepth))
                  }
                }
            }

          loop(endpoint, 0, None)
        }
      }
    }

  private def resolveRedirect(base: URL, location: String): Option[URL] =
    Try(new URI(location)).toOption.flatMap { reference =>
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

  /** The `ETag` the first host in the chain returned, and the document that fetch produced. */
  private final case class HttpState(etag: Option[String], last: Option[Document])

  /**
   * Apollo publishes several interchangeable uplink endpoints, so a load walks `config.endpoints` in
   * order. Only a transport-level failure moves to the next one: the request never completed
   * ([[SupergraphAcquisitionError.RequestFailed]]), the answer was not a usable uplink response
   * ([[SupergraphAcquisitionError.UnexpectedResponse]]), or the attempt ran out of time
   * ([[SupergraphAcquisitionError.TimedOut]]). Every other failure is the service's authoritative
   * answer and propagates from the endpoint that gave it, so an `AUTHENTICATION_FAILED` is never
   * re-POSTed, api key and all, to a second host.
   *
   * `acquisition.timeout` is a **per-attempt** budget, as it reads for every other source, so one load
   * costs at most `timeout * endpoints.size`. A first endpoint that blackholes therefore still
   * leaves the next one a full budget, which is the outage failover exists for.
   *
   * The cursor is endpoint-independent, as Apollo's own gateway rotates endpoints against a single
   * id, so every attempt within a load sends the same `ifAfterId`. Only a successful fetch advances it.
   */
  private def uplink(config: SupergraphUplinkConfig, http: GatewayHttpClient): UIO[Loader] =
    Ref.make(UplinkState(None, None)).map { state =>
      new Loader {
        def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] =
          state.get.flatMap(attempt(config.endpoints, _))

        private def attempt(endpoints: List[URL], uplinkState: UplinkState)(implicit
          trace: Trace
        ): IO[SupergraphAcquisitionError, Document] =
          endpoints match {
            // The caller describes the endpoints, so an empty list is a wiring bug rather than a load
            // failure; `SupergraphUplinkConfig.diagnostics` rejects it where the gateway is built.
            case Nil                  =>
              ZIO.die(new IllegalStateException("An uplink supergraph source requires at least one endpoint."))
            case endpoint :: Nil      => acquire(endpoint, uplinkState)
            case endpoint :: fallback =>
              acquire(endpoint, uplinkState).catchSome { case _: RequestFailed | _: UnexpectedResponse | _: TimedOut =>
                attempt(fallback, uplinkState)
              }
          }

        private def acquire(endpoint: URL, uplinkState: UplinkState)(implicit
          trace: Trace
        ): IO[SupergraphAcquisitionError, Document] = {
          val acquisition = config.acquisition

          ApolloUplinkClient
            .fetch(endpoint, config, uplinkState.cursor, http)
            .flatMap {
              case UplinkResponse.Success(id, Some(sdl), _) =>
                // The cursor advances on a successful fetch and caches the document that fetch produced.
                // `Unchanged` then re-offers a supergraph that fetched but failed to build, rather than
                // wedging the gateway on the generation it never managed to replace.
                if (RemoteSchemaAcquisition.withinGraphQLDepth(sdl, acquisition.maxParsingDepth))
                  ZIO.fromEither(parse(sdl)).tap(document => state.set(UplinkState(Some(id), Some(document))))
                else ZIO.fail(ParsingDepthExceeded(acquisition.maxParsingDepth))
              case UplinkResponse.Success(_, None, _)       =>
                // `Unchanged` answers a cursor we sent, so an empty cache means the server answered one we
                // never stored. Leave the cursor alone: acknowledging it would have every later poll ask
                // the same unanswerable question.
                ZIO
                  .fromOption(uplinkState.last)
                  .orElseFail(InvalidUplinkResponse(InvalidUplinkResponse.MissingSupergraphSdl))
              case UplinkResponse.Failure(code, _)          =>
                // The code is a fixed enum and safe to render; the message beside it is remote free text.
                ZIO.fail(UplinkFetchFailed(code))
            }
            .timeoutFail(TimedOut(acquisition.timeout))(acquisition.timeout)
        }
      }
    }

  private final case class UplinkState(cursor: Option[String], last: Option[Document])

  /**
   * A supergraph is served as SDL text rather than GraphQL JSON, and static file servers routinely
   * omit the media type or generalise it to `application/octet-stream`, so this is deliberately
   * permissive. `text/html` is the one worth rejecting: an HTML login or error page answering `200`
   * is the realistic failure, and it would otherwise surface as a confusing parse error.
   */
  private def allowedMediaType(status: Status, contentType: Option[String]): Boolean =
    status.isSuccess && !RemoteTransport.mediaType(contentType).exists(_.startsWith("text/html"))
}
