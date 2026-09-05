package caliban.gateway.internal.composition

import caliban.client.CalibanClientError.ServerError
import caliban.gateway.SupergraphAcquisitionError._
import caliban.gateway.internal.composition.ApolloUplinkClient.RouterConfig
import caliban.gateway.internal.execution.RemoteTransport
import caliban.gateway.{ RemoteGraphQLConfig, Supergraph, SupergraphAcquisitionError, SupergraphUplinkConfig }
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import sttp.capabilities.zio.ZioStreams
import sttp.client4._
import sttp.client4.httpclient.zio.SttpClient
import sttp.client4.jsoniter.asJson
import sttp.model.{ HeaderNames, StatusCode, Uri }
import zio.{ Exit, IO, Ref, Trace, UIO, ZIO }

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }

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

  def make(source: Supergraph.Source, backend: Option[SttpClient]): UIO[Loader] =
    source match {
      case Supergraph.Source.Sdl(value)             => ZIO.succeed(constant(parse(value)))
      case Supergraph.Source.Parsed(value)          => ZIO.succeed(constant(Right(value)))
      case Supergraph.Source.File(path)             => ZIO.succeed(file(path))
      case Supergraph.Source.Http(endpoint, config) =>
        // A defect rather than a failure: the caller decides whether the source is remote, so a
        // missing backend is a wiring bug that should surface where it is made, not as a load error.
        ZIO
          .fromOption(backend)
          .orDieWith(_ => new IllegalStateException("A remote supergraph source requires an acquisition backend."))
          .flatMap(http(endpoint, config, _))
      case Supergraph.Source.Uplink(config)         =>
        ZIO
          .fromOption(backend)
          .orDieWith(_ => new IllegalStateException("A remote supergraph source requires an acquisition backend."))
          .flatMap(uplink(config, _))

    }

  /** Parsing is deferred to `load` because `make` has no error channel, but it still happens once. */
  private def constant(result: Either[SupergraphAcquisitionError, Document]): Loader =
    new Loader {
      def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] = ZIO.fromEither(result)
    }

  private def resolveRedirect(base: Uri, location: Uri): Uri =
    if (location.scheme.isEmpty && location.authority.isEmpty && location.pathToString.isEmpty)
      base.copy(
        querySegments = if (location.querySegments.isEmpty) base.querySegments else location.querySegments,
        fragmentSegment = location.fragmentSegment
      )
    else base.resolve(location)

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
   *    earn a `200` on every future poll - the optimization silently never firing.
   *  - The cached document is the last **fetched** one, not the last **activated** one. A supergraph that
   *    fetched but failed to compose keeps its tag, so the next poll answers `304`, re-offers the same
   *    document, and the caller re-attempts the build against the still-older active generation.
   *    Caching the activated document instead would wedge the gateway on the schema it never replaced.
   */
  private def http(endpoint: Uri, config: RemoteGraphQLConfig.Acquisition, backend: SttpClient): UIO[Loader] =
    Ref.make(HttpState(None, None)).map { ref =>
      new Loader {
        def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] = {
          def loop(uri: Uri, redirects: Int, firstTag: Option[String]): IO[SupergraphAcquisitionError, Document] =
            ref.get.flatMap { state =>
              val first = redirects == 0

              RemoteTransport
                // A redirect target is treated as a different host: a presigned storage url does not
                // need the CDN token, and forwarding it would hand the token to a third party.
                .addHeaders(basicRequest.get(uri), if (first) config.headers else Nil)
                .header("Accept", "application/graphql, text/plain;q=0.9")
                .header(HeaderNames.IfNoneMatch, state.etag.filter(_ => first))
                .followRedirects(false)
                .response(
                  asStreamAlways(ZioStreams)(
                    RemoteTransport.readBounded(config.maxResponseBytes)
                  ).mapWithMetadata { (body, meta) =>
                    // Whether the first host answered with the body or redirected to it, its tag is
                    // the one that names the artifact.
                    val tag = if (first) meta.header(HeaderNames.Etag) else firstTag

                    if (body.limitExceeded) Left(ResponseTooLarge(config.maxResponseBytes))
                    else if (meta.code == StatusCode.NotModified)
                      // Only the first host was asked a conditional question. A `304` from a redirect
                      // target answers one nobody posed, and honouring it would return a document the
                      // first host has just said to go and fetch again.
                      if (first)
                        state.last.map(Exit.succeed).toRight(UnexpectedResponse(meta.code, meta.contentType))
                      else Left(UnexpectedResponse(meta.code, meta.contentType))
                    else if (meta.code.isRedirect && redirects < config.maxRedirects)
                      meta
                        .header(HeaderNames.Location)
                        .toRight(UnexpectedResponse(meta.code, meta.contentType))
                        .flatMap(Uri.parse(_).left.map(_ => UnexpectedResponse(meta.code, meta.contentType)))
                        .map(location => loop(resolveRedirect(uri, location), redirects + 1, tag))
                    else if (meta.code.isRedirect || !allowedMediaType(meta.code, meta.contentType))
                      Left(UnexpectedResponse(meta.code, meta.contentType))
                    else {
                      val sdl = new String(body.bytes, StandardCharsets.UTF_8)
                      if (RemoteSchemaAcquisition.withinGraphQLDepth(sdl, config.maxParsingDepth))
                        // Tag and document are stored together, and only once the document parses: a
                        // tag held without the document it names earns a `304` nothing can answer.
                        Right(ZIO.fromEither(parse(sdl)).tap(document => ref.set(HttpState(tag, Some(document)))))
                      else Left(ParsingDepthExceeded(config.maxParsingDepth))
                    }
                  }
                )
                .send(backend)
                .mapError[SupergraphAcquisitionError](RequestFailed(_))
                .map(_.body)
                .absolve
                .timeoutFail(TimedOut(config.timeout))(config.timeout)
                .flatten
            }

          loop(endpoint, 0, None)
        }
      }
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
   * re-POSTed - api key and all - to a second host.
   *
   * `acquisition.timeout` is a **per-attempt** budget, as it reads for every other source, so one load
   * costs at most `timeout * endpoints.size`. A first endpoint that blackholes therefore still
   * leaves the next one a full budget, which is the outage failover exists for.
   *
   * The cursor is endpoint-independent - Apollo's own gateway rotates endpoints against a single id -
   * so every attempt within a load sends the same `ifAfterId`, and only a successful fetch advances it.
   */
  private def uplink(config: SupergraphUplinkConfig, backend: SttpClient): UIO[Loader] =
    Ref.make(UplinkState(None, None)).map { state =>
      new Loader {
        def load(implicit trace: Trace): IO[SupergraphAcquisitionError, Document] =
          state.get.flatMap(attempt(config.endpoints, _))

        private def attempt(endpoints: List[Uri], uplinkState: UplinkState)(implicit
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

        private def acquire(endpoint: Uri, uplinkState: UplinkState)(implicit
          trace: Trace
        ): IO[SupergraphAcquisitionError, Document] = {
          val selection =
            ApolloUplinkClient.supergraphSDL(config.apiKey.stringValue, config.graphRef, uplinkState.cursor)
          val query     = selection.toGraphQL(useVariables = true, queryName = Some("SupergraphSdl"))

          RemoteTransport
            .addHeaders(basicRequest.post(endpoint).body(asJson(query)), config.acquisition.headers)
            .followRedirects(false)
            .response(
              asStreamAlways(ZioStreams)(
                RemoteTransport.readBounded(config.acquisition.maxResponseBytes)
              ).mapWithMetadata { (body, meta) =>
                val decoded: Either[SupergraphAcquisitionError, RouterConfig] =
                  if (body.limitExceeded) Left(ResponseTooLarge(config.acquisition.maxResponseBytes))
                  else if (meta.code.isRedirect || !allowedMediaType(meta.code, meta.contentType))
                    Left(UnexpectedResponse(meta.code, meta.contentType))
                  else if (
                    RemoteTransport
                      .validateJsonStructure(body.bytes, config.acquisition.maxParsingDepth, Int.MaxValue)
                      .isLeft
                  )
                    Left(ParsingDepthExceeded(config.acquisition.maxParsingDepth))
                  else
                    selection
                      .decode(new String(body.bytes, StandardCharsets.UTF_8))
                      .left
                      .map {
                        // A `ServerError` is a top-level `errors` array with no data; anything else the
                        // client raises here is a shape it could not read. Neither one's payload may be
                        // rendered - remote free text never reaches a diagnostic.
                        case _: ServerError => InvalidUplinkResponse(InvalidUplinkResponse.MissingData)
                        case _              => InvalidUplinkResponse(InvalidUplinkResponse.DecodingFailed)
                      }
                      .flatMap { case (routerConfig, errors, _) =>
                        if (errors.isEmpty) Right(routerConfig)
                        else Left(InvalidUplinkResponse(InvalidUplinkResponse.MissingRouterConfig))
                      }
                decoded
              }
            )
            .send(backend)
            .mapError[SupergraphAcquisitionError](RequestFailed(_))
            .map(_.body)
            .absolve
            .flatMap {
              case RouterConfig.Success(id, Some(sdl), _) =>
                // The cursor advances on a successful fetch and caches the document that fetch produced.
                // `Unchanged` then re-offers a supergraph that fetched but failed to build, rather than
                // wedging the gateway on the generation it never managed to replace.
                if (RemoteSchemaAcquisition.withinGraphQLDepth(sdl, config.acquisition.maxParsingDepth))
                  ZIO.fromEither(parse(sdl)).tap(document => state.set(UplinkState(Some(id), Some(document))))
                else ZIO.fail(ParsingDepthExceeded(config.acquisition.maxParsingDepth))
              case RouterConfig.Success(_, None, _)       =>
                // `Unchanged` answers a cursor we sent, so an empty cache means the server answered one we
                // never stored. Leave the cursor alone: acknowledging it would have every later poll ask
                // the same unanswerable question.
                ZIO
                  .fromOption(uplinkState.last)
                  .orElseFail(InvalidUplinkResponse(InvalidUplinkResponse.MissingSupergraphSdl))
              case RouterConfig.Failed(code, _)           =>
                // The code is a fixed enum and safe to render; the message beside it is remote free text.
                ZIO.fail(UplinkFetchFailed(code))
            }
            .timeoutFail(TimedOut(config.acquisition.timeout))(config.acquisition.timeout)
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
  private def allowedMediaType(code: StatusCode, contentType: Option[String]): Boolean =
    code.isSuccess && !RemoteTransport.mediaType(contentType).exists(_.startsWith("text/html"))
}
