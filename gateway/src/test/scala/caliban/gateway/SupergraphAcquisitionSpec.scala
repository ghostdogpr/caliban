package caliban.gateway

import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.composition.SupergraphAcquisition
import caliban.parsing.Parser
import caliban.parsing.adt.Document
import sttp.client4.httpclient.zio.{ HttpClientZioBackend, SttpClient }
import sttp.model.Uri
import zio.Config.Secret
import zio._
import zio.http.{ trailing, Body, Handler, Header, Headers, Method, Request, Response, Routes, Server, Status }
import zio.test._

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }

object SupergraphAcquisitionSpec extends ZIOSpecDefault {

  private val supergraphSdl =
    """schema
      |  @link(url: "https://specs.apollo.dev/link/v1.0")
      |  @link(url: "https://specs.apollo.dev/join/v0.5", for: EXECUTION)
      |{
      |  query: Query
      |}
      |
      |enum join__Graph {
      |  A @join__graph(name: "a", url: "http://a/graphql")
      |}
      |
      |type Query @join__type(graph: A) { hello: String }
      |""".stripMargin

  private def load(
    source: Supergraph.Source,
    backend: Option[SttpClient] = None
  ): URIO[Any, Exit[SupergraphAcquisitionError, Document]] =
    SupergraphAcquisition.make(source, backend).flatMap[Any, SupergraphAcquisitionError, Document](_.load).exit

  private def queryFields(document: Document): List[String] =
    document.objectTypeDefinitions.filter(_.name == "Query").flatMap(_.fields.map(_.name))

  /** Fails with the acquisition error, or dies describing what happened instead. */
  private def failure[A](exit: Exit[SupergraphAcquisitionError, A]): UIO[SupergraphAcquisitionError] =
    exit match {
      case Exit.Failure(cause) =>
        ZIO
          .fromOption(cause.failureOption)
          .orDieWith(_ => new AssertionError(s"expected a typed failure, got: ${cause.prettyPrint}"))
      case Exit.Success(value) => ZIO.die(new AssertionError(s"expected a failure, got: $value"))
    }

  /** A GET endpoint; `GatewayTestSupport` only exposes POST, which subgraph acquisition uses. */
  private def getEndpoint(
    body: String,
    status: Status = Status.Ok,
    mediaType: Option[String] = Some("application/graphql")
  ): ZIO[Server with Ref[Int], Nothing, Uri] =
    for {
      id     <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
      path    = s"supergraph-$id"
      server <- ZIO.service[Server]
      headers = mediaType.fold(Headers.empty)(value => Headers(Header.Custom("Content-Type", value)))
      _      <- server.install(
                  Routes(
                    Method.GET / path -> Handler.fromFunctionZIO[Request](_ =>
                      ZIO.succeed(Response(status, headers, Body.fromString(body)))
                    )
                  )
                )
      port   <- server.port
    } yield Uri.unsafeParse(s"http://127.0.0.1:$port/$path")

  private def temporaryFile(contents: String): ZIO[Scope, Nothing, Path] =
    ZIO
      .acquireRelease(ZIO.attempt {
        val path = Files.createTempFile("supergraph", ".graphql")
        Files.write(path, contents.getBytes(StandardCharsets.UTF_8))
        path
      })(path => ZIO.attempt(Files.deleteIfExists(path)).ignore)
      .orDie

  private def httpSource(endpoint: Uri, configure: RemoteGraphQLConfig.Acquisition => RemoteGraphQLConfig.Acquisition) =
    Supergraph.Source.Http(endpoint, configure(RemoteGraphQLConfig.Acquisition.default))

  // -----------------------------------------------------------------------------------------------
  // A route that records what it was asked and answers a script, for conditional and redirect tests
  // -----------------------------------------------------------------------------------------------

  private final case class Answer(
    status: Status = Status.Ok,
    body: String = "",
    etag: Option[String] = None,
    location: Option[Uri] = None,
    mediaType: Option[String] = Some("application/graphql")
  )

  private object Answer {
    def sdl(etag: Option[String] = None): Answer        = Answer(body = supergraphSdl, etag = etag)
    def redirect(to: Uri, etag: Option[String]): Answer =
      Answer(status = Status.Found, etag = etag, location = Some(to), mediaType = None)
    val notModified: Answer                             = Answer(status = Status.NotModified, mediaType = None)
  }

  private final case class Route(endpoint: Uri, requests: Ref[Vector[Headers]]) {
    def calls: UIO[Int] = requests.get.map(_.size)

    /** The `If-None-Match` of the nth request, or `None` when that request carried none. */
    def conditionOf(index: Int): UIO[Option[String]] =
      requests.get.map(_.lift(index).flatMap(_.rawHeader("If-None-Match")))
  }

  private final case class CdnRoute(base: Uri, requests: Ref[Vector[(String, Headers)]]) {
    def calls: UIO[Int] = requests.get.map(_.size)

    def pathOf(index: Int): UIO[Option[String]] = requests.get.map(_.lift(index).map(_._1))

    def keyOf(index: Int): UIO[Option[String]] =
      requests.get.map(_.lift(index).flatMap(_._2.rawHeader("X-Hive-CDN-Key")))
  }

  /**
   * Answers any path below its base and records the one it was asked for, so the url the source
   * builds is the assertion rather than the fixture. A route mounted at the expected path would
   * report a wrong url as a 404 and say nothing about what was actually requested.
   */
  private def cdnEndpoint(answers: Answer*): ZIO[Server with Ref[Int], Nothing, CdnRoute] =
    for {
      recorded  <- Ref.make(Vector.empty[(String, Headers)])
      remaining <- Ref.make(answers.toList)
      id        <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
      prefix     = s"hive-$id"
      server    <- ZIO.service[Server]
      _         <- server.install(
                     Routes(
                       Method.GET / prefix / trailing -> Handler.fromFunctionZIO[Request] { request =>
                         for {
                           _      <- recorded.update(_ :+ (request.path.toString -> request.headers))
                           answer <- remaining.modify {
                                       case only :: Nil  => only     -> (only :: Nil)
                                       case head :: rest => head     -> rest
                                       case Nil          => Answer() -> Nil
                                     }
                         } yield Response(
                           answer.status,
                           Headers(
                             answer.mediaType.map(Header.Custom("Content-Type", _)).toList :::
                               answer.location.map(value => Header.Custom("Location", value.toString)).toList
                           ),
                           Body.fromString(answer.body)
                         )
                       }
                     )
                   )
      port      <- server.port
    } yield CdnRoute(Uri.unsafeParse(s"http://127.0.0.1:$port/$prefix"), recorded)

  /** Answers each request with the head of `answers`, keeping the last one once the list runs out. */
  private def recordingEndpoint(answers: Answer*): ZIO[Server with Ref[Int], Nothing, Route] =
    for {
      recorded  <- Ref.make(Vector.empty[Headers])
      remaining <- Ref.make(answers.toList)
      id        <- ZIO.serviceWithZIO[Ref[Int]](_.updateAndGet(_ + 1))
      path       = s"cdn-$id"
      server    <- ZIO.service[Server]
      _         <- server.install(
                     Routes(
                       Method.GET / path -> Handler.fromFunctionZIO[Request] { request =>
                         for {
                           _      <- recorded.update(_ :+ request.headers)
                           answer <- remaining.modify {
                                       case only :: Nil  => only     -> (only :: Nil)
                                       case head :: rest => head     -> rest
                                       case Nil          => Answer() -> Nil
                                     }
                         } yield Response(
                           answer.status,
                           Headers(
                             answer.mediaType.map(Header.Custom("Content-Type", _)).toList :::
                               answer.etag.map(Header.Custom("ETag", _)).toList :::
                               answer.location.map(value => Header.Custom("Location", value.toString)).toList
                           ),
                           Body.fromString(answer.body)
                         )
                       }
                     )
                   )
      port      <- server.port
    } yield Route(Uri.unsafeParse(s"http://127.0.0.1:$port/$path"), recorded)

  private val backend: ZLayer[Any, Throwable, SttpClient] = ZLayer.scoped(HttpClientZioBackend.scoped())

  def spec = suite("SupergraphAcquisitionSpec")(
    suite("local sources")(
      test("parses sdl") {
        load(Supergraph.Source.Sdl(supergraphSdl)).flatMap(exit =>
          ZIO.fromEither(exit.toEither).orDie.map(document => assertTrue(queryFields(document) == List("hello")))
        )
      },
      test("returns a parsed document unchanged") {
        for {
          document <- ZIO.fromEither(Parser.parseQuery(supergraphSdl)).orDie
          exit     <- load(Supergraph.Source.Parsed(document))
        } yield assertTrue(exit == Exit.succeed(document))
      },
      test("reports unparseable sdl rather than throwing") {
        for {
          exit  <- load(Supergraph.Source.Sdl("type Query {"))
          error <- failure(exit)
        } yield assertTrue(error.isInstanceOf[SupergraphAcquisitionError.InvalidSupergraphSchema])
      }
    ),
    suite("file source")(
      test("re-reads the file on every load, so a rotated supergraph is observed") {
        // The whole point of a file source: the loader is built once and read on every reload.
        ZIO.scoped {
          for {
            path   <- temporaryFile(supergraphSdl)
            loader <- SupergraphAcquisition.make(Supergraph.Source.File(path), None)
            first  <- loader.load
            _      <- ZIO
                        .attempt(
                          Files.write(
                            path,
                            supergraphSdl
                              .replace("hello: String", "hello: String goodbye: String")
                              .getBytes(
                                StandardCharsets.UTF_8
                              )
                          )
                        )
                        .orDie
            second <- loader.load
          } yield assertTrue(
            queryFields(first) == List("hello"),
            queryFields(second) == List("hello", "goodbye")
          )
        }
      },
      test("reports an unreadable file distinctly from unparseable contents") {
        // Two different failures that would otherwise both surface as "could not load".
        ZIO.scoped {
          for {
            missing     <- load(Supergraph.Source.File(Path.of("/nonexistent/supergraph.graphql")))
            unreadable  <- failure(missing)
            path        <- temporaryFile("type Query {")
            malformed   <- load(Supergraph.Source.File(path))
            unparseable <- failure(malformed)
          } yield assertTrue(
            unreadable.isInstanceOf[SupergraphAcquisitionError.FileUnreadable],
            unparseable.isInstanceOf[SupergraphAcquisitionError.InvalidSupergraphSchema]
          )
        }
      }
    ),
    suite("http source")(
      test("fetches and parses a served supergraph") {
        for {
          endpoint <- getEndpoint(supergraphSdl)
          client   <- ZIO.service[SttpClient]
          exit     <- load(httpSource(endpoint, identity), Some(client))
          document <- ZIO.fromEither(exit.toEither).orDie
        } yield assertTrue(queryFields(document) == List("hello"))
      },
      test("accepts a response with no media type at all") {
        // Static file servers routinely omit it; refusing would make the common case unusable.
        for {
          endpoint <- getEndpoint(supergraphSdl, mediaType = None)
          client   <- ZIO.service[SttpClient]
          exit     <- load(httpSource(endpoint, identity), Some(client))
        } yield assertTrue(exit.isSuccess)
      },
      test("rejects an html page answering 200") {
        // A login or error page is the realistic failure, and it must not surface as a parse error.
        for {
          endpoint <- getEndpoint("<html><body>Sign in</body></html>", mediaType = Some("text/html; charset=utf-8"))
          client   <- ZIO.service[SttpClient]
          exit     <- load(httpSource(endpoint, identity), Some(client))
          error    <- failure(exit)
        } yield assertTrue(error.isInstanceOf[SupergraphAcquisitionError.UnexpectedResponse])
      },
      test("rejects a non-success status") {
        for {
          endpoint <- getEndpoint("nope", status = Status.InternalServerError)
          client   <- ZIO.service[SttpClient]
          exit     <- load(httpSource(endpoint, identity), Some(client))
          error    <- failure(exit)
        } yield assertTrue(
          error match {
            case SupergraphAcquisitionError.UnexpectedResponse(status, _) => status.code == 500
            case _                                                        => false
          }
        )
      },
      test("stops reading once the response exceeds the byte limit") {
        for {
          endpoint <- getEndpoint(supergraphSdl)
          client   <- ZIO.service[SttpClient]
          exit     <- load(httpSource(endpoint, _.withMaxResponseBytes(16)), Some(client))
          error    <- failure(exit)
        } yield assertTrue(error == SupergraphAcquisitionError.ResponseTooLarge(16))
      },
      test("rejects a supergraph nested past the parsing depth") {
        for {
          endpoint <- getEndpoint(supergraphSdl)
          client   <- ZIO.service[SttpClient]
          exit     <- load(httpSource(endpoint, _.withMaxParsingDepth(1)), Some(client))
          error    <- failure(exit)
        } yield assertTrue(error == SupergraphAcquisitionError.ParsingDepthExceeded(1))
      },
      test("reports unparseable served sdl") {
        for {
          endpoint <- getEndpoint("type Query {")
          client   <- ZIO.service[SttpClient]
          exit     <- load(httpSource(endpoint, identity), Some(client))
          error    <- failure(exit)
        } yield assertTrue(error.isInstanceOf[SupergraphAcquisitionError.InvalidSupergraphSchema])
      },
      test("fails rather than following a redirect") {
        for {
          endpoint <- getEndpoint("", status = Status.Found)
          client   <- ZIO.service[SttpClient]
          exit     <- load(httpSource(endpoint, identity), Some(client))
          error    <- failure(exit)
        } yield assertTrue(error.isInstanceOf[SupergraphAcquisitionError.UnexpectedResponse])
      },
      test("fails when the endpoint is unreachable") {
        for {
          client <- ZIO.service[SttpClient]
          exit   <- load(httpSource(unreachableEndpoint, identity), Some(client))
          error  <- failure(exit)
        } yield assertTrue(error.isInstanceOf[SupergraphAcquisitionError.RequestFailed])
      },
      test("never leaks the served payload into diagnostics") {
        // The same guarantee `lastReloadFailure` documents: categories and codes, never content.
        val secret = "SUPER_SECRET_TOKEN"
        for {
          html    <- getEndpoint(s"<html>$secret</html>", mediaType = Some("text/html"))
          sdl     <- getEndpoint(s"type Query { $secret ")
          client  <- ZIO.service[SttpClient]
          first   <- load(httpSource(html, identity), Some(client)).flatMap(failure(_))
          second  <- load(httpSource(sdl, identity), Some(client)).flatMap(failure(_))
          reported = (first.diagnostics ::: second.diagnostics).mkString("\n")
        } yield assertTrue(!reported.contains(secret))
      }
    ),

    // ---------------------------------------------------------------------------------------
    // Task 8 — conditional requests
    //
    // Hive's CDN honours `ETag` / `If-None-Match` and answers `304` when the supergraph has not
    // changed, which is the common case on every poll. This is an optimization, not a correctness
    // feature: fingerprint dedup in `ReloadableGatewayInterpreterImpl.cycle` already suppresses the
    // swap. The risk being gated is therefore a silently-never-firing optimization, not a wrong
    // answer — which is exactly what a green suite hides if it only ever exercises `200`.
    // ---------------------------------------------------------------------------------------
    suite("conditional requests")(
      test("a first load is unconditional, and stores the tag the response carried") {
        for {
          // Both answers are `200`, so this isolates storing and re-sending the tag from whether a
          // `304` is handled — that has its own tests below, and a shared fixture would report one
          // bug twice.
          cdn    <- recordingEndpoint(Answer.sdl(etag = Some("\"v1\"")))
          client <- ZIO.service[SttpClient]
          loader <- SupergraphAcquisition.make(httpSource(cdn.endpoint, identity), Some(client))
          first  <- loader.load
          _      <- loader.load
          before <- cdn.conditionOf(0)
          after  <- cdn.conditionOf(1)
        } yield assertTrue(
          queryFields(first) == List("hello"),
          // Nothing is cached yet, so asking "has it changed since?" would be meaningless.
          before.isEmpty,
          after.contains("\"v1\"")
        )
      },
      test("a 304 returns the document last fetched, without re-parsing anything") {
        for {
          cdn    <- recordingEndpoint(Answer.sdl(etag = Some("\"v1\"")), Answer.notModified)
          client <- ZIO.service[SttpClient]
          loader <- SupergraphAcquisition.make(httpSource(cdn.endpoint, identity), Some(client))
          first  <- loader.load
          second <- loader.load
          calls  <- cdn.calls
        } yield assertTrue(first == second, calls == 2)
      },
      test("a 304 with nothing cached is a protocol violation, not an empty success") {
        // A server answering `304` to an unconditional request. Succeeding with no document would
        // hand the caller a supergraph it never received.
        for {
          cdn    <- recordingEndpoint(Answer.notModified)
          client <- ZIO.service[SttpClient]
          exit   <- load(httpSource(cdn.endpoint, identity), Some(client))
          error  <- failure(exit)
        } yield assertTrue(error.isInstanceOf[SupergraphAcquisitionError.UnexpectedResponse])
      },
      test("a 200 carrying no ETag leaves the next request unconditional") {
        // The stored tag has to be cleared, not kept: re-sending a tag the origin no longer knows
        // about invites a `304` for a document that has in fact changed.
        for {
          cdn    <- recordingEndpoint(Answer.sdl(etag = Some("\"v1\"")), Answer.sdl(etag = None), Answer.sdl())
          client <- ZIO.service[SttpClient]
          loader <- SupergraphAcquisition.make(httpSource(cdn.endpoint, identity), Some(client))
          _      <- loader.load
          _      <- loader.load
          _      <- loader.load
          second <- cdn.conditionOf(1)
          third  <- cdn.conditionOf(2)
        } yield assertTrue(second.contains("\"v1\""), third.isEmpty)
      },
      test("resolves relative redirect locations against each request URI") {
        for {
          cdn      <- cdnEndpoint(
                        Answer.redirect(Uri.unsafeParse("nested/next"), None),
                        Answer.redirect(Uri.unsafeParse("../supergraph.graphql"), None),
                        Answer.sdl()
                      )
          backend  <- ZIO.service[SttpClient]
          result   <- load(httpSource(cdn.base.addPath("start"), _.withMaxRedirects(2)), Some(backend))
          requests <- cdn.requests.get
        } yield assertTrue(
          result.isSuccess,
          requests.map(_._1) == Vector("start", "nested/next", "supergraph.graphql").map(path =>
            s"${java.net.URI.create(cdn.base.toString).getPath}/$path"
          )
        )
      },
      test("query-only redirects retain the resource path and replace its query") {
        for {
          requests <- Ref.make(Vector.empty[String])
          endpoint <- GatewayTestSupport.getEndpoint("query-redirect") { request =>
                        requests.update(_ :+ request.url.toString) *> ZIO.succeed(
                          if (request.url.toString.endsWith("?version=2"))
                            Response(
                              Status.Ok,
                              Headers(Header.Custom("Content-Type", "application/graphql")),
                              Body.fromString(supergraphSdl)
                            )
                          else Response(Status.Found, Headers(Header.Custom("Location", "?version=2")), Body.empty)
                        )
                      }
          backend  <- ZIO.service[SttpClient]
          result   <- load(httpSource(endpoint.addParam("version", "1"), _.withMaxRedirects(1)), Some(backend))
          sent     <- requests.get
        } yield assertTrue(
          result.isSuccess,
          sent == Vector(s"${endpoint.pathToString}?version=1", s"${endpoint.pathToString}?version=2")
        )
      },
      test("the tag stored from a redirecting chain is the first host's, not the storage host's") {
        // Hive answers `302` to a 60-second presigned storage url. The tag that identifies the
        // artifact is the CDN's; the storage object's own tag is meaningless to the CDN, and sending
        // it back guarantees a `200` on every future poll — the optimization silently never fires.
        for {
          storage <- recordingEndpoint(Answer.sdl(etag = Some("\"storage-object\"")))
          cdn     <- recordingEndpoint(Answer.redirect(storage.endpoint, etag = Some("\"cdn-v1\"")))
          client  <- ZIO.service[SttpClient]
          loader  <- SupergraphAcquisition.make(httpSource(cdn.endpoint, _.withMaxRedirects(2)), Some(client))
          _       <- loader.load
          _       <- loader.load
          first   <- cdn.conditionOf(0)
          second  <- cdn.conditionOf(1)
          onward  <- storage.conditionOf(0)
        } yield assertTrue(
          first.isEmpty,
          second.contains("\"cdn-v1\""),
          // The conditional header goes to the first host only; the storage url is presigned and
          // knows nothing about it.
          onward.isEmpty
        )
      },
      test("a 304 from a redirect target is refused rather than answered from the cache") {
        // Only the first host is asked a conditional question, so this `304` answers one nobody
        // posed — and the CDN has just said the artifact moved. Returning the cached document would
        // pin the gateway to a supergraph the CDN is actively redirecting away from.
        for {
          storage <- recordingEndpoint(Answer.sdl(etag = Some("\"storage-object\"")), Answer.notModified)
          cdn     <- recordingEndpoint(Answer.redirect(storage.endpoint, etag = Some("\"cdn-v1\"")))
          client  <- ZIO.service[SttpClient]
          loader  <- SupergraphAcquisition.make(httpSource(cdn.endpoint, _.withMaxRedirects(2)), Some(client))
          first   <- loader.load.exit
          second  <- loader.load.exit
          error   <- failure(second)
        } yield assertTrue(first.isSuccess, error.isInstanceOf[SupergraphAcquisitionError.UnexpectedResponse])
      },
      test("a 304 through a redirecting chain never contacts the storage host") {
        // The assertion the whole suite exists for. A direct-route-only gate is passed by an
        // implementation that follows the redirect first and only then discovers the `304`, which
        // costs exactly the round trip the conditional request was added to avoid.
        for {
          storage      <- recordingEndpoint(Answer.sdl(etag = Some("\"storage-object\"")))
          cdn          <- recordingEndpoint(
                            Answer.redirect(storage.endpoint, etag = Some("\"cdn-v1\"")),
                            Answer.notModified
                          )
          client       <- ZIO.service[SttpClient]
          loader       <- SupergraphAcquisition.make(httpSource(cdn.endpoint, _.withMaxRedirects(2)), Some(client))
          first        <- loader.load
          second       <- loader.load
          cdnCalls     <- cdn.calls
          storageCalls <- storage.calls
        } yield assertTrue(
          first == second,
          cdnCalls == 2,
          storageCalls == 1
        )
      }
    ),

    // ---------------------------------------------------------------------------------------
    // Task 10 — Supergraph.hive
    //
    // A named constructor over `Source.Http`, so the only thing it can get wrong is the shape of
    // the request: the artifact path, the header the CDN authenticates with, and a redirect bound
    // large enough to reach the storage url the CDN answers with. All three are documented values
    // that no other test would notice changing.
    // ---------------------------------------------------------------------------------------
    suite("hive")(
      test("describes the documented CDN artifact url, with the key and a redirect bound") {
        // Pins the published defaults without a network call, including the host a caller who
        // passes no `cdn` reaches.
        Supergraph.hive("target-1", Secret("cdn-key")).source match {
          case Supergraph.Source.Http(endpoint, config) =>
            assertTrue(
              endpoint.toString == "https://cdn.graphql-hive.com/artifacts/v1/target-1/supergraph",
              config.headers.map(header => header.name -> header.value) == List("X-Hive-CDN-Key" -> "cdn-key"),
              // Hive answers a 302 to presigned storage, so a bound of zero would never reach the artifact.
              config.maxRedirects >= 1
            )
          case other                                    => assertTrue(false, other.toString.isEmpty)
        }
      },
      test("is refreshable, so a hive supergraph can drive Gateway.reloadable") {
        assertTrue(Supergraph.hive("target-1", Secret("cdn-key")).source.refreshable)
      },
      test("requests the artifact path and authenticates with the CDN key") {
        for {
          cdn      <- cdnEndpoint(Answer.sdl())
          client   <- ZIO.service[SttpClient]
          loader   <-
            SupergraphAcquisition.make(Supergraph.hive("target-1", Secret("cdn-key"), cdn.base).source, Some(client))
          document <- loader.load
          path     <- cdn.pathOf(0)
          key      <- cdn.keyOf(0)
        } yield assertTrue(
          queryFields(document) == List("hello"),
          path.exists(_.endsWith("/artifacts/v1/target-1/supergraph")),
          key.contains("cdn-key")
        )
      },
      test("follows the CDN's 302 to storage, and does not forward the key to it") {
        // The realistic Hive response. The storage url is presigned and does not need the key;
        // sending it there would hand a live CDN token to a third-party host.
        for {
          storage  <- recordingEndpoint(Answer.sdl())
          cdn      <- cdnEndpoint(Answer.redirect(storage.endpoint, etag = None))
          client   <- ZIO.service[SttpClient]
          loader   <-
            SupergraphAcquisition.make(Supergraph.hive("target-1", Secret("cdn-key"), cdn.base).source, Some(client))
          document <- loader.load
          key      <- cdn.keyOf(0)
          onward   <- storage.requests.get.map(_.headOption.flatMap(_.rawHeader("X-Hive-CDN-Key")))
          calls    <- cdn.calls.zip(storage.calls)
        } yield assertTrue(
          queryFields(document) == List("hello"),
          key.contains("cdn-key"),
          onward.isEmpty,
          calls == ((1, 1))
        )
      }
    )
  ).provide(testServer, stubIds, backend)
}
