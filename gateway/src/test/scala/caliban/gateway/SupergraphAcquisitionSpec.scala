package caliban.gateway

import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.GatewayHttpClient
import caliban.parsing.adt.Document
import zio.Config.Secret
import zio._
import zio.http._
import zio.test._

import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path }

object SupergraphAcquisitionSpec extends ZIOSpecDefault {

  private def load(source: Supergraph.Source): URIO[GatewayHttpClient, Exit[SupergraphAcquisitionError, Document]] =
    acquisitionLoader(source).flatten.exit

  private def staticEndpoint(
    body: String,
    status: Status = Status.Ok,
    mediaType: Option[String] = Some("application/graphql")
  ): ZIO[Server with Ref[Int], Nothing, URL] = {
    val headers = mediaType.fold(Headers.empty)(value => Headers(Header.Custom("Content-Type", value)))
    getEndpoint("supergraph")(_ => ZIO.succeed(Response(status, headers, Body.fromString(body))))
  }

  private def httpSource(endpoint: URL, configure: RemoteGraphQLConfig.Acquisition => RemoteGraphQLConfig.Acquisition) =
    Supergraph.Source.Http(endpoint, configure(RemoteGraphQLConfig.Acquisition.default))

  // -----------------------------------------------------------------------------------------------
  // A route that records what it was asked and answers a script, for conditional and redirect tests
  // -----------------------------------------------------------------------------------------------

  private final case class Answer(
    status: Status = Status.Ok,
    body: String = "",
    etag: Option[String] = None,
    location: Option[String] = None,
    mediaType: Option[String] = Some("application/graphql")
  ) {
    def response: Response =
      Response(
        status,
        Headers(
          mediaType.map(Header.Custom("Content-Type", _)).toList :::
            etag.map(Header.Custom("ETag", _)).toList :::
            location.map(value => Header.Custom("Location", value)).toList
        ),
        Body.fromString(body)
      )
  }

  private object Answer {
    def sdl(etag: Option[String] = None): Answer           = Answer(body = minimalSupergraphSdl, etag = etag)
    def redirect(to: String, etag: Option[String]): Answer =
      Answer(status = Status.Found, etag = etag, location = Some(to), mediaType = None)
    val notModified: Answer                                = Answer(status = Status.NotModified, mediaType = None)
  }

  private final case class Route(endpoint: URL, requests: Ref[Vector[(String, Headers)]]) {
    def calls: UIO[Int] = requests.get.map(_.size)

    def pathOf(index: Int): UIO[Option[String]] = requests.get.map(_.lift(index).map(_._1))

    /** The `If-None-Match` of the nth request, or `None` when that request carried none. */
    def conditionOf(index: Int): UIO[Option[String]] =
      requests.get.map(_.lift(index).flatMap(_._2.rawHeader("If-None-Match")))

    def keyOf(index: Int): UIO[Option[String]] =
      requests.get.map(_.lift(index).flatMap(_._2.rawHeader("X-Hive-CDN-Key")))
  }

  /**
   * Answers each request with the head of `answers`, keeping the last one once the list runs out.
   *
   * Answers any path below its base and records the one it was asked for, so the url the source
   * builds is the assertion rather than the fixture. A route mounted at the expected path would
   * report a wrong url as a 404 and say nothing about what was actually requested.
   */
  private def recordingEndpoint(answers: Answer*): ZIO[Server with Ref[Int], Nothing, Route] =
    for {
      recorded  <- Ref.make(Vector.empty[(String, Headers)])
      remaining <- Ref.make(answers.toList)
      endpoint  <- routesEndpoint("cdn") { path =>
                     Routes(
                       Method.GET / path / trailing -> Handler.fromFunctionZIO[Request] { request =>
                         recorded.update(_ :+ (request.path.toString -> request.headers)) *>
                           nextAnswer(remaining, Answer()).map(_.response)
                       }
                     )
                   }
    } yield Route(endpoint, recorded)

  def spec = suite("SupergraphAcquisitionSpec")(
    suite("local sources")(
      test("parses sdl") {
        for {
          exit     <- load(Supergraph.Source.Sdl(minimalSupergraphSdl))
          document <- ZIO.fromEither(exit.toEither).orDie
        } yield assertTrue(queryFields(document) == List("hello"))
      },
      test("returns a parsed document unchanged") {
        for {
          document <- parseSdl(minimalSupergraphSdl)
          exit     <- load(Supergraph.Source.Parsed(document))
        } yield assertTrue(exit == Exit.succeed(document))
      },
      test("reports unparseable sdl rather than throwing") {
        for {
          exit  <- load(Supergraph.Source.Sdl("type Query {"))
          error <- acquisitionFailure(exit)
        } yield assertTrue(error.isInstanceOf[SchemaAcquisitionError.SchemaParsingFailed])
      }
    ),
    suite("file source")(
      test("re-reads the file on every load, so a rotated supergraph is observed") {
        // The whole point of a file source: the loader is built once and read on every reload.
        ZIO.scoped {
          for {
            path   <- temporaryFile(minimalSupergraphSdl)
            loader <- acquisitionLoader(Supergraph.Source.File(path))
            first  <- loader
            _      <- ZIO.attempt(Files.write(path, changedSupergraphSdl.getBytes(StandardCharsets.UTF_8))).orDie
            second <- loader
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
            unreadable  <- acquisitionFailure(missing)
            path        <- temporaryFile("type Query {")
            malformed   <- load(Supergraph.Source.File(path))
            unparseable <- acquisitionFailure(malformed)
          } yield assertTrue(
            unreadable.isInstanceOf[SupergraphAcquisitionError.FileReadFailed],
            unparseable.isInstanceOf[SchemaAcquisitionError.SchemaParsingFailed]
          )
        }
      }
    ),
    suite("http source")(
      test("fetches and parses a served supergraph") {
        for {
          endpoint <- staticEndpoint(minimalSupergraphSdl)
          exit     <- load(httpSource(endpoint, identity))
          document <- ZIO.fromEither(exit.toEither).orDie
        } yield assertTrue(queryFields(document) == List("hello"))
      },
      test("accepts a response with no media type at all") {
        // Static file servers routinely omit it; refusing would make the common case unusable.
        for {
          endpoint <- staticEndpoint(minimalSupergraphSdl, mediaType = None)
          exit     <- load(httpSource(endpoint, identity))
        } yield assertTrue(exit.isSuccess)
      },
      test("rejects a non-success status") {
        for {
          endpoint <- staticEndpoint("nope", status = Status.InternalServerError)
          exit     <- load(httpSource(endpoint, identity))
          error    <- acquisitionFailure(exit)
        } yield assertTrue(
          error match {
            case SchemaAcquisitionError.UnexpectedResponse(status, _) => status.code == 500
            case _                                                    => false
          }
        )
      },
      test("stops reading once the response exceeds the byte limit") {
        for {
          endpoint <- staticEndpoint(minimalSupergraphSdl)
          exit     <- load(httpSource(endpoint, _.withMaxResponseBytes(16)))
          error    <- acquisitionFailure(exit)
        } yield assertTrue(error == SchemaAcquisitionError.ResponseTooLarge(16))
      },
      test("rejects a supergraph nested past the parsing depth") {
        for {
          endpoint <- staticEndpoint(minimalSupergraphSdl)
          exit     <- load(httpSource(endpoint, _.withMaxParsingDepth(1)))
          error    <- acquisitionFailure(exit)
        } yield assertTrue(error == SchemaAcquisitionError.ParsingDepthExceeded(1))
      },
      test("fails rather than following a redirect") {
        for {
          endpoint <- staticEndpoint("", status = Status.Found)
          exit     <- load(httpSource(endpoint, identity))
          error    <- acquisitionFailure(exit)
        } yield assertTrue(error.isInstanceOf[SchemaAcquisitionError.UnexpectedResponse])
      },
      test("resolves relative redirect locations against each request URI") {
        for {
          cdn      <- recordingEndpoint(
                        Answer.redirect("nested/next", None),
                        Answer.redirect("../supergraph.graphql", None),
                        Answer.sdl()
                      )
          result   <- load(httpSource(cdn.endpoint.addPath("start"), _.withMaxRedirects(2)))
          requests <- cdn.requests.get
        } yield assertTrue(
          result.isSuccess,
          requests.map(_._1) == Vector("start", "nested/next", "supergraph.graphql").map(path =>
            s"${java.net.URI.create(cdn.endpoint.toString).getPath}/$path"
          )
        )
      },
      test("query-only redirects retain the resource path and replace its query") {
        for {
          requests <- Ref.make(Vector.empty[String])
          endpoint <- getEndpoint("query-redirect") { request =>
                        requests.update(_ :+ request.url.toString) *> ZIO.succeed(
                          if (request.url.toString.endsWith("?version=2"))
                            Response(
                              Status.Ok,
                              Headers(Header.Custom("Content-Type", "application/graphql")),
                              Body.fromString(minimalSupergraphSdl)
                            )
                          else Response(Status.Found, Headers(Header.Custom("Location", "?version=2")), Body.empty)
                        )
                      }
          result   <- load(httpSource(endpoint.addQueryParam("version", "1"), _.withMaxRedirects(1)))
          sent     <- requests.get
        } yield assertTrue(
          result.isSuccess,
          sent == Vector(s"${endpoint.path.encode}?version=1", s"${endpoint.path.encode}?version=2")
        )
      },
      test("fails when the endpoint is unreachable") {
        for {
          exit  <- load(httpSource(unreachableEndpoint, identity))
          error <- acquisitionFailure(exit)
        } yield assertTrue(error.isInstanceOf[SchemaAcquisitionError.RequestFailed])
      },
      test("never leaks the served payload into diagnostics") {
        // The same guarantee `lastReloadFailure` documents: categories and codes, never content.
        val secret = "SUPER_SECRET_TOKEN"
        for {
          html    <- staticEndpoint(s"<html>$secret</html>", mediaType = Some("text/html"))
          sdl     <- staticEndpoint(s"type Query { $secret ")
          first   <- load(httpSource(html, identity)).flatMap(acquisitionFailure(_))
          second  <- load(httpSource(sdl, identity)).flatMap(acquisitionFailure(_))
          reported = (first.diagnostics ::: second.diagnostics).mkString("\n")
        } yield assertTrue(
          // A login or error page is the realistic failure, and it must not surface as a parse error.
          first.isInstanceOf[SchemaAcquisitionError.UnexpectedResponse],
          second.isInstanceOf[SchemaAcquisitionError.SchemaParsingFailed],
          !reported.contains(secret)
        )
      }
    ),

    // ---------------------------------------------------------------------------------------
    // Task 8: conditional requests
    //
    // Hive's CDN honours `ETag` / `If-None-Match` and answers `304` when the supergraph has not
    // changed, which is the common case on every poll. This is an optimization, not a correctness
    // feature: fingerprint dedup in `ReloadableGatewayInterpreterImpl.cycle` already suppresses the
    // swap. The risk being gated is therefore a silently-never-firing optimization, not a wrong
    // answer, which is exactly what a green suite hides if it only ever exercises `200`.
    // ---------------------------------------------------------------------------------------
    suite("conditional requests")(
      test("a first load is unconditional, and stores the tag the response carried") {
        for {
          // Both answers are `200`, so this isolates storing and re-sending the tag from whether a
          // `304` is handled. That has its own tests below, and a shared fixture would report one
          // bug twice.
          cdn    <- recordingEndpoint(Answer.sdl(etag = Some("\"v1\"")))
          loader <- acquisitionLoader(httpSource(cdn.endpoint, identity))
          first  <- loader
          _      <- loader
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
          loader <- acquisitionLoader(httpSource(cdn.endpoint, identity))
          first  <- loader
          second <- loader
          calls  <- cdn.calls
        } yield assertTrue(first == second, calls == 2)
      },
      test("a 304 with nothing cached is a protocol violation, not an empty success") {
        // A server answering `304` to an unconditional request. Succeeding with no document would
        // hand the caller a supergraph it never received.
        for {
          cdn   <- recordingEndpoint(Answer.notModified)
          exit  <- load(httpSource(cdn.endpoint, identity))
          error <- acquisitionFailure(exit)
        } yield assertTrue(error.isInstanceOf[SchemaAcquisitionError.UnexpectedResponse])
      },
      test("a 200 carrying no ETag leaves the next request unconditional") {
        // The stored tag has to be cleared, not kept: re-sending a tag the origin no longer knows
        // about invites a `304` for a document that has in fact changed.
        for {
          cdn    <- recordingEndpoint(Answer.sdl(etag = Some("\"v1\"")), Answer.sdl(etag = None), Answer.sdl())
          loader <- acquisitionLoader(httpSource(cdn.endpoint, identity))
          _      <- loader
          _      <- loader
          _      <- loader
          second <- cdn.conditionOf(1)
          third  <- cdn.conditionOf(2)
        } yield assertTrue(second.contains("\"v1\""), third.isEmpty)
      },
      test("a redirecting chain stores the final response's tag and sends it on every hop") {
        for {
          storage <- recordingEndpoint(Answer.sdl(etag = Some("\"storage-object\"")))
          cdn     <- recordingEndpoint(Answer.redirect(storage.endpoint.encode, etag = Some("\"cdn-v1\"")))
          loader  <- acquisitionLoader(httpSource(cdn.endpoint, _.withMaxRedirects(2)))
          _       <- loader
          _       <- loader
          first   <- cdn.conditionOf(0)
          second  <- cdn.conditionOf(1)
          onward  <- storage.conditionOf(1)
        } yield assertTrue(
          first.isEmpty,
          second.contains("\"storage-object\""),
          onward.contains("\"storage-object\"")
        )
      },
      test("a 304 from a redirect target answers from the cache") {
        for {
          storage <- recordingEndpoint(Answer.sdl(etag = Some("\"storage-object\"")), Answer.notModified)
          cdn     <- recordingEndpoint(Answer.redirect(storage.endpoint.encode, etag = Some("\"cdn-v1\"")))
          loader  <- acquisitionLoader(httpSource(cdn.endpoint, _.withMaxRedirects(2)))
          first   <- loader
          second  <- loader
        } yield assertTrue(first == second)
      },
      test("a 304 through a redirecting chain never contacts the storage host") {
        // The assertion the whole suite exists for. A direct-route-only gate is passed by an
        // implementation that follows the redirect first and only then discovers the `304`, which
        // costs exactly the round trip the conditional request was added to avoid.
        for {
          storage      <- recordingEndpoint(Answer.sdl(etag = Some("\"storage-object\"")))
          cdn          <- recordingEndpoint(
                            Answer.redirect(storage.endpoint.encode, etag = Some("\"cdn-v1\"")),
                            Answer.notModified
                          )
          loader       <- acquisitionLoader(httpSource(cdn.endpoint, _.withMaxRedirects(2)))
          first        <- loader
          second       <- loader
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
    // Task 10: Supergraph.hive
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
              config.headers.map(header => RemoteGraphQLConfig.headerName(header) -> header.renderedValue) == List(
                "X-Hive-CDN-Key" -> "cdn-key"
              ),
              // Hive answers a 302 to presigned storage, so a bound of zero would never reach the artifact.
              config.maxRedirects >= 1
            )
          case other                                    => assertTrue(false, other.toString.isEmpty)
        }
      },
      test("requests the artifact path and authenticates with the CDN key") {
        for {
          cdn      <- recordingEndpoint(Answer.sdl())
          loader   <- acquisitionLoader(Supergraph.hive("target-1", Secret("cdn-key"), cdn.endpoint).source)
          document <- loader
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
          cdn      <- recordingEndpoint(Answer.redirect(storage.endpoint.encode, etag = None))
          loader   <- acquisitionLoader(Supergraph.hive("target-1", Secret("cdn-key"), cdn.endpoint).source)
          document <- loader
          key      <- cdn.keyOf(0)
          onward   <- storage.keyOf(0)
          calls    <- cdn.calls.zip(storage.calls)
        } yield assertTrue(
          queryFields(document) == List("hello"),
          key.contains("cdn-key"),
          onward.isEmpty,
          calls == ((1, 1))
        )
      }
    )
  ).provide(testServer, stubIds, httpClient)
}
