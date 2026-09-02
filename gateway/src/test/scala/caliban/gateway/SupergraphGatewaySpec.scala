package caliban.gateway

import caliban.Value.StringValue
import caliban.gateway.GatewayTestSupport._
import sttp.model.Uri
import zio.Config.Secret
import zio._
import zio.http.{ Body, Header, Headers, MediaType, Response, Server, Status }
import zio.test._

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.io.{ Source => ScalaSource }

/**
 * Reload behaviour for a gateway built from a supergraph rather than a hand-listed subgraph set.
 *
 * The distinguishing property is that a single document drives every generation: the supergraph is
 * re-read on each cycle, and the subgraphs it decomposes into are pinned, so a poll costs exactly
 * one request no matter how many graphs the supergraph declares.
 */
object SupergraphGatewaySpec extends ZIOSpecDefault {

  private implicit final class TestGatewayOps[R](private val gateway: Gateway[R]) extends AnyVal {

    /** One-second polls with no jitter; sources without a published floor accept any interval. */
    def reloadableForTest(implicit trace: Trace): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] =
      reloadableEvery(1.second)

    def reloadableEvery(interval: Duration)(implicit
      trace: Trace
    ): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] =
      gateway.withConfig(_.withReloadPollInterval(interval).withReloadJitter(0.0)).reloadable
  }

  private def resource(name: String): UIO[String] =
    ZIO
      .scoped(ZIO.fromAutoCloseable(ZIO.attempt(ScalaSource.fromResource(s"supergraph/$name"))).map(_.mkString))
      .orDie

  private val supergraphSchema: UIO[String] = resource("supergraph.graphql")

  /** A root field only `characters` can resolve, so activating it proves the new generation is live. */
  private def withCrew(sdl: String): String =
    sdl.replace(
      "  character(name: String!): Character @join__field(graph: CHARACTERS)",
      """  character(name: String!): Character @join__field(graph: CHARACTERS)
        |  crew: [Character!]! @join__field(graph: CHARACTERS)""".stripMargin
    )

  /** Parses, but names a graph the enum omits — a decomposition failure rather than a parse failure. */
  private def malformed(sdl: String): String =
    sdl.replace("@join__type(graph: EPISODES, key: \"name\")", "@join__type(graph: MISSING, key: \"name\")")

  /** Comments and indentation only: the fingerprint is taken over the parsed document. */
  private def reformatted(sdl: String): String =
    "# republished, unchanged\n" + sdl.replace("type Query\n", "type Query\n\n")

  private final case class Source(
    endpoint: Uri,
    document: Ref[String],
    fetches: Ref[Int],
    characters: Stub,
    episodes: Stub
  ) {

    /** The fixture's routing urls point at fixed ports nothing listens on, so every test redirects them. */
    def endpoints: String => Option[Uri] =
      Map("characters" -> characters.endpoint, "episodes" -> episodes.endpoint).get

    def supergraph: Supergraph[Any] = Supergraph.http(endpoint).withSubgraphEndpoint(endpoints)

    def setSchema(sdl: String): UIO[Unit] = document.set(sdl)

    /** Every request the projected subgraphs received, so acquisition traffic can be ruled out. */
    def subgraphQueries: UIO[Vector[String]] =
      characters.requests.get.zipWith(episodes.requests.get)(_ ++ _).map(_.flatMap(_.query))
  }

  private def source(sdl: String): ZIO[Server with Ref[Int], Nothing, Source] =
    for {
      document   <- Ref.make(sdl)
      fetches    <- Ref.make(0)
      characters <-
        stubByRequestZIO(_ =>
          ZIO.succeed(
            """{"data":{"characters":[{"name":"Naomi"}],"crew":[{"name":"Amos"}],"character":{"name":"Naomi"}}}"""
          )
        )
      episodes   <- stubByRequestZIO(_ => ZIO.succeed("""{"data":{"episodes":[{"name":"Dulcinea"}]}}"""))
      endpoint   <- getEndpoint("supergraph") { _ =>
                      fetches.update(_ + 1) *> document.get.map { text =>
                        Response(
                          Status.Ok,
                          Headers(Header.ContentType(MediaType.text.plain).untyped),
                          Body.fromString(text)
                        )
                      }
                    }
    } yield Source(endpoint, document, fetches, characters, episodes)

  // The next poll timer is installed only after the current refresh and retirement finish.
  private def awaitPoll(interval: Duration): UIO[Unit] =
    Clock.instant.flatMap(now => TestClock.sleeps.repeatUntil(_.contains(now.plus(interval)))).unit

  private def poll(runtime: ReloadableGatewayInterpreter[_], interval: Duration = 1.second): UIO[Option[String]] =
    TestClock.adjust(interval) *> awaitPoll(interval) *> runtime.lastReloadFailure

  private def names(value: caliban.ResponseValue, root: String): List[Option[caliban.ResponseValue]] =
    listValues(field(value, root)).map(field(_, "name"))

  def spec = suite("Supergraph gateway")(
    test("serves every graph the supergraph declares, through the configured endpoints") {
      for {
        sdl     <- supergraphSchema
        remote  <- source(sdl)
        runtime <- Gateway.fromSupergraph(remote.supergraph).reloadableForTest
        result  <- runtime.execute("{ characters { name } episodes { name } }")
        queries <- remote.subgraphQueries
        fetches <- remote.fetches.get
      } yield assertTrue(
        result.errors.isEmpty,
        names(result.data, "characters") == List(Some(StringValue("Naomi"))),
        names(result.data, "episodes") == List(Some(StringValue("Dulcinea"))),
        fetches == 1,
        queries.size == 2,
        // The projections are pinned, so no generation ever acquires a subgraph schema.
        !queries.exists(query => query.contains("_service") || query.contains("__schema"))
      )
    },
    test("activates a new generation when the supergraph is republished with a changed schema") {
      for {
        sdl     <- supergraphSchema
        remote  <- source(sdl)
        runtime <- Gateway.fromSupergraph(remote.supergraph).reloadableForTest
        before  <- runtime.check("{ crew { name } }").exit
        _       <- remote.setSchema(withCrew(sdl))
        failed  <- poll(runtime)
        after   <- runtime.execute("{ crew { name } }")
        queries <- remote.subgraphQueries
        fetches <- remote.fetches.get
      } yield assertTrue(
        before.isFailure,
        failed.isEmpty,
        after.errors.isEmpty,
        names(after.data, "crew") == List(Some(StringValue("Amos"))),
        // One supergraph read per cycle regardless of how many graphs it decomposes into.
        fetches == 2,
        !queries.exists(query => query.contains("_service") || query.contains("__schema"))
      )
    },
    test("preserves the generation and warm cache when the republished supergraph is unchanged") {
      for {
        sdl              <- supergraphSchema
        recorded         <- recordEvents
        (events, wrapper) = recorded
        remote           <- source(sdl)
        runtime          <- (Gateway.fromSupergraph(remote.supergraph) @@ wrapper).reloadableForTest
        _                <- runtime.execute("{ characters { name } }")
        _                <- remote.setSchema(reformatted(sdl))
        failed           <- poll(runtime)
        result           <- runtime.execute("{ characters { name } }")
        observed         <- events.get
        fetches          <- remote.fetches.get
      } yield assertTrue(
        failed.isEmpty,
        result.errors.isEmpty,
        fetches == 2,
        observed.count(_ == GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Miss)) == 1,
        observed.count(_ == GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Hit)) == 1
      )
    },
    test("retains the previous generation when the republished supergraph cannot be decomposed") {
      for {
        sdl       <- supergraphSchema
        remote    <- source(sdl)
        runtime   <- Gateway.fromSupergraph(remote.supergraph).reloadableForTest
        _         <- remote.setSchema(malformed(sdl))
        failed    <- poll(runtime)
        result    <- runtime.execute("{ characters { name } }")
        _         <- remote.setSchema(withCrew(sdl))
        recovered <- poll(runtime)
        after     <- runtime.execute("{ crew { name } }")
      } yield assertTrue(
        // A fixed reason: the diagnostic never carries the supergraph text or a graph name from it.
        failed.contains("Unable to decompose supergraph into subgraphs."),
        result.errors.isEmpty,
        recovered.isEmpty,
        after.errors.isEmpty
      )
    },
    test("retains the previous generation when the supergraph endpoint stops serving it") {
      for {
        sdl       <- supergraphSchema
        remote    <- source(sdl)
        runtime   <- Gateway.fromSupergraph(remote.supergraph).reloadableForTest
        _         <- remote.setSchema("not a supergraph")
        failed    <- poll(runtime)
        result    <- runtime.execute("{ characters { name } }")
        _         <- remote.setSchema(sdl)
        recovered <- poll(runtime)
      } yield assertTrue(
        failed.contains("Unable to load supergraph."),
        result.errors.isEmpty,
        recovered.isEmpty
      )
    },
    test("rejects reload from a source that cannot be re-read, but still builds an interpreter") {
      for {
        sdl      <- supergraphSchema
        remote   <- source(sdl)
        pinned    = remote.supergraph
        static    = Supergraph.sdl(sdl).withSubgraphEndpoint(remote.endpoints)
        rejected <- Gateway.fromSupergraph(static).reloadableForTest.exit
        runtime  <- Gateway.fromSupergraph(static).interpreter
        result   <- runtime.execute("{ characters { name } episodes { name } }")
        // The same document over http is refreshable, so only the source distinguishes the two.
        accepted <- Gateway.fromSupergraph(pinned).reloadableForTest.exit
        fetches  <- remote.fetches.get
      } yield assertTrue(
        buildDiagnostics(rejected) == List("Gateway reload from supergraph requires a remote source."),
        result.errors.isEmpty,
        names(result.data, "characters") == List(Some(StringValue("Naomi"))),
        accepted.isSuccess,
        // The static source never reaches the network; only the reloadable one reads the endpoint.
        fetches == 1
      )
    },
    test("re-reads a file source, so a supergraph rotated on disk activates a new generation") {
      for {
        sdl     <- supergraphSchema
        remote  <- source(sdl)
        path    <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempFile("supergraph", ".graphql")).orDie)(path =>
                     ZIO.attemptBlocking(Files.deleteIfExists(path)).ignore
                   )
        write    =
          (text: String) => ZIO.attemptBlocking(Files.write(path, text.getBytes(StandardCharsets.UTF_8))).unit.orDie
        _       <- write(sdl)
        runtime <-
          Gateway.fromSupergraph(Supergraph.file(path).withSubgraphEndpoint(remote.endpoints)).reloadableForTest
        before  <- runtime.check("{ crew { name } }").exit
        _       <- write(withCrew(sdl))
        failed  <- poll(runtime)
        after   <- runtime.execute("{ crew { name } }")
        fetches <- remote.fetches.get
      } yield assertTrue(
        before.isFailure,
        failed.isEmpty,
        after.errors.isEmpty,
        names(after.data, "crew") == List(Some(StringValue("Amos"))),
        // The file drives every generation on its own; the http endpoint is never consulted.
        fetches == 0
      )
    },
    uplinkSuite
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential @@ TestAspect.timeout(30.seconds)

  // ===============================================================================================
  // The same reload machinery, driven by the Apollo uplink protocol instead of a plain http GET.
  // ===============================================================================================

  private val graphRef = "caliban-gateway@production"
  private val apiKey   = Secret("service:caliban-gateway:s3cr3t-uplink-key")

  /**
   * Uplink's published floor is ten seconds, so this is the fastest poll `reloadable` accepts, and
   * jitter has to be zero: `GatewayConfig.minimumReloadPollInterval` is what the floor is checked
   * against, and the default twenty percent would put it at eight.
   */
  private val uplinkPollInterval = 10.seconds

  /**
   * Generous relative to the poll interval, so advancing the test clock by a whole interval can
   * never be mistaken for an acquisition that ran out of time.
   */
  private val uplinkAcquisition = RemoteGraphQLConfig.Acquisition.default.withTimeout(5.minutes)

  private def uplinkSource(endpoint: Uri) =
    SupergraphUplinkConfig(graphRef, apiKey).withEndpoints(endpoint).withAcquisition(uplinkAcquisition)

  private final case class Uplink(uplink: Stub, answer: Ref[String], characters: Stub, episodes: Stub) {
    def supergraph: Supergraph[Any] =
      Supergraph
        .uplink(uplinkSource(uplink.endpoint))
        .withSubgraphEndpoint(Map("characters" -> characters.endpoint, "episodes" -> episodes.endpoint).get)

    /** What every later poll is answered with, until it is set again. */
    def serve(body: String): UIO[Unit] = answer.set(body)

    def polls: UIO[Int] = uplink.requests.get.map(_.size)

    /** The `ifAfterId` variable of the nth poll, or `None` when it was absent or JSON null. */
    def cursorOf(index: Int): UIO[Option[String]] =
      uplink.requests.get.map(
        _.lift(index).flatMap(_.variables).flatMap(_.get("ifAfterId")).collect { case StringValue(value) => value }
      )

    def subgraphQueries: UIO[Vector[String]] =
      characters.requests.get.zipWith(episodes.requests.get)(_ ++ _).map(_.flatMap(_.query))
  }

  private def uplinkOf(sdl: String): ZIO[Server with Ref[Int], Nothing, Uplink] =
    for {
      answer     <- Ref.make(uplinkConfigResult("id-1", sdl))
      // The uplink speaks GraphQL over POST and answers `application/graphql-response+json`, which is
      // exactly what the subgraph stub already does.
      uplink     <- stubByRequestZIO(_ => answer.get)
      characters <-
        stubByRequestZIO(_ =>
          ZIO.succeed(
            """{"data":{"characters":[{"name":"Naomi"}],"crew":[{"name":"Amos"}],"character":{"name":"Naomi"}}}"""
          )
        )
      episodes   <- stubByRequestZIO(_ => ZIO.succeed("""{"data":{"episodes":[{"name":"Dulcinea"}]}}"""))
    } yield Uplink(uplink, answer, characters, episodes)

  private def uplinkPoll(runtime: ReloadableGatewayInterpreter[_]): UIO[Option[String]] =
    poll(runtime, uplinkPollInterval)

  private val uplinkSuite = suite("uplink source")(
    test("builds from one uplink poll, which carries no cursor, and serves every graph") {
      for {
        sdl     <- supergraphSchema
        remote  <- uplinkOf(sdl)
        runtime <- Gateway.fromSupergraph(remote.supergraph).reloadableEvery(uplinkPollInterval)
        result  <- runtime.execute("{ characters { name } episodes { name } }")
        polls   <- remote.polls
        cursor  <- remote.cursorOf(0)
        queries <- remote.subgraphQueries
      } yield assertTrue(
        result.errors.isEmpty,
        names(result.data, "characters") == List(Some(StringValue("Naomi"))),
        names(result.data, "episodes") == List(Some(StringValue("Dulcinea"))),
        polls == 1,
        cursor.isEmpty,
        // The projections are pinned, so no generation ever acquires a subgraph schema.
        !queries.exists(query => query.contains("_service") || query.contains("__schema"))
      )
    },
    test("activates a new generation when the uplink answers a republished supergraph") {
      for {
        sdl     <- supergraphSchema
        remote  <- uplinkOf(sdl)
        runtime <- Gateway.fromSupergraph(remote.supergraph).reloadableEvery(uplinkPollInterval)
        before  <- runtime.check("{ crew { name } }").exit
        _       <- remote.serve(uplinkConfigResult("id-2", withCrew(sdl)))
        failed  <- uplinkPoll(runtime)
        after   <- runtime.execute("{ crew { name } }")
        cursor  <- remote.cursorOf(1)
        polls   <- remote.polls
      } yield assertTrue(
        before.isFailure,
        failed.isEmpty,
        after.errors.isEmpty,
        names(after.data, "crew") == List(Some(StringValue("Amos"))),
        // The second poll acknowledges the id the first one was answered with. A loader rebuilt per
        // cycle would send no cursor here, and the uplink would resend the whole supergraph forever.
        cursor.contains("id-1"),
        polls == 2
      )
    },
    test("preserves the generation and warm cache when the uplink answers Unchanged") {
      for {
        sdl              <- supergraphSchema
        recorded         <- recordEvents
        (events, wrapper) = recorded
        remote           <- uplinkOf(sdl)
        runtime          <- (Gateway.fromSupergraph(remote.supergraph) @@ wrapper).reloadableEvery(uplinkPollInterval)
        _                <- runtime.execute("{ characters { name } }")
        _                <- remote.serve(uplinkUnchanged("id-1"))
        failed           <- uplinkPoll(runtime)
        result           <- runtime.execute("{ characters { name } }")
        observed         <- events.get
        polls            <- remote.polls
      } yield assertTrue(
        failed.isEmpty,
        result.errors.isEmpty,
        polls == 2,
        // No swap: the second execution reuses the first generation's warm operation cache.
        observed.count(_ == GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Miss)) == 1,
        observed.count(_ == GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Hit)) == 1
      )
    },
    test("retains the previous generation when the uplink answers a FetchError") {
      for {
        sdl       <- supergraphSchema
        remote    <- uplinkOf(sdl)
        runtime   <- Gateway.fromSupergraph(remote.supergraph).reloadableEvery(uplinkPollInterval)
        _         <- remote.serve(uplinkFetchError("AUTHENTICATION_FAILED", "invalid key service:caliban-gateway:xyz"))
        failed    <- uplinkPoll(runtime)
        result    <- runtime.execute("{ characters { name } }")
        _         <- remote.serve(uplinkConfigResult("id-2", sdl))
        recovered <- uplinkPoll(runtime)
      } yield assertTrue(
        // A fixed reason. The uplink error code and the remote message it came with are both absent,
        // and so is the api key the failing request carried.
        failed.contains("Unable to load supergraph."),
        !failed.exists(_.contains("AUTHENTICATION_FAILED")),
        !failed.exists(_.contains("invalid key")),
        !failed.exists(_.contains(apiKey.stringValue)),
        result.errors.isEmpty,
        recovered.isEmpty
      )
    },
    test("rejects a poll interval whose jittered minimum breaches the ten-second uplink floor") {
      val floor    = "Supergraph uplink polling requires a reload poll interval of at least ten seconds."
      val describe = Gateway.fromSupergraph(Supergraph.uplink(graphRef, apiKey))
      for {
        tooFast  <- describe.reloadableEvery(1.second).exit
        // Ten seconds nominal, but the default twenty percent jitter polls as fast as eight.
        jittered <- describe.withConfig(_.withReloadPollInterval(10.seconds)).reloadable.exit
      } yield assertTrue(
        buildDiagnostics(tooFast) == List(floor),
        buildDiagnostics(jittered) == List(floor),
        // The shipped default is thirty seconds with the same jitter, so it already clears the floor.
        GatewayConfig.default.minimumReloadPollInterval >= 10.seconds
      )
    },
    test("rejects an unusable uplink configuration before any poll is made") {
      // Without this the description builds and every poll fails at load time instead.
      val blank = SupergraphUplinkConfig(graphRef, Secret(""))
      for {
        rejected <- Gateway.fromSupergraph(Supergraph.uplink(blank)).reloadableEvery(uplinkPollInterval).exit
      } yield assertTrue(buildDiagnostics(rejected) == List("Supergraph uplink apikey must not be empty."))
    }
  )
}
