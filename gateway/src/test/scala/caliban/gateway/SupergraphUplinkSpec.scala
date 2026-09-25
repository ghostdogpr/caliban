package caliban.gateway

import caliban.Value.{ NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.SchemaAcquisitionError._
import caliban.gateway.SupergraphAcquisitionError.UplinkFetchFailed
import caliban.parsing.adt.Document
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, writeToString }
import zio.Config.Secret
import zio._
import zio.http._
import zio.test._

/**
 * Gates for the Apollo uplink source: the configuration it is described by, the error cases it can
 * fail with, the cursor and cache the loader carries across polls, and endpoint failover.
 *
 * The loader is stateful across runs by design, since `Gateway` builds it once per gateway
 * rather than once per reload cycle, so every test here runs one loader more than once.
 */
object SupergraphUplinkSpec extends ZIOSpecDefault {

  /** Nests five deep inside the field definition, well past the response envelope's own three. */
  private val deeplyNestedSdl =
    minimalSupergraphSdl.replace("hello: String", "hello(filter: Filter = { a: { b: [1] } }): String")

  // ---------------------------------------------------------------------------------------------
  // Uplink protocol responses
  // ---------------------------------------------------------------------------------------------

  private val graphQLErrors: String =
    writeToString(GraphQLResponse[CalibanError](NullValue, List(CalibanError.ExecutionError("denied"))))

  // ---------------------------------------------------------------------------------------------
  // A stub speaking the uplink protocol
  // ---------------------------------------------------------------------------------------------

  private final case class Answer(
    body: String,
    status: Status = Status.Ok,
    delay: Duration = Duration.Zero,
    contentType: String = "application/graphql-response+json"
  )

  private final case class Uplink(endpoint: URL, requests: Ref[Vector[GraphQLRequest]], answers: Ref[List[Answer]]) {
    def calls: UIO[Int] = requests.get.map(_.size)

    def cursorOf(index: Int): UIO[Option[String]] = uplinkCursor(requests, index)

    def variablesOf(index: Int): UIO[Map[String, caliban.InputValue]] =
      requests.get.map(_.lift(index).flatMap(_.variables).getOrElse(Map.empty))

    def queryOf(index: Int): UIO[String] =
      requests.get.map(_.lift(index).flatMap(_.query).getOrElse(""))
  }

  /** Answers each request with the head of `answers`, keeping the last one once the list runs out. */
  private def uplinkStub(answers: Answer*): ZIO[Server with Ref[Int], Nothing, Uplink] =
    for {
      recorded  <- Ref.make(Vector.empty[GraphQLRequest])
      remaining <- Ref.make(answers.toList)
      endpoint  <- postEndpoint("uplink") { request =>
                     for {
                       bytes  <- request.body.asArray.orDie
                       _      <- recorded.update(_ :+ readFromArray[GraphQLRequest](bytes))
                       answer <- nextAnswer(remaining, Answer("{}"))
                       _      <- ZIO.sleep(answer.delay).when(answer.delay > Duration.Zero)
                     } yield Response(
                       answer.status,
                       Headers(Header.Custom("Content-Type", answer.contentType)),
                       Body.fromString(answer.body)
                     )
                   }
    } yield Uplink(endpoint, recorded, remaining)

  // ---------------------------------------------------------------------------------------------
  // Loader construction
  // ---------------------------------------------------------------------------------------------

  private def configFor(endpoints: URL*): SupergraphUplinkConfig =
    SupergraphUplinkConfig(graphRef, apiKey).withEndpoints(endpoints: _*).withAcquisition(_.withTimeout(2.seconds))

  private def loaderFor(
    config: SupergraphUplinkConfig
  ): ZIO[GatewayHttpClient, Nothing, IO[SupergraphAcquisitionError, Document]] =
    acquisitionLoader(Supergraph.Source.Uplink(config))

  private def loaderFor(endpoints: URL*): ZIO[GatewayHttpClient, Nothing, IO[SupergraphAcquisitionError, Document]] =
    loaderFor(configFor(endpoints: _*))

  /** Every string a diagnostic must never contain: the api key, and any remote free text. */
  private def leaks(diagnostics: List[String], secrets: String*): List[String] =
    secrets.filter(secret => diagnostics.exists(_.contains(secret))).toList

  // =============================================================================================

  def spec = suite("SupergraphUplinkSpec")(
    // -------------------------------------------------------------------------------------------
    // Task 2: Supergraph.Source.Uplink and SupergraphUplinkConfig
    // -------------------------------------------------------------------------------------------
    suite("configuration")(
      test("defaults to Apollo's two published endpoints, gcp first") {
        val config = SupergraphUplinkConfig(graphRef, apiKey)
        assertTrue(
          config.endpoints.map(_.toString) == List(
            "https://uplink.api.apollographql.com/",
            "https://aws.uplink.api.apollographql.com/"
          ),
          config.endpoints == SupergraphUplinkConfig.DefaultEndpoints,
          config.acquisition == RemoteGraphQLConfig.Acquisition.default,
          config.diagnostics.isEmpty
        )
      },
      test("withEndpoints replaces the endpoint list rather than appending to the defaults") {
        // Appending would silently keep Apollo's public endpoints in the rotation for anyone
        // pointing at a proxy or a test double.
        val only = url"https://uplink.internal/"
        assertTrue(SupergraphUplinkConfig(graphRef, apiKey).withEndpoints(only).endpoints == List(only))
      },
      test("an empty graph ref is a diagnostic") {
        assertTrue(
          SupergraphUplinkConfig("", apiKey).diagnostics == List("Supergraph uplink graph ref must not be empty.")
        )
      },
      test("an empty api key is a diagnostic") {
        assertTrue(
          SupergraphUplinkConfig(graphRef, Secret("")).diagnostics == List(
            "Supergraph uplink apikey must not be empty."
          )
        )
      },
      test("an empty endpoint list is a diagnostic") {
        assertTrue(
          SupergraphUplinkConfig(graphRef, apiKey).withEndpoints().diagnostics == List(
            "Supergraph uplink must have at least one endpoint."
          )
        )
      },
      test("acquisition diagnostics are carried through") {
        val config = SupergraphUplinkConfig(graphRef, apiKey)
          .withAcquisition(_.withMaxResponseBytes(0))
        assertTrue(config.diagnostics == List("Schema acquisition maxResponseBytes must be positive."))
      },
      test("no diagnostic renders the api key, and neither does toString") {
        val config = SupergraphUplinkConfig("", Secret("")).withEndpoints()
        assertTrue(
          config.diagnostics.size == 3,
          leaks(config.diagnostics, apiKey.stringValue, graphRef).isEmpty,
          !SupergraphUplinkConfig(graphRef, apiKey).toString.contains(apiKey.stringValue)
        )
      },
      test("Supergraph.uplink describes an uplink source") {
        val fromParts                           = Supergraph.uplink(graphRef, apiKey)
        val fromConfig                          = Supergraph.uplink(SupergraphUplinkConfig(graphRef, apiKey))
        def uplink(supergraph: Supergraph[Any]) = supergraph.source match {
          case Supergraph.Source.Uplink(config) => Some((config.graphRef, config.apiKey, config.endpoints))
          case _                                => None
        }
        assertTrue(
          uplink(fromParts) == Some((graphRef, apiKey, SupergraphUplinkConfig.DefaultEndpoints)),
          uplink(fromConfig) == uplink(fromParts)
        )
      }
    ),

    // -------------------------------------------------------------------------------------------
    // Task 3: SupergraphAcquisitionError uplink cases
    // -------------------------------------------------------------------------------------------
    suite("error cases")(
      test("every uplink case renders a non-empty diagnostic") {
        val paths                                    = List("$.data", "$.data.routerConfig.supergraphSDL")
        val errors: List[SupergraphAcquisitionError] =
          UplinkFetchFailed("AUTHENTICATION_FAILED") :: paths.map(InvalidResponse(_))

        assertTrue(
          errors.forall(_.diagnostics.nonEmpty),
          errors.forall(_.diagnostics.forall(_.trim.nonEmpty)),
          errors.head.diagnostics.exists(_.contains("AUTHENTICATION_FAILED")),
          errors.head.getMessage.contains("AUTHENTICATION_FAILED"),
          // Distinct reasons must not collapse to the same message, or a diagnostic tells you nothing.
          paths.map(InvalidResponse(_).diagnostics).distinct.size == paths.size
        )
      },
      test("a caused error keeps its payload in the cause channel, never in the diagnostic") {
        val cause      = new IllegalStateException("Bearer service:caliban-gateway:s3cr3t-uplink-key")
        val error      = RequestFailed(cause)
        val buildError = GatewayBuildError.SupergraphAcquisitionFailed(error)
        assertTrue(
          error.getCause == cause,
          buildError.getCause eq error,
          buildError.getCause.getCause eq cause,
          buildError.diagnostics == error.diagnostics.map(message => s"[supergraph] $message"),
          leaks(buildError.diagnostics, apiKey.stringValue).isEmpty
        )
      }
    ),

    // -------------------------------------------------------------------------------------------
    // Task 4: the uplink loader
    // -------------------------------------------------------------------------------------------
    suite("loader")(
      test("sends the SupergraphSdl operation with the api key and graph ref as variables") {
        for {
          stub   <- uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)))
          loader <- loaderFor(stub.endpoint)
          _      <- loader
          query  <- stub.queryOf(0)
          vars   <- stub.variablesOf(0)
        } yield assertTrue(
          query.contains("SupergraphSdl"),
          query.contains("routerConfig"),
          query.contains("RouterConfigResult"),
          query.contains("FetchError"),
          vars.get("apiKey").contains(StringValue(apiKey.stringValue)),
          vars.get("ref").contains(StringValue(graphRef))
        )
      },
      test("advances the cursor: the first poll sends no id, the second sends the one it received") {
        // The assertion that catches a regression of the loader-lifetime refactor. A loader rebuilt
        // per reload cycle would send no cursor on every poll.
        for {
          stub    <- uplinkStub(
                       Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)),
                       Answer(uplinkConfigResult("id-2", changedSupergraphSdl))
                     )
          loader  <- loaderFor(stub.endpoint)
          first   <- loader
          second  <- loader
          cursor0 <- stub.cursorOf(0)
          cursor1 <- stub.cursorOf(1)
          _       <- loader
          cursor2 <- stub.cursorOf(2)
        } yield assertTrue(
          cursor0.isEmpty,
          cursor1.contains("id-1"),
          cursor2.contains("id-2"),
          queryFields(first) == List("hello"),
          queryFields(second) == List("hello", "goodbye")
        )
      },
      test("Unchanged returns the document last fetched, without re-parsing anything") {
        // The reason the cache holds the last *fetched* document rather than the last *activated*
        // one: the caller compares against the active generation, so it must keep seeing the new
        // document until it manages to build it. Caching the activated one wedges the gateway.
        for {
          stub   <- uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)), Answer(uplinkUnchanged("id-1")))
          loader <- loaderFor(stub.endpoint)
          first  <- loader
          second <- loader
          calls  <- stub.calls
        } yield assertTrue(first == second, calls == 2)
      },
      test("Unchanged with nothing cached is a protocol violation, not an empty success") {
        for {
          stub    <- uplinkStub(Answer(uplinkUnchanged("id-1")), Answer(uplinkConfigResult("id-2", minimalSupergraphSdl)))
          loader  <- loaderFor(stub.endpoint)
          exit    <- loader.exit
          error   <- acquisitionFailure(exit)
          _       <- loader
          cursor1 <- stub.cursorOf(1)
        } yield assertTrue(
          error == InvalidResponse("$.data.routerConfig.supergraphSDL"),
          // Acknowledging an id we have no document for would have every later poll ask the same
          // unanswerable question, and the loader would never recover a full fetch.
          cursor1.isEmpty
        )
      },
      test("FetchError maps to the code, and never carries the remote message") {
        for {
          stub   <- uplinkStub(Answer(uplinkFetchError("AUTHENTICATION_FAILED", "invalid key service:xyz for graph")))
          loader <- loaderFor(stub.endpoint)
          exit   <- loader.exit
          error  <- acquisitionFailure(exit)
        } yield assertTrue(
          error == UplinkFetchFailed("AUTHENTICATION_FAILED"),
          error.diagnostics.exists(_.contains("AUTHENTICATION_FAILED")),
          leaks(error.diagnostics, "invalid key", "service:xyz").isEmpty
        )
      },
      test("a supergraph schema parsing failure does not advance the cursor") {
        for {
          stub    <- uplinkStub(
                       Answer(uplinkConfigResult("id-1", "type Query {")),
                       Answer(uplinkConfigResult("id-2", minimalSupergraphSdl))
                     )
          loader  <- loaderFor(stub.endpoint)
          exit    <- loader.exit
          error   <- acquisitionFailure(exit)
          _       <- loader
          cursor1 <- stub.cursorOf(1)
        } yield assertTrue(
          error.isInstanceOf[SchemaParsingFailed],
          // A document we could not parse must not be acknowledged, or it is never re-offered.
          cursor1.isEmpty
        )
      },
      test("graphql errors in the uplink response are an invalid response") {
        for {
          stub   <- uplinkStub(Answer(graphQLErrors))
          loader <- loaderFor(stub.endpoint)
          exit   <- loader.exit
          error  <- acquisitionFailure(exit)
        } yield assertTrue(
          error == InvalidResponse("$.errors"),
          leaks(error.diagnostics, "denied").isEmpty
        )
      },
      test("an undecodable body is an invalid response rather than a defect") {
        for {
          stub   <- uplinkStub(Answer("not json"))
          loader <- loaderFor(stub.endpoint)
          exit   <- loader.exit
          error  <- acquisitionFailure(exit)
        } yield assertTrue(error.isInstanceOf[ResponseDecodingFailed])
      },
      test("a body larger than maxResponseBytes is rejected before it is decoded") {
        for {
          stub   <- uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)))
          loader <- loaderFor(configFor(stub.endpoint).withAcquisition(_.withMaxResponseBytes(32)))
          exit   <- loader.exit
          error  <- acquisitionFailure(exit)
        } yield assertTrue(error == ResponseTooLarge(32))
      },
      test("a response envelope nested past maxParsingDepth is rejected before it is decoded") {
        // The uplink answers GraphQL JSON, so the envelope is bounded as JSON. `{"data":{"routerConfig":{`
        // is already three deep, and the supergraph itself is a string inside it.
        for {
          stub   <- uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)))
          loader <- loaderFor(configFor(stub.endpoint).withAcquisition(_.withMaxParsingDepth(2)))
          exit   <- loader.exit
          error  <- acquisitionFailure(exit)
        } yield assertTrue(error == ParsingDepthExceeded(2))
      },
      test("a supergraph nested past maxParsingDepth is rejected even when the envelope clears it") {
        // The same bound applies twice, to two different grammars. At four the envelope passes, so
        // only the supergraph's own nesting can be what rejects the second load.
        for {
          stub    <- uplinkStub(
                       Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)),
                       Answer(uplinkConfigResult("id-2", deeplyNestedSdl))
                     )
          loader  <- loaderFor(configFor(stub.endpoint).withAcquisition(_.withMaxParsingDepth(4)))
          shallow <- loader.exit
          exit    <- loader.exit
          error   <- acquisitionFailure(exit)
        } yield assertTrue(shallow.isSuccess, error == ParsingDepthExceeded(4))
      },
      test("a redirect is refused rather than followed") {
        for {
          stub   <- uplinkStub(Answer("", status = Status.Found))
          loader <- loaderFor(stub.endpoint)
          exit   <- loader.exit
          error  <- acquisitionFailure(exit)
        } yield assertTrue(error.isInstanceOf[UnexpectedResponse])
      },
      test("a response slower than the acquisition timeout fails with TimedOut") {
        for {
          stub   <- uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl), delay = 3.seconds))
          loader <- loaderFor(configFor(stub.endpoint).withAcquisition(_.withTimeout(300.millis)))
          exit   <- loader.exit
          error  <- acquisitionFailure(exit)
        } yield assertTrue(error == TimedOut(300.millis))
      },
      test("a connection that cannot be made fails with RequestFailed") {
        for {
          loader <- loaderFor(unreachableEndpoint)
          exit   <- loader.exit
          error  <- acquisitionFailure(exit)
        } yield assertTrue(error.isInstanceOf[RequestFailed])
      }
    ),

    // -------------------------------------------------------------------------------------------
    // Task 5: endpoint failover
    // -------------------------------------------------------------------------------------------
    suite("failover")(
      test("moves to the next endpoint when the first cannot be reached") {
        for {
          second <- uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)))
          loader <- loaderFor(unreachableEndpoint, second.endpoint)
          exit   <- loader.exit
          calls  <- second.calls
        } yield assertTrue(exit.isSuccess, calls == 1)
      },
      test("moves to the next endpoint when the first answers a non-2xx status") {
        for {
          first  <- uplinkStub(Answer("upstream unavailable", status = Status.ServiceUnavailable))
          second <- uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)))
          loader <- loaderFor(first.endpoint, second.endpoint)
          exit   <- loader.exit
          calls  <- second.calls.zip(first.calls)
        } yield assertTrue(exit.isSuccess, calls == ((1, 1)))
      },
      test("moves to the next endpoint when the first hangs past the per-attempt timeout") {
        // The realistic uplink outage is a hang, not a refusal. A refusing endpoint fails fast and
        // passes with or without a correct timeout budget, so it cannot stand in for this case.
        for {
          first  <- uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl), delay = 10.seconds))
          second <- uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)))
          loader <-
            loaderFor(
              configFor(first.endpoint, second.endpoint).withAcquisition(_.withTimeout(300.millis))
            )
          exit   <- loader.exit
          calls  <- second.calls
        } yield assertTrue(exit.isSuccess, calls == 1)
      },
      test("does not fail over on an authoritative FetchError") {
        // Re-POSTing an AUTHENTICATION_FAILED would send the api key to a second host for an answer
        // the first host already gave definitively.
        for {
          first  <- uplinkStub(Answer(uplinkFetchError("AUTHENTICATION_FAILED", "invalid key")))
          second <- uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)))
          loader <- loaderFor(first.endpoint, second.endpoint)
          exit   <- loader.exit
          error  <- acquisitionFailure(exit)
          calls  <- second.calls
        } yield assertTrue(error == UplinkFetchFailed("AUTHENTICATION_FAILED"), calls == 0)
      },
      test("does not fail over on an unparseable supergraph") {
        for {
          first  <- uplinkStub(Answer(uplinkConfigResult("id-1", "type Query {")))
          second <- uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)))
          loader <- loaderFor(first.endpoint, second.endpoint)
          exit   <- loader.exit
          error  <- acquisitionFailure(exit)
          calls  <- second.calls
        } yield assertTrue(error.isInstanceOf[SchemaParsingFailed], calls == 0)
      },
      test("gives up once every endpoint has been tried, rather than retrying forever") {
        for {
          first  <- uplinkStub(Answer("", status = Status.ServiceUnavailable))
          second <- uplinkStub(Answer("", status = Status.ServiceUnavailable))
          loader <- loaderFor(first.endpoint, second.endpoint)
          exit   <- loader.exit
          error  <- acquisitionFailure(exit)
          calls  <- first.calls.zip(second.calls)
        } yield assertTrue(
          error.isInstanceOf[UnexpectedResponse],
          error.asInstanceOf[UnexpectedResponse].status == Status.ServiceUnavailable,
          // Each endpoint is tried once per load; a rotation that retried them would poll a whole
          // uplink outage several times over on every cycle.
          calls == ((1, 1))
        )
      },
      test("the failover budget is per load, so a later poll still reaches the second endpoint") {
        // A budget counted over the loader's lifetime rather than per load stops failing over after
        // a handful of polls, and a long-lived gateway is exactly where that matters.
        for {
          first  <- uplinkStub(Answer("", status = Status.BadGateway))
          second <- uplinkStub(
                      Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)),
                      Answer(uplinkConfigResult("id-2", changedSupergraphSdl)),
                      Answer(uplinkConfigResult("id-3", minimalSupergraphSdl))
                    )
          loader <- loaderFor(first.endpoint, second.endpoint)
          loads  <- ZIO.foreach(1 to 3)(_ => loader.exit)
          calls  <- first.calls.zip(second.calls)
        } yield assertTrue(loads.forall(_.isSuccess), calls == ((3, 3)))
      },
      test("the cursor is endpoint-independent: a failover reuses the id the other endpoint gave") {
        for {
          first  <-
            uplinkStub(Answer(uplinkConfigResult("id-1", minimalSupergraphSdl)), Answer("", status = Status.BadGateway))
          second <- uplinkStub(Answer(uplinkUnchanged("id-1")))
          loader <- loaderFor(first.endpoint, second.endpoint)
          _      <- loader
          exit   <- loader.exit
          cursor <- second.cursorOf(0)
          calls  <- second.calls
        } yield assertTrue(exit.isSuccess, cursor.contains("id-1"), calls == 1)
      }
    )
  ).provide(testServer, stubIds, httpClient) @@ TestAspect.sequential @@ TestAspect.withLiveClock
}
