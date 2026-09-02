package caliban.gateway

import caliban.Value.{ NullValue, StringValue }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.SupergraphAcquisitionError._
import caliban.gateway.internal.composition.SupergraphAcquisition
import caliban.parsing.adt.Document
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, writeToString }
import sttp.client4.httpclient.zio.{ HttpClientZioBackend, SttpClient }
import sttp.model.{ StatusCode, Uri }
import zio.Config.Secret
import zio._
import zio.http.{ Body, Header, Headers, Response, Server, Status }
import zio.test._

/**
 * Gates for the Apollo uplink source: the configuration it is described by, the error cases it can
 * fail with, the cursor and cache the loader carries across polls, and endpoint failover.
 *
 * The loader is stateful across `load` calls by design — `Gateway` builds it once per gateway
 * rather than once per reload cycle — so every test here drives one loader more than once.
 */
object SupergraphUplinkSpec extends ZIOSpecDefault {

  private val graphRef = "caliban-gateway@production"
  private val apiKey   = Secret("service:caliban-gateway:s3cr3t-uplink-key")

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

  private val changedSdl = supergraphSdl.replace("hello: String", "hello: String goodbye: String")

  /** Nests five deep inside the field definition, well past the response envelope's own three. */
  private val deeplyNestedSdl =
    supergraphSdl.replace("hello: String", "hello(filter: Filter = { a: { b: [1] } }): String")

  // ---------------------------------------------------------------------------------------------
  // Uplink protocol responses
  // ---------------------------------------------------------------------------------------------

  private def configResult(id: String, sdl: String): String = uplinkConfigResult(id, sdl)
  private def unchanged(id: String): String                 = uplinkUnchanged(id)
  private def fetchError(code: String, message: String)     = uplinkFetchError(code, message)

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

  private final case class Uplink(endpoint: Uri, requests: Ref[Vector[GraphQLRequest]], answers: Ref[List[Answer]]) {
    def calls: UIO[Int] = requests.get.map(_.size)

    /** The `ifAfterId` variable of the nth request, or `None` when it was absent or JSON null. */
    def cursorOf(index: Int): UIO[Option[String]] =
      requests.get.map(
        _.lift(index)
          .flatMap(_.variables)
          .flatMap(_.get("ifAfterId"))
          .collect { case StringValue(value) => value }
      )

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
                       answer <- remaining.modify {
                                   case only :: Nil  => only         -> (only :: Nil)
                                   case head :: rest => head         -> rest
                                   case Nil          => Answer("{}") -> Nil
                                 }
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

  private val fastAcquisition = RemoteGraphQLConfig.Acquisition.default.withTimeout(2.seconds)

  private def configFor(endpoints: Uri*): SupergraphUplinkConfig =
    SupergraphUplinkConfig(graphRef, apiKey).withEndpoints(endpoints: _*).withAcquisition(fastAcquisition)

  private def loaderFor(config: SupergraphUplinkConfig): ZIO[SttpClient, Nothing, SupergraphAcquisition.Loader] =
    ZIO.serviceWithZIO[SttpClient](client => SupergraphAcquisition.make(Supergraph.Source.Uplink(config), Some(client)))

  private def loaderFor(endpoints: Uri*): ZIO[SttpClient, Nothing, SupergraphAcquisition.Loader] =
    loaderFor(configFor(endpoints: _*))

  /** Fails with the acquisition error, or dies describing what happened instead. */
  private def failure[A](exit: Exit[SupergraphAcquisitionError, A]): UIO[SupergraphAcquisitionError] =
    exit match {
      case Exit.Failure(cause) =>
        ZIO
          .fromOption(cause.failureOption)
          .orDieWith(_ => new AssertionError(s"expected a typed failure, got: ${cause.prettyPrint}"))
      case Exit.Success(value) => ZIO.die(new AssertionError(s"expected a failure, got: $value"))
    }

  private def queryFields(document: Document): List[String] =
    document.objectTypeDefinitions.filter(_.name == "Query").flatMap(_.fields.map(_.name))

  private val backend: ZLayer[Any, Throwable, SttpClient] = ZLayer.scoped(HttpClientZioBackend.scoped())

  /** Every string a diagnostic must never contain: the api key, and any remote free text. */
  private def leaks(diagnostics: List[String], secrets: String*): List[String] =
    secrets.filter(secret => diagnostics.exists(_.contains(secret))).toList

  // =============================================================================================

  def spec = suite("SupergraphUplinkSpec")(
    // -------------------------------------------------------------------------------------------
    // Task 2 — Supergraph.Source.Uplink and SupergraphUplinkConfig
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
        val only = Uri.unsafeParse("https://uplink.internal/")
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
          .withAcquisition(RemoteGraphQLConfig.Acquisition.default.withMaxResponseBytes(0))
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
      test("an uplink source is refreshable, so Gateway.reloadable accepts it") {
        assertTrue(Supergraph.Source.Uplink(SupergraphUplinkConfig(graphRef, apiKey)).refreshable)
      },
      test("Supergraph.uplink describes an uplink source") {
        val fromParts  = Supergraph.uplink(graphRef, apiKey)
        val fromConfig = Supergraph.uplink(SupergraphUplinkConfig(graphRef, apiKey))
        assertTrue(
          fromParts.source == Supergraph.Source.Uplink(SupergraphUplinkConfig(graphRef, apiKey)),
          fromConfig.source == fromParts.source
        )
      }
    ),

    // -------------------------------------------------------------------------------------------
    // Task 3 — SupergraphAcquisitionError uplink cases
    // -------------------------------------------------------------------------------------------
    suite("error cases")(
      test("every uplink case renders a non-empty diagnostic") {
        val reasons                                  = List(
          InvalidUplinkResponse.MissingData,
          InvalidUplinkResponse.MissingRouterConfig,
          InvalidUplinkResponse.UnknownTypename,
          InvalidUplinkResponse.MissingSupergraphSdl,
          InvalidUplinkResponse.MissingId,
          InvalidUplinkResponse.DecodingFailed
        )
        val errors: List[SupergraphAcquisitionError] =
          UplinkFetchFailed("AUTHENTICATION_FAILED") :: reasons.map(InvalidUplinkResponse(_))

        assertTrue(
          errors.forall(_.diagnostics.nonEmpty),
          errors.forall(_.diagnostics.forall(_.trim.nonEmpty)),
          // Distinct reasons must not collapse to the same message, or a diagnostic tells you nothing.
          reasons.map(InvalidUplinkResponse(_).diagnostics).distinct.size == reasons.size
        )
      },
      test("UplinkFetchFailed surfaces the code, which is a fixed enum") {
        assertTrue(UplinkFetchFailed("UNKNOWN_REF").diagnostics.exists(_.contains("UNKNOWN_REF")))
      },
      test("getMessage renders the diagnostics") {
        assertTrue(UplinkFetchFailed("ACCESS_DENIED").getMessage.contains("ACCESS_DENIED"))
      },
      test("a caused error keeps its payload in the cause channel, never in the diagnostic") {
        val cause = new IllegalStateException("Bearer service:caliban-gateway:s3cr3t-uplink-key")
        val error = RequestFailed(cause)
        assertTrue(
          error.getCause == cause,
          leaks(error.diagnostics, apiKey.stringValue).isEmpty
        )
      }
    ),

    // -------------------------------------------------------------------------------------------
    // Task 4 — the uplink loader
    // -------------------------------------------------------------------------------------------
    suite("loader")(
      test("sends the SupergraphSdl operation with the api key and graph ref as variables") {
        for {
          stub   <- uplinkStub(Answer(configResult("id-1", supergraphSdl)))
          loader <- loaderFor(stub.endpoint)
          _      <- loader.load
          query  <- stub.queryOf(0)
          vars   <- stub.variablesOf(0)
        } yield assertTrue(
          query.contains("SupergraphSdl"),
          query.contains("routerConfig"),
          query.contains("RouterConfigResult"),
          query.contains("Unchanged"),
          query.contains("FetchError"),
          vars.get("apiKey").contains(StringValue(apiKey.stringValue)),
          vars.get("ref").contains(StringValue(graphRef))
        )
      },
      test("advances the cursor: the first poll sends no id, the second sends the one it received") {
        // The assertion that catches a regression of the loader-lifetime refactor. A loader rebuilt
        // per reload cycle would send no cursor on every poll, and this is the only place it shows.
        for {
          stub    <- uplinkStub(Answer(configResult("id-1", supergraphSdl)), Answer(configResult("id-2", changedSdl)))
          loader  <- loaderFor(stub.endpoint)
          first   <- loader.load
          second  <- loader.load
          cursor0 <- stub.cursorOf(0)
          cursor1 <- stub.cursorOf(1)
          _       <- loader.load
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
        for {
          stub   <- uplinkStub(Answer(configResult("id-1", supergraphSdl)), Answer(unchanged("id-1")))
          loader <- loaderFor(stub.endpoint)
          first  <- loader.load
          second <- loader.load
          calls  <- stub.calls
        } yield assertTrue(first == second, calls == 2)
      },
      test("a document that fetched but failed downstream is re-offered on the next poll") {
        // The reason the cache holds the last *fetched* document rather than the last *activated*
        // one: the caller compares against the active generation, so it must keep seeing the new
        // document until it manages to build it. Caching the activated one wedges the gateway.
        for {
          stub    <- uplinkStub(Answer(configResult("id-1", supergraphSdl)), Answer(unchanged("id-1")))
          loader  <- loaderFor(stub.endpoint)
          fetched <- loader.load
          again   <- loader.load
          third   <- loader.load
        } yield assertTrue(fetched == again, again == third)
      },
      test("Unchanged with nothing cached is a protocol violation, not an empty success") {
        for {
          stub    <- uplinkStub(Answer(unchanged("id-1")), Answer(configResult("id-2", supergraphSdl)))
          loader  <- loaderFor(stub.endpoint)
          exit    <- loader.load.exit
          error   <- failure(exit)
          _       <- loader.load
          cursor1 <- stub.cursorOf(1)
        } yield assertTrue(
          error == InvalidUplinkResponse(InvalidUplinkResponse.MissingSupergraphSdl),
          // Acknowledging an id we have no document for would have every later poll ask the same
          // unanswerable question, and the loader would never recover a full fetch.
          cursor1.isEmpty
        )
      },
      test("FetchError maps to the code, and never carries the remote message") {
        for {
          stub   <- uplinkStub(Answer(fetchError("AUTHENTICATION_FAILED", "invalid key service:xyz for graph")))
          loader <- loaderFor(stub.endpoint)
          exit   <- loader.load.exit
          error  <- failure(exit)
        } yield assertTrue(
          error == UplinkFetchFailed("AUTHENTICATION_FAILED"),
          error.diagnostics.exists(_.contains("AUTHENTICATION_FAILED")),
          leaks(error.diagnostics, "invalid key", "service:xyz").isEmpty
        )
      },
      test("unparseable supergraph sdl is reported as an invalid schema, and does not advance the cursor") {
        for {
          stub    <- uplinkStub(Answer(configResult("id-1", "type Query {")), Answer(configResult("id-2", supergraphSdl)))
          loader  <- loaderFor(stub.endpoint)
          exit    <- loader.load.exit
          error   <- failure(exit)
          _       <- loader.load
          cursor1 <- stub.cursorOf(1)
        } yield assertTrue(
          error.isInstanceOf[InvalidSupergraphSchema],
          // A document we could not parse must not be acknowledged, or it is never re-offered.
          cursor1.isEmpty
        )
      },
      test("graphql errors in the uplink response are an invalid response") {
        for {
          stub   <- uplinkStub(Answer(graphQLErrors))
          loader <- loaderFor(stub.endpoint)
          exit   <- loader.load.exit
          error  <- failure(exit)
        } yield assertTrue(
          error == InvalidUplinkResponse(InvalidUplinkResponse.MissingData),
          leaks(error.diagnostics, "denied").isEmpty
        )
      },
      test("an undecodable body is an invalid response rather than a defect") {
        for {
          stub   <- uplinkStub(Answer(invalidResponse))
          loader <- loaderFor(stub.endpoint)
          exit   <- loader.load.exit
          error  <- failure(exit)
        } yield assertTrue(error == InvalidUplinkResponse(InvalidUplinkResponse.DecodingFailed))
      },
      test("a body larger than maxResponseBytes is rejected before it is decoded") {
        for {
          stub   <- uplinkStub(Answer(configResult("id-1", supergraphSdl)))
          loader <- loaderFor(configFor(stub.endpoint).withAcquisition(fastAcquisition.withMaxResponseBytes(32)))
          exit   <- loader.load.exit
          error  <- failure(exit)
        } yield assertTrue(error == ResponseTooLarge(32))
      },
      test("a response envelope nested past maxParsingDepth is rejected before it is decoded") {
        // The uplink answers GraphQL JSON, so the envelope is bounded as JSON. `{"data":{"routerConfig":{`
        // is already three deep, and the supergraph itself is a string inside it.
        for {
          stub   <- uplinkStub(Answer(configResult("id-1", supergraphSdl)))
          loader <- loaderFor(configFor(stub.endpoint).withAcquisition(fastAcquisition.withMaxParsingDepth(2)))
          exit   <- loader.load.exit
          error  <- failure(exit)
        } yield assertTrue(error == ParsingDepthExceeded(2))
      },
      test("a supergraph nested past maxParsingDepth is rejected even when the envelope clears it") {
        // The same bound applies twice, to two different grammars. At four the envelope passes, so
        // only the supergraph's own nesting can be what rejects the second load.
        for {
          stub    <- uplinkStub(Answer(configResult("id-1", supergraphSdl)), Answer(configResult("id-2", deeplyNestedSdl)))
          loader  <- loaderFor(configFor(stub.endpoint).withAcquisition(fastAcquisition.withMaxParsingDepth(4)))
          shallow <- loader.load.exit
          exit    <- loader.load.exit
          error   <- failure(exit)
        } yield assertTrue(shallow.isSuccess, error == ParsingDepthExceeded(4))
      },
      test("a redirect is refused rather than followed") {
        for {
          stub   <- uplinkStub(Answer("", status = Status.Found))
          loader <- loaderFor(stub.endpoint)
          exit   <- loader.load.exit
          error  <- failure(exit)
        } yield assertTrue(error.isInstanceOf[UnexpectedResponse])
      },
      test("a response slower than the acquisition timeout fails with TimedOut") {
        for {
          stub   <- uplinkStub(Answer(configResult("id-1", supergraphSdl), delay = 3.seconds))
          loader <- loaderFor(configFor(stub.endpoint).withAcquisition(fastAcquisition.withTimeout(300.millis)))
          exit   <- loader.load.exit
          error  <- failure(exit)
        } yield assertTrue(error == TimedOut(300.millis))
      } @@ TestAspect.withLiveClock,
      test("a connection that cannot be made fails with RequestFailed") {
        for {
          loader <- loaderFor(unreachableEndpoint)
          exit   <- loader.load.exit
          error  <- failure(exit)
        } yield assertTrue(error.isInstanceOf[RequestFailed])
      }
    ),

    // -------------------------------------------------------------------------------------------
    // Task 5 — endpoint failover
    // -------------------------------------------------------------------------------------------
    suite("failover")(
      test("moves to the next endpoint when the first cannot be reached") {
        for {
          second <- uplinkStub(Answer(configResult("id-1", supergraphSdl)))
          loader <- loaderFor(unreachableEndpoint, second.endpoint)
          exit   <- loader.load.exit
          calls  <- second.calls
        } yield assertTrue(exit.isSuccess, calls == 1)
      },
      test("moves to the next endpoint when the first answers a non-2xx status") {
        for {
          first  <- uplinkStub(Answer("upstream unavailable", status = Status.ServiceUnavailable))
          second <- uplinkStub(Answer(configResult("id-1", supergraphSdl)))
          loader <- loaderFor(first.endpoint, second.endpoint)
          exit   <- loader.load.exit
          calls  <- second.calls.zip(first.calls)
        } yield assertTrue(exit.isSuccess, calls == ((1, 1)))
      },
      test("moves to the next endpoint when the first hangs past the per-attempt timeout") {
        // The realistic uplink outage is a hang, not a refusal. A refusing endpoint fails fast and
        // passes with or without a correct timeout budget, so it cannot stand in for this case.
        for {
          first  <- uplinkStub(Answer(configResult("id-1", supergraphSdl), delay = 10.seconds))
          second <- uplinkStub(Answer(configResult("id-1", supergraphSdl)))
          loader <-
            loaderFor(
              configFor(first.endpoint, second.endpoint).withAcquisition(fastAcquisition.withTimeout(300.millis))
            )
          exit   <- loader.load.exit
          calls  <- second.calls
        } yield assertTrue(exit.isSuccess, calls == 1)
      } @@ TestAspect.withLiveClock,
      test("does not fail over on an authoritative FetchError") {
        // Re-POSTing an AUTHENTICATION_FAILED would send the api key to a second host for an answer
        // the first host already gave definitively.
        for {
          first  <- uplinkStub(Answer(fetchError("AUTHENTICATION_FAILED", "invalid key")))
          second <- uplinkStub(Answer(configResult("id-1", supergraphSdl)))
          loader <- loaderFor(first.endpoint, second.endpoint)
          exit   <- loader.load.exit
          error  <- failure(exit)
          calls  <- second.calls
        } yield assertTrue(error == UplinkFetchFailed("AUTHENTICATION_FAILED"), calls == 0)
      },
      test("does not fail over on an unparseable supergraph") {
        for {
          first  <- uplinkStub(Answer(configResult("id-1", "type Query {")))
          second <- uplinkStub(Answer(configResult("id-1", supergraphSdl)))
          loader <- loaderFor(first.endpoint, second.endpoint)
          exit   <- loader.load.exit
          error  <- failure(exit)
          calls  <- second.calls
        } yield assertTrue(error.isInstanceOf[InvalidSupergraphSchema], calls == 0)
      },
      test("gives up once every endpoint has been tried, rather than retrying forever") {
        for {
          first  <- uplinkStub(Answer("", status = Status.ServiceUnavailable))
          second <- uplinkStub(Answer("", status = Status.ServiceUnavailable))
          loader <- loaderFor(first.endpoint, second.endpoint)
          exit   <- loader.load.exit
          error  <- failure(exit)
          calls  <- first.calls.zip(second.calls)
        } yield assertTrue(
          error.isInstanceOf[UnexpectedResponse],
          error.asInstanceOf[UnexpectedResponse].status == StatusCode.ServiceUnavailable,
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
                      Answer(configResult("id-1", supergraphSdl)),
                      Answer(configResult("id-2", changedSdl)),
                      Answer(configResult("id-3", supergraphSdl))
                    )
          loader <- loaderFor(first.endpoint, second.endpoint)
          loads  <- ZIO.foreach(1 to 3)(_ => loader.load.exit)
          calls  <- first.calls.zip(second.calls)
        } yield assertTrue(loads.forall(_.isSuccess), calls == ((3, 3)))
      },
      test("the cursor is endpoint-independent: a failover reuses the id the other endpoint gave") {
        for {
          first  <- uplinkStub(Answer(configResult("id-1", supergraphSdl)), Answer("", status = Status.BadGateway))
          second <- uplinkStub(Answer(unchanged("id-1")))
          loader <- loaderFor(first.endpoint, second.endpoint)
          _      <- loader.load
          exit   <- loader.load.exit
          cursor <- second.cursorOf(0)
          calls  <- second.calls
        } yield assertTrue(exit.isSuccess, cursor.contains("id-1"), calls == 1)
      }
    )
  ).provide(testServer, stubIds, backend) @@ TestAspect.sequential @@ TestAspect.withLiveClock
}
