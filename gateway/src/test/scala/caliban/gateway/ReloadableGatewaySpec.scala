package caliban.gateway

import caliban.{ graphQL, GraphQL, GraphQLRequest, GraphQLResponse, QuickAdapter, RootResolver }
import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.{ SchemaFingerprint, SubscriptionTermination }
import caliban.gateway.internal.execution.SubgraphExecutor
import caliban.parsing.Parser
import caliban.schema.{ GenericSchema, Schema }
import caliban.tools.IntrospectionClient
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import zio._
import zio.http.{ Body, Header, Request, Server, Status, URL }
import zio.stream.ZStream
import zio.test._

import java.time.{ Instant, LocalDateTime, OffsetDateTime }
import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit

object ReloadableGatewaySpec extends ZIOSpecDefault {
  private val schema                                          = "type Query { value: String } type Mutation { setValue: String }"
  private val changed                                         = "type Query { value: String added: String } type Mutation { setValue: String }"
  private val configureReload: GatewayConfig => GatewayConfig =
    _.withReloadPollInterval(1.second).withReloadJitter(0.0)

  private implicit final class TestGatewayOps[R](private val gateway: Gateway[R]) extends AnyVal {
    def reloadableForTest(implicit trace: Trace): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] =
      gateway.withConfig(configureReload).reloadable
  }

  private object UpdatedApi extends GenericSchema[Any] {
    import auto._
    final case class Query(value: String, added: String)
    implicit val querySchema: Schema[Any, Query] = gen
    val api                                      = graphQL(RootResolver(Query("old", "new")))
  }

  private def introspectionResponse(api: GraphQL[Any]): UIO[String] = {
    implicit val config: IntrospectionClient.Config = IntrospectionClient.Config.default
    ZIO.fromEither(api.interpreterEither).orDie.flatMap { interpreter =>
      interpreter
        .execute(IntrospectionClient.introspection.toGraphQL(dropNullInputValues = true).query)
        .map(writeToString(_))
    }
  }

  private def serviceResponse(sdl: String): String =
    writeToString(
      GraphQLResponse[Any](
        ObjectValue(List("_service" -> ObjectValue(List("sdl" -> StringValue(sdl))))),
        Nil
      )
    )

  private final case class Source(stub: Stub, response: Ref[String], checks: Ref[Int], beforeSchema: Ref[UIO[Unit]]) {
    def setSchema(sdl: String): UIO[Unit] = response.set(serviceResponse(sdl))
    def subgraph: Subgraph[Any]           = Subgraph.federation("remote", stub.endpoint)
  }

  private def source(sdl: String = schema): ZIO[Server with Ref[Int], Nothing, Source] =
    for {
      response <- Ref.make(serviceResponse(sdl))
      checks   <- Ref.make(0)
      before   <- Ref.make[UIO[Unit]](ZIO.unit)
      stub     <- stubByRequestZIO { request =>
                    if (request.query.exists(query => query.contains("_service") || query.contains("__schema")))
                      checks.update(_ + 1) *> before.get.flatten *> response.get
                    else ZIO.succeed("""{"data":{"value":"old","added":"new","setValue":"saved","setAdded":"saved"}}""")
                  }
    } yield Source(stub, response, checks, before)

  // The next poll timer is installed only after the current refresh and retirement finish.
  private def awaitPoll: UIO[Unit] =
    Clock.instant.flatMap(now => TestClock.sleeps.repeatUntil(_.contains(now.plusSeconds(1)))).unit

  private def poll(runtime: ReloadableGatewayInterpreter[_]): UIO[Option[String]] =
    TestClock.adjust(1.second) *> awaitPoll *> runtime.lastReloadFailure

  private def awaitSchema(runtime: GatewayInterpreter[Any], query: String): UIO[Unit] =
    ZTestLogger.logOutput.repeatUntil(_.exists(_.message() == "Gateway activated generation 2.")) *>
      runtime.check(query).orDie

  private def pauseNanoTime(
    entered: Promise[Nothing, Unit],
    release: Promise[Nothing, Unit],
    everyCall: Boolean = false
  ): UIO[Clock] =
    ZIO.clock.zipWith(Ref.make(false)) { (clock, paused) =>
      new Clock {
        def currentTime(unit: => TimeUnit)(implicit trace: Trace): UIO[Long]                     = clock.currentTime(unit)
        def currentTime(unit: => ChronoUnit)(implicit trace: Trace, d: DummyImplicit): UIO[Long] =
          clock.currentTime(unit)
        def currentDateTime(implicit trace: Trace): UIO[OffsetDateTime]                          = clock.currentDateTime
        def instant(implicit trace: Trace): UIO[Instant]                                         = clock.instant
        def javaClock(implicit trace: Trace): UIO[java.time.Clock]                               = clock.javaClock
        def localDateTime(implicit trace: Trace): UIO[LocalDateTime]                             = clock.localDateTime
        def scheduler(implicit trace: Trace): UIO[Scheduler]                                     = clock.scheduler
        def sleep(duration: => zio.Duration)(implicit trace: Trace): UIO[Unit]                   = clock.sleep(duration)
        def nanoTime(implicit trace: Trace): UIO[Long]                                           =
          paused
            .getAndSet(true)
            .flatMap(alreadyPaused =>
              (entered.succeed(()) *> release.await).unless(alreadyPaused && !everyCall) *> clock.nanoTime
            )
      }
    }

  def spec = suite("Reloadable gateway")(
    test("reload terminates active subscriptions promptly and unstarted streams use the new generation") {
      object Api extends GenericSchema[Any] {
        import auto._
        final case class Query(local: Boolean)
        final case class Subscription(event: ZStream[Any, Throwable, Int])
        def api(stream: ZStream[Any, Throwable, Int]) = graphQL(
          RootResolver(
            queryResolver = Some(Query(true)),
            mutationResolver = Option.empty[Unit],
            subscriptionResolver = Some(Subscription(stream))
          )
        )
      }
      for {
        remote        <- source()
        opened        <- Promise.make[Nothing, Unit]
        closed        <- Ref.make(0)
        stream         = ZStream.acquireReleaseWith(opened.succeed(()))(_ => closed.update(_ + 1)) *> (ZStream.succeed(
                           1
                         ) ++ ZStream.never)
        runtime       <- Gateway
                           .compose(remote.subgraph, Subgraph.local("local", Api.api(stream)))
                           .reloadableForTest
        request        = GraphQLRequest(query = Some("subscription { event }"))
        prepared      <- runtime.executeRequest(request)
        dormant        = runtime.executeStream(request)
        active        <- runtime.executeStream(request).runDrain.exit.forkScoped
        _             <- opened.await
        _             <- remote.setSchema(changed)
        _             <- poll(runtime)
        exit          <- active.join
        dormantEvents <- dormant.take(1).runCollect
        stale         <- SubgraphExecutor.responses(prepared).runDrain.exit
        count         <- closed.get
      } yield assertTrue(
        count == 2,
        exit.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.Reload),
        dormantEvents.size == 1,
        stale.causeOption.flatMap(_.failureOption).contains(SubscriptionTermination.Reload)
      )
    },
    test("rejects static-only gateways and invalid reload configuration") {
      for {
        remote  <- source()
        static  <- Gateway.compose(Subgraph.local("local", localGraph(ZIO.succeed("value")))).reloadable.exit
        results <- ZIO.foreach(
                     List[GatewayConfig => GatewayConfig](
                       _.withReloadPollInterval(Duration.Zero),
                       _.withReloadPollInterval(Duration.Infinity),
                       _.withReloadJitter(-0.1),
                       _.withReloadJitter(1.0),
                       _.withReloadJitter(Double.NaN)
                     )
                   )(configure => Gateway.compose(remote.subgraph).withConfig(configure).reloadable.exit)
        calls   <- remote.checks.get
      } yield assertTrue(static.isFailure, results.forall(_.isFailure), calls == 0)
    },
    test("fails startup without an initial usable generation") {
      for {
        remote <- source("not graphql")
        result <- Gateway.compose(remote.subgraph).reloadableForTest.exit
      } yield assertTrue(result.isFailure)
    },
    test("preserves the generation and warm cache when only formatting changes") {
      for {
        recorded         <- recordEvents
        (events, wrapper) = recorded
        remote           <- source()
        runtime          <- (Gateway.compose(remote.subgraph) @@ wrapper).reloadableForTest
        _                <- runtime.execute("{ value }")
        _                <- remote.setSchema("# comment\ntype Query {\n value: String\n}\ntype Mutation { setValue: String }")
        _                <- poll(runtime)
        _                <- runtime.execute("{ value }")
        observed         <- events.get
        calls            <- remote.checks.get
      } yield assertTrue(
        observed.count(_ == GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Miss)) == 1,
        observed.count(_ == GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Hit)) == 1,
        calls == 2
      )
    },
    test("builds from the checked snapshot and updates an existing HTTP adapter") {
      for {
        remote   <- source()
        runtime  <- Gateway.compose(remote.subgraph).reloadableForTest
        adapter   = QuickAdapter[Any](runtime)
        request   = Request
                      .post(URL.empty, Body.fromString("""{"query":"{ added }"}"""))
                      .addHeader(Header.Custom("Accept", "application/graphql-response+json"))
        before   <- adapter.handlers.api.runZIO(request)
        _        <- remote.setSchema(changed)
        _        <- poll(runtime)
        response <- adapter.handlers.api.runZIO(request)
        body     <- response.body.asString.orDie
        checks   <- remote.checks.get
        valid    <- runtime.check("{ added }").exit
        plan     <- runtime.explain("{ added }")
      } yield assertTrue(
        before.status == Status.BadRequest,
        response.status == Status.Ok,
        body.contains("new"),
        checks == 2,
        valid.isSuccess,
        plan.nonEmpty
      )
    },
    test("reload accepts policy annotations and continues serving unrelated schema updates") {
      val guarded = s"""extend schema @link(url: "https://specs.apollo.dev/federation/v2.9", import: ["@policy"])
                       |directive @link(url: String!, import: [String!]) repeatable on SCHEMA
                       |directive @policy(policies: [[String!]!]!) on FIELD_DEFINITION
                       |type Query { value: String @policy(policies: [["owner"]]) }
                       |""".stripMargin
      for {
        remote    <- source()
        other     <- source("type Query { another: String }")
        runtime   <-
          Gateway.compose(remote.subgraph, Subgraph.federation("other", other.stub.endpoint)).reloadableForTest
        before    <- runtime.execute("{ value }")
        _         <- remote.setSchema(guarded)
        _         <- other.setSchema("type Query { another: String added: String }")
        failure   <- poll(runtime)
        denied    <- runtime.execute("{ value }")
        added     <- runtime.execute("{ added }")
        _         <- remote.setSchema(schema)
        recovered <- poll(runtime)
        after     <- runtime.execute("{ value }")
      } yield assertTrue(
        before.errors.isEmpty,
        failure.isEmpty,
        denied.errors.exists(_.msg.contains("unsupported @policy")),
        added.errors.isEmpty,
        field(added.data, "added").contains(StringValue("new")),
        recovered.isEmpty,
        after.errors.isEmpty
      )
    },
    test("preserves the generation and cache when definitions and fields are reordered") {
      for {
        recorded         <- recordEvents
        (events, wrapper) = recorded
        remote           <- source(changed)
        runtime          <- (Gateway.compose(remote.subgraph) @@ wrapper).reloadableForTest
        _                <- runtime.execute("{ value added }")
        _                <- remote.setSchema("type Mutation { setValue: String } type Query { added: String value: String }")
        _                <- poll(runtime)
        _                <- runtime.execute("{ value added }")
        observed         <- events.get
      } yield assertTrue(
        observed.count(_ == GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Miss)) == 1,
        observed.count(_ == GatewayWrapper.Event.CacheAccess(GatewayWrapper.CacheResult.Hit)) == 1
      )
    },
    test("refreshes ordinary introspection schemas") {
      for {
        remote  <- source()
        initial <- introspectionResponse(localGraph(ZIO.succeed("old")))
        updated <- introspectionResponse(UpdatedApi.api)
        _       <- remote.response.set(initial)
        runtime <- Gateway.compose(Subgraph.graphql("ordinary", remote.stub.endpoint)).reloadableForTest
        _       <- remote.response.set(updated)
        _       <- poll(runtime)
        result  <- runtime.execute("{ added }")
        checks  <- remote.checks.get
      } yield assertTrue(
        result.errors.isEmpty,
        checks == 2,
        field(result.data, "added").contains(StringValue("new"))
      )
    },
    test("does not overlap slow polling cycles or block request execution") {
      for {
        remote  <- source()
        started <- Promise.make[Nothing, Unit]
        release <- Promise.make[Nothing, Unit]
        runtime <- Gateway.compose(remote.subgraph).reloadableForTest
        _       <- remote.beforeSchema.set(started.succeed(()).unit *> release.await)
        _       <- TestClock.adjust(1.second)
        _       <- started.await
        _       <- TestClock.adjust(3.seconds)
        checks  <- remote.checks.get
        result  <- runtime.execute("{ value }")
        _       <- release.succeed(())
        _       <- awaitPoll
      } yield assertTrue(checks == 2, result.errors.isEmpty)
    },
    test("uses acquisition timeouts and recovers without replacing an unchanged generation") {
      for {
        remote    <- source()
        started   <- Promise.make[Nothing, Unit]
        release   <- Promise.make[Nothing, Unit]
        subgraph   = Subgraph.federation(
                       "remote",
                       remote.stub.endpoint,
                       RemoteGraphQLConfig.default.withAcquisition(_.withTimeout(1.second))
                     )
        runtime   <- Gateway.compose(subgraph).reloadableForTest
        _         <- remote.beforeSchema.set(started.succeed(()).unit *> release.await)
        _         <- TestClock.adjust(1.second)
        _         <- started.await
        _         <- TestClock.adjust(1.second)
        failed    <- runtime.lastReloadFailure.repeatUntil(_.nonEmpty)
        _         <- awaitPoll
        result    <- runtime.execute("{ value }")
        _         <- release.succeed(())
        _         <- TestClock.adjust(1.second)
        // Acquisition and polling use the same duration here; await recovery itself.
        recovered <- runtime.lastReloadFailure.repeatUntil(_.isEmpty)
      } yield assertTrue(
        failed.contains("Unable to load subgraph schemas."),
        result.errors.isEmpty,
        recovered.isEmpty
      )
    },
    test("keeps pinned and local schemas fixed while acquired schemas change") {
      for {
        remote  <- source()
        pinned  <- stub("""{"data":{"pinned":"fixed"}}""")
        runtime <- Gateway
                     .compose(
                       remote.subgraph,
                       Subgraph.graphql("pinned", pinned.endpoint, "type Query { pinned: String }"),
                       Subgraph
                         .local("local", localGraph(ZIO.succeed("local")))
                         .transform(SchemaTransformation.renameField("Query", "value", "local"))
                     )
                     .reloadableForTest
        _       <- remote.setSchema(changed)
        _       <- poll(runtime)
        result  <- runtime.execute("{ added pinned local }")
        calls   <- pinned.requests.get
      } yield assertTrue(
        result.errors.isEmpty,
        calls.size == 1,
        field(result.data, "local").contains(StringValue("local"))
      )
    },
    test("retains the old generation on acquisition failure and recovers on the next cycle") {
      for {
        remote    <- source()
        runtime   <- Gateway.compose(remote.subgraph).reloadableForTest
        _         <- remote.response.set("""{"errors":[{"message":"secret response body"}]}""")
        failed    <- poll(runtime)
        result    <- runtime.execute("{ value }")
        _         <- remote.setSchema(changed)
        recovered <- poll(runtime)
      } yield assertTrue(
        failed.contains("Unable to load subgraph schemas."),
        !failed.toString.contains("secret"),
        result.errors.isEmpty,
        recovered.isEmpty
      )
    },
    test("requires a complete collection and never combines fresh and cached schemas") {
      for {
        remote    <- source()
        other     <- source("type Query { other: String }")
        runtime   <-
          Gateway.compose(remote.subgraph, Subgraph.federation("other", other.stub.endpoint)).reloadableForTest
        _         <- remote.setSchema(changed)
        _         <- other.response.set(invalidResponse)
        failed    <- poll(runtime)
        missing   <- runtime.check("{ added }").exit
        _         <- other.setSchema("type Query { other: String }")
        recovered <- poll(runtime)
        checks    <- remote.checks.get
      } yield assertTrue(failed.nonEmpty, missing.isFailure, recovered.isEmpty, checks == 3)
    },
    test("retries identical rejected candidates without replacing the active interpreter") {
      for {
        remote    <- source()
        runtime   <- Gateway.compose(remote.subgraph).reloadableForTest
        _         <- remote.setSchema("type Query { value: MissingType }")
        first     <- poll(runtime)
        second    <- poll(runtime)
        result    <- runtime.execute("{ value }")
        _         <- remote.setSchema(changed)
        recovered <- poll(runtime)
        checks    <- remote.checks.get
        added     <- runtime.check("{ added }").exit
      } yield assertTrue(
        first.nonEmpty,
        second == first,
        result.errors.isEmpty,
        checks == 4,
        recovered.isEmpty,
        added.isSuccess
      )
    },
    test("keeps serving when individually valid schemas fail composition") {
      for {
        remote  <- source()
        pinned  <- stub("""{"data":{"other":"fixed"}}""")
        runtime <-
          Gateway
            .compose(remote.subgraph, Subgraph.graphql("pinned", pinned.endpoint, "type Query { other: String }"))
            .reloadableForTest
        _       <- remote.setSchema("type Query { value: String other: Int }")
        failed  <- poll(runtime)
        result  <- runtime.execute("{ value other }")
      } yield assertTrue(
        failed.contains("Subgraph schemas could not be composed."),
        result.errors.isEmpty
      )
    },
    test("pins admitted mutations across replacement and keeps limits per interpreter") {
      for {
        remote  <- source()
        started <- Promise.make[Nothing, Unit]
        release <- Promise.make[Nothing, Unit]
        resolver = OperationResolver[Any](request =>
                     (if (request.operationName.contains("Before")) started.succeed(()).unit *> release.await
                      else ZIO.unit).as(request.query.getOrElse(""))
                   )
        runtime <- Gateway
                     .compose(remote.subgraph)
                     .withOperationResolver(resolver)
                     .withConfig(_.withMaxConcurrentRequests(1).withRequestTimeout(1.hour).withDrainTimeout(10.seconds))
                     .reloadableForTest
        _       <- ZIO.addFinalizer(release.succeed(()).unit)
        before  <- runtime.execute("mutation Before { setValue }", Some("Before")).fork
        _       <- started.await
        _       <- remote.setSchema(changed.replace("setValue", "setAdded"))
        _       <- TestClock.adjust(1.second)
        _       <- awaitSchema(runtime, "mutation { setAdded }")
        after   <- runtime.execute("mutation After { setAdded }", Some("After"))
        _       <- release.succeed(())
        old     <- before.join
        _       <- awaitPoll
        calls   <- remote.stub.requests.get
      } yield assertTrue(
        old.errors.isEmpty,
        after.errors.isEmpty,
        calls.count(_.query.exists(_.contains("mutation"))) == 2
      )
    },
    test("a paused reservation neither blocks publication nor replays mutation work") {
      for {
        remote      <- source()
        runtime     <- Gateway.compose(remote.subgraph).reloadableForTest
        entered     <- Promise.make[Nothing, Unit]
        release     <- Promise.make[Nothing, Unit]
        clock       <- pauseNanoTime(entered, release)
        _           <- ZIO.addFinalizer(release.succeed(()).unit)
        pending     <- runtime.execute("mutation { setAdded }").withClock(clock).fork
        _           <- entered.await
        independent <- runtime.execute("{ value }")
        _           <- remote.setSchema(changed.replace("setValue", "setAdded"))
        _           <- poll(runtime)
        _           <- release.succeed(())
        response    <- pending.join
        requests    <- remote.stub.requests.get
      } yield assertTrue(
        independent.errors.isEmpty,
        response.errors.isEmpty,
        requests.count(_.query.exists(_.contains("mutation"))) == 1
      )
    },
    test("releases a racing reservation after admission closes but before draining starts") {
      for {
        remote         <- source()
        scope          <- Scope.make
        runtime        <- scope.extend(Gateway.compose(remote.subgraph).reloadableForTest)
        requestEntered <- Promise.make[Nothing, Unit]
        requestRelease <- Promise.make[Nothing, Unit]
        closeEntered   <- Promise.make[Nothing, Unit]
        closeRelease   <- Promise.make[Nothing, Unit]
        requestClock   <- pauseNanoTime(requestEntered, requestRelease)
        // Pause finite-request shutdown before it starts draining.
        closeClock     <- pauseNanoTime(closeEntered, closeRelease, everyCall = true)
        _              <-
          ZIO.addFinalizer(requestRelease.succeed(()).unit *> closeRelease.succeed(()).unit *> scope.close(Exit.unit))
        pending        <- runtime.execute("mutation { setValue }").withClock(requestClock).fork
        _              <- requestEntered.await
        closing        <- scope.close(Exit.unit).withClock(closeClock).fork
        _              <- closeEntered.await
        _              <- requestRelease.succeed(())
        response       <- pending.join
        requests       <- remote.stub.requests.get
        _              <- closeRelease.succeed(())
        _              <- closing.join
      } yield assertTrue(
        response.errors.exists(_.msg == "Gateway is shutting down."),
        !requests.exists(_.query.exists(_.contains("mutation")))
      )
    },
    test("bounds generations and pauses polling behind uninterruptible retirement") {
      for {
        remote      <- source("type Query { remote: String }")
        started     <- Promise.make[Nothing, Unit]
        release     <- Promise.make[Nothing, Unit]
        runtime     <-
          Gateway
            .compose(
              remote.subgraph,
              Subgraph
                .local("local", localGraph(started.succeed(()).unit *> ZIO.uninterruptible(release.await).as("done")))
            )
            .withConfig(
              _.withMaxConcurrentRequests(Int.MaxValue).withRequestTimeout(1.hour).withDrainTimeout(2.seconds)
            )
            .reloadableForTest
        _           <- ZIO.addFinalizer(release.succeed(()).unit)
        old         <- runtime.execute("{ value }").fork
        _           <- started.await
        _           <- remote.setSchema("type Query { remote: String added: String }")
        _           <- TestClock.adjust(1.second)
        _           <- awaitSchema(runtime, "{ added }")
        _           <- remote.setSchema("type Query { remote: String added: String newest: String }")
        _           <- TestClock.adjust(10.seconds)
        unavailable <- runtime.check("{ newest }").exit
        checks      <- remote.checks.get
        result      <- runtime.execute("{ added }")
        pending     <- old.poll
        _           <- release.succeed(())
        exit        <- old.await
        _           <- awaitPoll
        _           <- poll(runtime)
        newest      <- runtime.check("{ newest }").exit
      } yield assertTrue(
        unavailable.isFailure,
        newest.isSuccess,
        checks == 2,
        pending.isEmpty,
        exit.isInterrupted,
        result.errors.isEmpty
      )
    },
    test("shutdown cancels acquisition and prevents late publication") {
      for {
        remote   <- source()
        scope    <- Scope.make
        runtime  <- scope.extend(Gateway.compose(remote.subgraph).reloadableForTest)
        started  <- Promise.make[Nothing, Unit]
        release  <- Promise.make[Nothing, Unit]
        _        <- remote.setSchema(changed)
        _        <- remote.beforeSchema.set(started.succeed(()).unit *> release.await)
        _        <- TestClock.adjust(1.second)
        _        <- started.await
        _        <- scope.close(Exit.unit)
        _        <- release.succeed(())
        _        <- TestClock.adjust(10.seconds)
        rejected <- runtime.execute("{ value }")
        check    <- runtime.check("{ value }").exit
        explain  <- runtime.explain("{ value }").exit
        calls    <- remote.checks.get
      } yield assertTrue(
        calls == 2,
        rejected.errors.exists(_.msg == "Gateway is shutting down."),
        check.isFailure,
        explain.isFailure
      )
    },
    test("shutdown drains the active generation while an old generation remains stuck") {
      for {
        remote            <- source("type Query { remote: String }")
        scope             <- Scope.make
        firstStarted      <- Promise.make[Nothing, Unit]
        secondStarted     <- Promise.make[Nothing, Unit]
        secondInterrupted <- Promise.make[Nothing, Unit]
        release           <- Promise.make[Nothing, Unit]
        calls             <- Ref.make(0)
        effect             = calls.getAndUpdate(_ + 1).flatMap {
                               case 0 => firstStarted.succeed(()).unit *> ZIO.uninterruptible(release.await).as("old")
                               case _ =>
                                 (secondStarted.succeed(()).unit *> ZIO.never)
                                   .onInterrupt(secondInterrupted.succeed(()).unit)
                             }
        runtime           <- scope.extend(
                               Gateway
                                 .compose(remote.subgraph, Subgraph.local("local", localGraph(effect)))
                                 .withConfig(_.withRequestTimeout(1.hour).withDrainTimeout(2.seconds))
                                 .reloadableForTest
                             )
        _                 <- ZIO.addFinalizer(release.succeed(()).unit *> scope.close(Exit.unit))
        first             <- runtime.execute("{ value }").fork
        _                 <- firstStarted.await
        _                 <- remote.setSchema("type Query { remote: String added: String }")
        _                 <- TestClock.adjust(1.second)
        _                 <- awaitSchema(runtime, "{ added }")
        _                 <- TestClock.adjust(Duration.Zero)
        _                 <- TestClock.adjust(1.second)
        second            <- runtime.execute("{ value }").fork
        _                 <- secondStarted.await
        closing           <- scope.close(Exit.unit).fork
        _                 <- TestClock.adjust(Duration.Zero)
        _                 <- TestClock.adjust(1.second)
        activePending     <- second.poll
        _                 <- TestClock.adjust(1.second)
        _                 <- secondInterrupted.await
        oldPending        <- first.poll
        closePending      <- closing.poll
        _                 <- release.succeed(())
        firstExit         <- first.await
        secondExit        <- second.await
        _                 <- closing.join
      } yield assertTrue(
        oldPending.isEmpty,
        closePending.isEmpty,
        activePending.isEmpty,
        firstExit.isInterrupted,
        secondExit.isInterrupted
      )
    },
    test("fingerprints ignore locations but retain directive arguments and descriptions") {
      def fingerprint(sdl: String): String = SchemaFingerprint(Parser.parseQuery(sdl).toOption.get)
      val original                         = "type Query { value: String @deprecated(reason: \"old\") }"
      assertTrue(
        fingerprint(original) == fingerprint("# comment\n" + original.replace("@deprecated", "\n @deprecated")),
        fingerprint(original) != fingerprint(original.replace("old", "new")),
        fingerprint("type Query { value: String }") != fingerprint("\" \" type Query { value: String }")
      )
    },
    test("fingerprints canonicalize schema declarations but retain ordered values and directives") {
      def fingerprint(sdl: String): String = SchemaFingerprint(Parser.parseQuery(sdl).toOption.get)
      val equivalent                       = List(
        "type Query { x(a: Int, b: Int): String y: Int }"                          -> "type Query { y: Int x(b: Int, a: Int): String }",
        "input Input { a: Int b: Int } enum E { A B }"                             -> "enum E { B A } input Input { b: Int a: Int }",
        "union U = A | B"                                                          -> "union U = B | A",
        "type T implements A & B { x: Int y: Int }"                                -> "type T implements B & A { y: Int x: Int }",
        "directive @d(a: Int, b: Int) on OBJECT | FIELD_DEFINITION"                ->
          "directive @d(b: Int, a: Int) on FIELD_DEFINITION | OBJECT",
        "extend type T { x: Int } extend type T { y: Int } extend union U = A | B" ->
          "extend union U = B | A extend type T { y: Int } extend type T { x: Int }",
        "type Query { x(a: Input = {a: 1, b: 2}): Int }"                           -> "type Query { x(a: Input = {b: 2, a: 1}): Int }"
      )
      assertTrue(
        equivalent.forall { case (first, second) => fingerprint(first) == fingerprint(second) },
        fingerprint("type Query { x(a: [Int] = [1, 2]): Int }") != fingerprint(
          "type Query { x(a: [Int] = [2, 1]): Int }"
        ),
        fingerprint("type Query @d(a: 1) @d(a: 2) { x: Int }") != fingerprint("type Query @d(a: 2) @d(a: 1) { x: Int }")
      )
    },
    test("fingerprints preserve directive order across extensions of the same target") {
      def fingerprint(sdl: String): String = SchemaFingerprint(Parser.parseQuery(sdl).toOption.get)
      val typeFirst                        = "extend type Query @d(a: 1)"
      val typeSecond                       = "extend type Query @d(a: 2)"
      val schemaFirst                      = "extend schema @d(a: 1)"
      val schemaSecond                     = "extend schema @d(a: 2)"
      val other                            = "extend type Other @d(a: 3)"
      assertTrue(
        fingerprint(s"$typeFirst $typeSecond") != fingerprint(s"$typeSecond $typeFirst"),
        fingerprint(s"$schemaFirst $schemaSecond") != fingerprint(s"$schemaSecond $schemaFirst"),
        fingerprint(s"$typeFirst $other $typeSecond") == fingerprint(s"$other $typeFirst $typeSecond"),
        fingerprint(s"$typeFirst $schemaFirst $typeSecond") == fingerprint(s"$schemaFirst $typeFirst $typeSecond")
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential @@ TestAspect.timeout(30.seconds)
}
