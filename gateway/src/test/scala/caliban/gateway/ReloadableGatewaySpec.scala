package caliban.gateway

import caliban.{ graphQL, GraphQL, GraphQLRequest, GraphQLResponse, QuickAdapter, RootResolver }
import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.ReloadableGatewayInterpreter.{ FailureStage, Phase }
import caliban.gateway.internal.SchemaFingerprint
import caliban.parsing.Parser
import caliban.schema.{ GenericSchema, Schema }
import caliban.tools.IntrospectionClient
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import zio._
import zio.http.{ Body, Header, Request, Server, Status, URL }
import zio.test._

import java.time.{ Instant, LocalDateTime, OffsetDateTime }
import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit

object ReloadableGatewaySpec extends ZIOSpecDefault {
  private val schema  = "type Query { value: String } type Mutation { setValue: String }"
  private val changed = "type Query { value: String added: String } type Mutation { setValue: String }"
  private val config  = GatewayReloadConfig.default.withPollInterval(1.second).withJitter(0.0)

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

  private def awaitStatus(runtime: ReloadableGatewayInterpreter[_])(
    predicate: ReloadableGatewayInterpreter.Status => Boolean
  ): UIO[ReloadableGatewayInterpreter.Status] =
    (ZIO.yieldNow *> runtime.reloadStatus).repeatUntil(predicate)

  private def poll(runtime: ReloadableGatewayInterpreter[_]): UIO[ReloadableGatewayInterpreter.Status] =
    for {
      before <- runtime.reloadStatus
      _      <- TestClock.adjust(1.second)
      after  <- awaitStatus(runtime)(s => s.phase == Phase.Idle && s.lastAttemptAt != before.lastAttemptAt)
    } yield after

  private def pauseFirstNanoTime(entered: Promise[Nothing, Unit], release: Promise[Nothing, Unit]): UIO[Clock] =
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
            .flatMap(alreadyPaused => (entered.succeed(()) *> release.await).unless(alreadyPaused) *> clock.nanoTime)
      }
    }

  def spec = suite("Reloadable gateway")(
    test("rejects static-only gateways and invalid reload configuration") {
      for {
        remote  <- source()
        static  <- Gateway.compose(Subgraph.local("local", localGraph(ZIO.succeed("value")))).reloadable().exit
        results <- ZIO.foreach(
                     List(
                       config.withPollInterval(Duration.Zero),
                       config.withPollInterval(Duration.Infinity),
                       config.withJitter(-0.1),
                       config.withJitter(1.0),
                       config.withJitter(Double.NaN)
                     )
                   )(value => Gateway.compose(remote.subgraph).reloadable(value).exit)
        calls   <- remote.checks.get
      } yield assertTrue(static.isFailure, results.forall(_.isFailure), calls == 0)
    },
    test("fails startup without an initial usable generation") {
      for {
        remote <- source("not graphql")
        result <- Gateway.compose(remote.subgraph).reloadable(config).exit
      } yield assertTrue(result.isFailure)
    },
    test("preserves the generation and warm cache when only formatting changes") {
      for {
        remote  <- source()
        runtime <- Gateway.compose(remote.subgraph).reloadable(config)
        _       <- runtime.execute("{ value }")
        before  <- runtime.reloadStatus
        _       <- remote.setSchema("# comment\ntype Query {\n value: String\n}\ntype Mutation { setValue: String }")
        after   <- poll(runtime)
        _       <- runtime.execute("{ value }")
        cached  <- runtime.status
        calls   <- remote.checks.get
      } yield assertTrue(
        after.active.id == before.active.id,
        after.active.activatedAt == before.active.activatedAt,
        after.lastSuccessfulCheckAt.isAfter(before.lastSuccessfulCheckAt),
        after.active.status.operationCache == before.active.status.operationCache,
        cached.operationCache.hits == before.active.status.operationCache.hits + 1L,
        calls == 2
      )
    },
    test("builds from the checked snapshot and updates an existing HTTP adapter") {
      for {
        remote   <- source()
        runtime  <- Gateway.compose(remote.subgraph).reloadable(config)
        adapter   = QuickAdapter[Any](runtime)
        request   = Request
                      .post(URL.empty, Body.fromString("""{"query":"{ added }"}"""))
                      .addHeader(Header.Custom("Accept", "application/graphql-response+json"))
        before   <- adapter.handlers.api.runZIO(request)
        _        <- remote.setSchema(changed)
        after    <- poll(runtime)
        response <- adapter.handlers.api.runZIO(request)
        body     <- response.body.asString.orDie
        checks   <- remote.checks.get
        valid    <- runtime.check("{ added }").exit
        plan     <- runtime.explain("{ added }")
      } yield assertTrue(
        before.status == Status.BadRequest,
        response.status == Status.Ok,
        body.contains("new"),
        after.active.id == 2L,
        after.retiring.isEmpty,
        checks == 2,
        valid.isSuccess,
        plan.nonEmpty
      )
    },
    test("preserves the generation and cache when definitions and fields are reordered") {
      for {
        remote  <- source(changed)
        runtime <- Gateway.compose(remote.subgraph).reloadable(config)
        _       <- runtime.execute("{ value added }")
        before  <- runtime.reloadStatus
        _       <- remote.setSchema("type Mutation { setValue: String } type Query { added: String value: String }")
        after   <- poll(runtime)
        _       <- runtime.execute("{ value added }")
        cached  <- runtime.status
      } yield assertTrue(
        after.active.id == before.active.id,
        cached.operationCache.hits == before.active.status.operationCache.hits + 1L
      )
    },
    test("refreshes ordinary introspection schemas") {
      for {
        remote  <- source()
        initial <- introspectionResponse(localGraph(ZIO.succeed("old")))
        updated <- introspectionResponse(UpdatedApi.api)
        _       <- remote.response.set(initial)
        runtime <- Gateway.compose(Subgraph.graphql("ordinary", remote.stub.endpoint)).reloadable(config)
        _       <- remote.response.set(updated)
        after   <- poll(runtime)
        result  <- runtime.execute("{ added }")
        checks  <- remote.checks.get
      } yield assertTrue(
        after.active.id == 2L,
        result.errors.isEmpty,
        checks == 2,
        field(result.data, "added").contains(StringValue("new"))
      )
    },
    test("does not overlap slow polling cycles or block request execution") {
      for {
        remote   <- source()
        started  <- Promise.make[Nothing, Unit]
        release  <- Promise.make[Nothing, Unit]
        runtime  <- Gateway.compose(remote.subgraph).reloadable(config)
        _        <- remote.beforeSchema.set(started.succeed(()).unit *> release.await)
        _        <- TestClock.adjust(1.second)
        _        <- started.await
        _        <- TestClock.adjust(3.seconds)
        checking <- runtime.reloadStatus
        checks   <- remote.checks.get
        result   <- runtime.execute("{ value }")
        _        <- release.succeed(())
        _        <- awaitStatus(runtime)(_.phase == Phase.Idle)
      } yield assertTrue(checking.phase == Phase.Checking, checks == 2, result.errors.isEmpty)
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
        runtime   <- Gateway.compose(subgraph).reloadable(config)
        _         <- remote.beforeSchema.set(started.succeed(()).unit *> release.await)
        _         <- TestClock.adjust(1.second)
        _         <- started.await
        _         <- TestClock.adjust(1.second)
        failed    <- awaitStatus(runtime)(_.lastFailure.nonEmpty)
        result    <- runtime.execute("{ value }")
        _         <- release.succeed(())
        recovered <- poll(runtime)
      } yield assertTrue(
        failed.lastFailure.exists(_.stage == FailureStage.Acquisition),
        result.errors.isEmpty,
        recovered.active.id == 1L,
        recovered.lastFailure.isEmpty
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
                     .reloadable(config)
        _       <- remote.setSchema(changed)
        after   <- poll(runtime)
        result  <- runtime.execute("{ added pinned local }")
        calls   <- pinned.requests.get
      } yield assertTrue(
        after.active.id == 2L,
        result.errors.isEmpty,
        calls.size == 1,
        field(result.data, "local").contains(StringValue("local"))
      )
    },
    test("retains the old generation on acquisition failure and recovers on the next cycle") {
      for {
        remote    <- source()
        runtime   <- Gateway.compose(remote.subgraph).reloadable(config)
        _         <- remote.response.set("""{"errors":[{"message":"secret response body"}]}""")
        failed    <- poll(runtime)
        result    <- runtime.execute("{ value }")
        _         <- remote.setSchema(changed)
        recovered <- poll(runtime)
      } yield assertTrue(
        failed.active.id == 1L,
        failed.lastFailure.exists(_.stage == FailureStage.Acquisition),
        !failed.lastFailure.toString.contains("secret"),
        result.errors.isEmpty,
        recovered.active.id == 2L,
        recovered.lastFailure.isEmpty
      )
    },
    test("requires a complete collection and never combines fresh and cached schemas") {
      for {
        remote    <- source()
        other     <- source("type Query { other: String }")
        runtime   <-
          Gateway.compose(remote.subgraph, Subgraph.federation("other", other.stub.endpoint)).reloadable(config)
        _         <- remote.setSchema(changed)
        _         <- other.response.set(invalidResponse)
        failed    <- poll(runtime)
        missing   <- runtime.check("{ added }").exit
        _         <- other.setSchema("type Query { other: String }")
        recovered <- poll(runtime)
        checks    <- remote.checks.get
      } yield assertTrue(failed.active.id == 1L, missing.isFailure, recovered.active.id == 2L, checks == 3)
    },
    test("retries identical rejected candidates without replacing the active interpreter") {
      for {
        remote    <- source()
        runtime   <- Gateway.compose(remote.subgraph).reloadable(config)
        _         <- remote.setSchema("type Query { value: MissingType }")
        first     <- poll(runtime)
        second    <- poll(runtime)
        result    <- runtime.execute("{ value }")
        _         <- remote.setSchema(changed)
        recovered <- poll(runtime)
      } yield assertTrue(
        first.active.id == 1L,
        second.active.id == 1L,
        first.lastFailure.exists(_.stage == FailureStage.Construction),
        second.lastFailure.exists(error => first.lastFailure.exists(_.at.isBefore(error.at))),
        result.errors.isEmpty,
        recovered.active.id == 2L
      )
    },
    test("keeps serving when individually valid schemas fail composition") {
      for {
        remote  <- source()
        pinned  <- stub("""{"data":{"other":"fixed"}}""")
        runtime <-
          Gateway
            .compose(remote.subgraph, Subgraph.graphql("pinned", pinned.endpoint, "type Query { other: String }"))
            .reloadable(config)
        before  <- runtime.reloadStatus
        _       <- remote.setSchema("type Query { value: String other: Int }")
        failed  <- poll(runtime)
        result  <- runtime.execute("{ value other }")
      } yield assertTrue(
        failed.active.id == 1L,
        failed.lastFailure.exists(_.reason == "Subgraph schemas could not be composed."),
        failed.lastSuccessfulCheckAt == before.lastSuccessfulCheckAt,
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
                     .reloadable(config)
        _       <- ZIO.addFinalizer(release.succeed(()).unit)
        before  <- runtime.execute("mutation Before { setValue }", Some("Before")).fork
        _       <- started.await
        _       <- remote.setSchema(changed.replace("setValue", "setAdded"))
        _       <- TestClock.adjust(1.second)
        during  <- awaitStatus(runtime)(_.active.id == 2L)
        after   <- runtime.execute("mutation After { setAdded }", Some("After"))
        total   <- runtime.status
        _       <- release.succeed(())
        old     <- before.join
        _       <- awaitStatus(runtime)(_.phase == Phase.Idle)
        calls   <- remote.stub.requests.get
      } yield assertTrue(
        during.retiring.exists(_.status.requests.active == 1),
        during.active.status.requests.limit == 1,
        total.requests.limit == 2,
        old.errors.isEmpty,
        after.errors.isEmpty,
        calls.count(_.query.exists(_.contains("mutation"))) == 2
      )
    },
    test("a paused reservation neither blocks publication nor replays mutation work") {
      for {
        remote      <- source()
        runtime     <- Gateway.compose(remote.subgraph).reloadable(config)
        entered     <- Promise.make[Nothing, Unit]
        release     <- Promise.make[Nothing, Unit]
        clock       <- pauseFirstNanoTime(entered, release)
        _           <- ZIO.addFinalizer(release.succeed(()).unit)
        pending     <- runtime.execute("mutation { setAdded }").withClock(clock).fork
        _           <- entered.await
        independent <- runtime.execute("{ value }")
        _           <- remote.setSchema(changed.replace("setValue", "setAdded"))
        after       <- poll(runtime)
        _           <- release.succeed(())
        response    <- pending.join
        requests    <- remote.stub.requests.get
      } yield assertTrue(
        independent.errors.isEmpty,
        after.active.id == 2L,
        response.errors.isEmpty,
        requests.count(_.query.exists(_.contains("mutation"))) == 1
      )
    },
    test("releases a racing reservation after admission closes but before draining starts") {
      for {
        remote         <- source()
        scope          <- Scope.make
        runtime        <- scope.extend(Gateway.compose(remote.subgraph).reloadable(config))
        requestEntered <- Promise.make[Nothing, Unit]
        requestRelease <- Promise.make[Nothing, Unit]
        closeEntered   <- Promise.make[Nothing, Unit]
        closeRelease   <- Promise.make[Nothing, Unit]
        requestClock   <- pauseFirstNanoTime(requestEntered, requestRelease)
        closeClock     <- pauseFirstNanoTime(closeEntered, closeRelease)
        _              <-
          ZIO.addFinalizer(requestRelease.succeed(()).unit *> closeRelease.succeed(()).unit *> scope.close(Exit.unit))
        pending        <- runtime.execute("mutation { setValue }").withClock(requestClock).fork
        _              <- requestEntered.await
        closing        <- scope.close(Exit.unit).withClock(closeClock).fork
        _              <- closeEntered.await
        _              <- requestRelease.succeed(())
        response       <- pending.join
        during         <- runtime.reloadStatus
        requests       <- remote.stub.requests.get
        _              <- closeRelease.succeed(())
        _              <- closing.join
      } yield assertTrue(
        response.errors.exists(_.msg == "Gateway is shutting down."),
        during.phase == Phase.Closing,
        during.active.status.lifecycle.state == GatewayInterpreter.LifecycleState.Running,
        during.active.status.lifecycle.active == 0,
        !requests.exists(_.query.exists(_.contains("mutation")))
      )
    },
    test("bounds generations and pauses polling behind uninterruptible retirement") {
      for {
        remote  <- source("type Query { remote: String }")
        started <- Promise.make[Nothing, Unit]
        release <- Promise.make[Nothing, Unit]
        runtime <-
          Gateway
            .compose(
              remote.subgraph,
              Subgraph
                .local("local", localGraph(started.succeed(()).unit *> ZIO.uninterruptible(release.await).as("done")))
            )
            .withConfig(
              _.withMaxConcurrentRequests(Int.MaxValue).withRequestTimeout(1.hour).withDrainTimeout(2.seconds)
            )
            .reloadable(config)
        _       <- ZIO.addFinalizer(release.succeed(()).unit)
        old     <- runtime.execute("{ value }").fork
        _       <- started.await
        _       <- remote.setSchema("type Query { remote: String added: String }")
        _       <- TestClock.adjust(1.second)
        _       <- awaitStatus(runtime)(_.active.id == 2L)
        _       <- remote.setSchema("type Query { remote: String added: String newest: String }")
        _       <- TestClock.adjust(10.seconds)
        blocked <- awaitStatus(runtime)(_.retirementOverdue)
        total   <- runtime.status
        checks  <- remote.checks.get
        result  <- runtime.execute("{ added }")
        pending <- old.poll
        _       <- release.succeed(())
        exit    <- old.await
        _       <- awaitStatus(runtime)(_.phase == Phase.Idle)
        next    <- poll(runtime)
      } yield assertTrue(
        blocked.active.id == 2L,
        blocked.retiring.exists(_.id == 1L),
        checks == 2,
        total.requests.limit == Int.MaxValue,
        pending.isEmpty,
        exit.isInterrupted,
        result.errors.isEmpty,
        next.active.id == 3L
      )
    },
    test("shutdown cancels acquisition and prevents late publication") {
      for {
        remote   <- source()
        scope    <- Scope.make
        runtime  <- scope.extend(Gateway.compose(remote.subgraph).reloadable(config))
        started  <- Promise.make[Nothing, Unit]
        release  <- Promise.make[Nothing, Unit]
        _        <- remote.setSchema(changed)
        _        <- remote.beforeSchema.set(started.succeed(()).unit *> release.await)
        _        <- TestClock.adjust(1.second)
        _        <- started.await
        _        <- scope.close(Exit.unit)
        _        <- release.succeed(())
        _        <- TestClock.adjust(10.seconds)
        closed   <- runtime.reloadStatus
        rejected <- runtime.execute("{ value }")
        check    <- runtime.check("{ value }").exit
        explain  <- runtime.explain("{ value }").exit
        calls    <- remote.checks.get
      } yield assertTrue(
        closed.phase == Phase.Closed,
        closed.active.id == 1L,
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
                                 .reloadable(config)
                             )
        _                 <- ZIO.addFinalizer(release.succeed(()).unit *> scope.close(Exit.unit))
        first             <- runtime.execute("{ value }").fork
        _                 <- firstStarted.await
        _                 <- remote.setSchema("type Query { remote: String added: String }")
        _                 <- TestClock.adjust(1.second)
        _                 <- awaitStatus(runtime)(
                               _.retiring.exists(_.status.lifecycle.state == GatewayInterpreter.LifecycleState.Draining)
                             )
        _                 <- TestClock.adjust(1.second)
        second            <- runtime.execute("{ value }").fork
        _                 <- secondStarted.await
        closing           <- scope.close(Exit.unit).fork
        _                 <- awaitStatus(runtime)(_.active.status.lifecycle.state == GatewayInterpreter.LifecycleState.Draining)
        _                 <- TestClock.adjust(1.second)
        retiring          <- awaitStatus(runtime)(_.retiring.exists(_.status.lifecycle.overdue == 1))
        activePending     <- second.poll
        _                 <- TestClock.adjust(1.second)
        _                 <- secondInterrupted.await
        oldPending        <- first.poll
        closePending      <- closing.poll
        _                 <- release.succeed(())
        firstExit         <- first.await
        secondExit        <- second.await
        _                 <- closing.join
        closed            <- runtime.reloadStatus
      } yield assertTrue(
        oldPending.isEmpty,
        closePending.isEmpty,
        activePending.isEmpty,
        retiring.phase == Phase.Closing,
        firstExit.isInterrupted,
        secondExit.isInterrupted,
        closed.phase == Phase.Closed,
        closed.active.status.lifecycle.active == 0
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
