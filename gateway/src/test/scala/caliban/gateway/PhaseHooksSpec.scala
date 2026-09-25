package caliban.gateway

import caliban.gateway.GatewayTestSupport._
import caliban.gateway.PhaseHooks.{ CacheResult, Event }
import caliban.gateway.internal.OperationCache
import caliban.gateway.internal.OperationCache.Weighted
import caliban.parsing.adt.OperationType
import caliban.GraphQLRequest
import zio.http.{ Header, Status }
import zio.{ Duration, FiberRef, Promise, Ref, Scope, ZIO }
import zio.test.{ assert, assertTrue, Assertion, Spec, TestAspect, TestClock, TestEnvironment, ZIOSpecDefault }
import zio.stream.ZStream

object PhaseHooksSpec extends ZIOSpecDefault {

  private val schema = "type Query { value: String! }"

  def spec: Spec[TestEnvironment with Scope, Any] = suite("PhaseHooksSpec")(
    test("idle subscriptions use dedicated lifetime and admission metrics") {
      for {
        opened           <- Promise.make[Nothing, Unit]
        runtime          <-
          (subscriptionGateway(
            ZStream.fromZIO(opened.succeed(())) *> ZStream.never
          ) @@ GatewayMetrics.hooks).interpreter
        requestsBefore   <- counter("caliban_gateway_requests_total", "outcome", "success")
        admittedBefore   <- counter("caliban_gateway_subscription_admission_total", "result", "accepted")
        terminatedBefore <- counter("caliban_gateway_subscription_terminations_total", "reason", "cancelled")
        fiber            <-
          runtime.executeStream(GraphQLRequest(query = Some("subscription { event }"))).runDrain.forkScoped
        _                <- opened.await
        active           <- gauge("caliban_gateway_subscriptions_active")
        requests         <- gauge("caliban_gateway_requests_active")
        _                <- fiber.interrupt
        after            <- gauge("caliban_gateway_subscriptions_active")
        admittedAfter    <- counter("caliban_gateway_subscription_admission_total", "result", "accepted")
        requestsAfter    <- counter("caliban_gateway_requests_total", "outcome", "success")
        terminatedAfter  <- counter("caliban_gateway_subscription_terminations_total", "reason", "cancelled")
      } yield assertTrue(
        active == 1d,
        requests == 0d,
        after == 0d,
        admittedAfter == admittedBefore + 1d,
        requestsAfter == requestsBefore,
        terminatedAfter == terminatedBefore + 1d
      )
    },
    test("subscription event counts come from duration metrics") {
      for {
        runtime      <- (subscriptionGateway(ZStream(1, 2)) @@ GatewayMetrics.hooks).interpreter
        eventsBefore <- histogram("caliban_gateway_subscription_event_duration_seconds", "outcome" -> "success")
        events       <- runtime.executeStream(GraphQLRequest(query = Some("subscription { event }"))).runCollect
        eventsAfter  <- histogram("caliban_gateway_subscription_event_duration_seconds", "outcome" -> "success")
      } yield assertTrue(
        events.size == 2,
        eventsAfter == eventsBefore + 2L
      )
    },
    test("wraps orchestration and can transform remote headers") {
      for {
        recorded                <- recordEventsAndResults
        (events, results, hooks) = recorded
        remote                  <- stub(okResponse)
        runtime                 <- Gateway
                                     .compose(Subgraph.graphql("products", remote.endpoint, schema))
                                     .withPhaseHooks(hooks ++ taggingSubgraphCalls)
                                     .interpreter
        response                <-
          runtime.executeRequest(GraphQLRequest(query = Some("query Named { value }"), operationName = Some("Named")))
        observed                <- events.get
        completed               <- results.get
        headers                 <- remote.headers.get
      } yield assertTrue(
        response.errors.isEmpty,
        observed.headOption.contains(Event.Preparation),
        observed.collect { case event: Event.Execution => event } == Vector(Event.Execution(Some("Named"))),
        observed.contains(Event.CacheAccess(CacheResult.Miss)),
        observed.contains(Event.SubgraphCall("products", OperationType.Query)),
        observed.collect { case event: Event.Attempt => event.subgraph -> event.number } ==
          Vector("products" -> 0),
        observed.lastOption.contains(Event.Completion),
        completed.size == observed.size,
        completed.forall(_._2.outcome == PhaseHooks.Outcome.Success),
        headers.headOption.flatMap(_.get("x-gateway-hook")).contains("products")
      )
    },
    test("transforms headers for each attempt and counts only retries") {
      val config  = RemoteGraphQLConfig.default.withExecution(_.withRetries(1, Duration.Zero))
      val headers = PhaseHooks.attempt(
        PhaseHandler.incoming[Any, Event.Attempt, Nothing](event =>
          ZIO.succeed(event.copy(headers = Header.Custom("X-Attempt", event.number.toString) :: event.headers))
        )
      )
      for {
        calls      <- Ref.make(0)
        callHeaders =
          PhaseHooks.subgraphCall(
            PhaseHandler.incoming[Any, Event.SubgraphCall, Nothing](event =>
              calls
                .updateAndGet(_ + 1)
                .map(number => event.copy(headers = Header.Custom("X-Call", number.toString) :: event.headers))
            )
          )
        remote     <- stubWithStatuses(Status.ServiceUnavailable -> "{}", Status.Ok -> okResponse)
        runtime    <- (Gateway.compose(Subgraph.graphql("products", remote.endpoint, schema, config)) @@
                        (callHeaders ++ headers ++ GatewayMetrics.hooks)).interpreter
        before     <- counter("caliban_gateway_retries_total", "subgraph", "products")
        response   <- runtime.execute("{ value }")
        sent       <- remote.headers.get
        after      <- counter("caliban_gateway_retries_total", "subgraph", "products")
        callCount  <- calls.get
      } yield assertTrue(
        response.errors.isEmpty,
        sent.map(_.get("x-attempt")) == Vector(Some("0"), Some("1")),
        sent.map(_.get("x-call")) == Vector(Some("1"), Some("1")),
        callCount == 1,
        after == before + 1d
      )
    },
    test("keeps calls with different hook headers separate during deduplication") {
      for {
        identity    <- FiberRef.make("")
        arrivals    <- Ref.make(0)
        bothStarted <- Promise.make[Nothing, Unit]
        release     <- Promise.make[Nothing, Unit]
        remote      <-
          stubWith(
            arrivals.updateAndGet(_ + 1).flatMap(count => bothStarted.succeed(()).unit.whenDiscard(count == 2)) *>
              release.await,
            okResponse
          )
        headers      =
          PhaseHooks.subgraphCall(
            PhaseHandler.incoming[Any, Event.SubgraphCall, Nothing](event =>
              identity.get.map(value => event.copy(headers = Header.Custom("X-Identity", value) :: event.headers))
            )
          )
        runtime     <- (Gateway.compose(Subgraph.graphql("products", remote.endpoint, schema)) @@ headers).interpreter
        first       <- identity.locally("first")(runtime.execute("{ value }")).forkScoped
        second      <- identity.locally("second")(runtime.execute("{ value }")).forkScoped
        _           <- bothStarted.await
        _           <- release.succeed(())
        responses   <- first.join.zip(second.join)
        sent        <- remote.headers.get
      } yield assertTrue(
        responses._1.errors.isEmpty,
        responses._2.errors.isEmpty,
        sent.flatMap(_.get("x-identity")).toSet == Set("first", "second"),
        sent.size == 2
      )
    } @@ TestAspect.timeout(Duration.fromSeconds(10)),
    test("reports subgraph timeouts after spending part of the deadline resolving headers") {
      for {
        headersStarted     <- Promise.make[Nothing, Unit]
        requestStarted     <- Promise.make[Nothing, Unit]
        recorded           <- recordEventsAndResults
        (_, results, hooks) = recorded
        remote             <- stubWith(requestStarted.succeed(()).unit *> ZIO.never, okResponse)
        config              = RemoteGraphQLConfig.default
                                .withExecution(_.withTimeout(Duration.fromSeconds(2)).withInFlightQueryDeduplication(false))
                                .withExecutionHeadersZIO(
                                  headersStarted.succeed(()) *> ZIO.sleep(Duration.fromSeconds(1)).as(Nil)
                                )
        runtime            <- (Gateway.compose(Subgraph.graphql("products", remote.endpoint, schema, config)) @@ hooks).interpreter
        fiber              <- runtime.execute("{ value }").forkScoped
        _                  <- headersStarted.await
        _                  <- TestClock.adjust(Duration.fromSeconds(1))
        _                  <- requestStarted.await
        _                  <- TestClock.adjust(Duration.fromSeconds(1))
        response           <- fiber.join
        completed          <- results.get
      } yield assertTrue(
        response.errors.nonEmpty,
        completed.collect { case (_: Event.SubgraphCall, result) => result.outcome } == Vector(
          PhaseHooks.Outcome.Timeout
        )
      )
    } @@ TestAspect.timeout(Duration.fromSeconds(10)),
    test("classifies intentional resolver rejections as request errors and observes them without a document") {
      for {
        recorded           <- recordEventsAndResults
        (_, results, hooks) = recorded
        observed           <- Ref.make(Vector.empty[OperationEvent])
        order              <- Ref.make(Vector.empty[String])
        remote             <- stub(okResponse)
        runtime            <-
          (Gateway
            .compose(Subgraph.graphql("remote", remote.endpoint, schema))
            .withPhaseHooks(
              PhaseHooks.resolution[Any](_ => ZIO.fail(PhaseHooks.Rejection("Not found.", "PERSISTED_QUERY_NOT_FOUND")))
            )
            .withPhaseHooks(hooks)
            .withPhaseHooks(observing(observed, order)) @@ GatewayMetrics.hooks).interpreter
        before             <- histogram(
                                "caliban_gateway_request_duration_seconds",
                                "outcome"        -> "request_error",
                                "operation_type" -> "unknown"
                              )
        response           <- runtime.executeRequest(GraphQLRequest())
        after              <- histogram(
                                "caliban_gateway_request_duration_seconds",
                                "outcome"        -> "request_error",
                                "operation_type" -> "unknown"
                              )
        completed          <- results.get
        events             <- observed.get
        sequence           <- order.get
        sent               <- remote.requests.get
        preparation         = completed.collect { case (Event.Preparation, result) => result.outcome }
      } yield assertTrue(
        response.errors.map(_.msg) == List("Not found."),
        preparation == Vector(PhaseHooks.Outcome.RequestError),
        completed.lastOption.exists(_._2.outcome == PhaseHooks.Outcome.RequestError),
        !completed.exists(_._2.outcome == PhaseHooks.Outcome.InternalError),
        after == before + 1L,
        events.map(_.outcome) == Vector(PhaseHooks.Outcome.RequestError),
        events.flatMap(_.errors.map(_.msg)) == Vector("Not found."),
        events.forall(event => event.document.isEmpty && event.executionRequest.isEmpty && event.operationType.isEmpty),
        sequence == Vector("direct-in", "direct-out"),
        sent.isEmpty
      )
    },
    test("counts execution hook work toward the runtime deadline and observes the timeout without a document") {
      for {
        entered                 <- Promise.make[Nothing, Unit]
        recorded                <- recordEventsAndResults
        (events, results, hooks) = recorded
        operations              <- Ref.make(Vector.empty[OperationEvent])
        order                   <- Ref.make(Vector.empty[String])
        remote                  <- stub(okResponse)
        runtime                 <- Gateway
                                     .compose(Subgraph.graphql("products", remote.endpoint, schema))
                                     .withConfig(_.withRequestTimeout(Duration.fromSeconds(1)))
                                     .withPhaseHooks(delaying(entered) ++ hooks ++ observing(operations, order))
                                     .interpreter
        fiber                   <- runtime.execute("{ value }").fork
        _                       <- entered.await
        _                       <- TestClock.adjust(Duration.fromSeconds(2))
        response                <- fiber.join
        sent                    <- remote.requests.get
        observed                <- events.get
        completed               <- results.get
        observations            <- operations.get
        sequence                <- order.get
      } yield assertTrue(
        response.errors.map(_.msg) == List("Gateway request timed out."),
        sent.isEmpty,
        observed.lastOption.contains(Event.Completion),
        completed.lastOption.exists(_._2.outcome == PhaseHooks.Outcome.Timeout),
        observations.map(_.outcome) == Vector(PhaseHooks.Outcome.Timeout),
        observations.forall(event => event.document.isEmpty && event.executionRequest.isEmpty),
        observations.forall(_.operationType.isEmpty),
        sequence == Vector("direct-in", "direct-out")
      )
    },
    test("records cache outcomes through the metrics hooks") {
      for {
        cache      <- OperationCache.make[String, Nothing, Int, Any](16, GatewayMetrics.hooks)
        missBefore <- counter("caliban_gateway_operation_cache_total", "result", "miss")
        hitBefore  <- counter("caliban_gateway_operation_cache_total", "result", "hit")
        _          <- cache.getOrCompute("operation")(ZIO.succeed(Weighted(1, 1)))
        _          <- cache.getOrCompute("operation")(ZIO.dieMessage("cache missed"))
        missAfter  <- counter("caliban_gateway_operation_cache_total", "result", "miss")
        hitAfter   <- counter("caliban_gateway_operation_cache_total", "result", "hit")
      } yield assertTrue(
        missAfter == missBefore + 1.0,
        hitAfter == hitBefore + 1.0
      )
    },
    test("records request and local execution metrics") {
      for {
        started        <- Promise.make[Nothing, Unit]
        runtime        <-
          (localGateway(started.succeed(()).unit *> ZIO.never)
            .withConfig(_.withRequestTimeout(Duration.fromSeconds(1))) @@ GatewayMetrics.hooks).interpreter
        requestsBefore <- counter("caliban_gateway_requests_total", "outcome", "error")
        callsBefore    <- counter("caliban_gateway_subgraph_calls_total", "subgraph", "local")
        durationBefore <- histogram(
                            "caliban_gateway_request_duration_seconds",
                            "outcome"        -> "timeout",
                            "operation_type" -> "unknown"
                          )
        responseFiber  <- runtime.execute("{ value }").fork
        _              <- started.await
        requestsActive <- gauge("caliban_gateway_requests_active")
        _              <- TestClock.adjust(Duration.fromSeconds(1))
        response       <- responseFiber.join
        requestsAfter  <- counter("caliban_gateway_requests_total", "outcome", "error")
        callsAfter     <- counter("caliban_gateway_subgraph_calls_total", "subgraph", "local")
        durationAfter  <- histogram(
                            "caliban_gateway_request_duration_seconds",
                            "outcome"        -> "timeout",
                            "operation_type" -> "unknown"
                          )
        requestsDone   <- gauge("caliban_gateway_requests_active")
      } yield assertTrue(
        response.errors.map(_.msg) == List("Gateway request timed out."),
        requestsActive == 1.0,
        requestsAfter == requestsBefore + 1.0,
        callsAfter == callsBefore + 1.0,
        durationAfter == durationBefore + 1L,
        requestsDone == 0.0
      )
    },
    test("delivers an operation observation through combined and scoped handlers") {
      for {
        direct   <- Ref.make(Vector.empty[OperationEvent])
        inScope  <- Ref.make(Vector.empty[OperationEvent])
        order    <- Ref.make(Vector.empty[String])
        remote   <- stub(okResponse)
        runtime  <- Gateway
                      .compose(Subgraph.graphql("products", remote.endpoint, schema))
                      .withPhaseHooks(observingScoped(inScope, order) ++ observing(direct, order))
                      .interpreter
        response <-
          runtime.executeRequest(GraphQLRequest(query = Some("query Named { value }"), operationName = Some("Named")))
        observed <- direct.get
        scoped   <- inScope.get
        sequence <- order.get
      } yield assertTrue(
        response.errors.isEmpty,
        observed.map(event => event.operationType -> event.outcome) ==
          Vector(Some(OperationType.Query) -> PhaseHooks.Outcome.Success),
        observed.forall(event => event.document.isDefined && event.executionRequest.isDefined),
        scoped == observed,
        //  "scoped-out" precedes "scope-closed", i.e. the scope outlives the handler's own outgoing side.
        // "direct-out" precedes "scoped-out" for the same reason: a scoped handler nests the ones combined after it.
        sequence == Vector("scoped-in", "direct-in", "direct-out", "scoped-out", "scope-closed")
      )
    },
    test("observes an interrupted request as cancelled") {
      for {
        started  <- Promise.make[Nothing, Unit]
        observed <- Ref.make(Vector.empty[OperationEvent])
        order    <- Ref.make(Vector.empty[String])
        runtime  <- localGateway(started.succeed(()).unit *> ZIO.never)
                      .withPhaseHooks(observing(observed, order))
                      .interpreter
        fiber    <- runtime.execute("{ value }").fork
        _        <- started.await
        _        <- fiber.interrupt
        events   <- observed.get
        sequence <- order.get
      } yield assertTrue(
        events.map(_.outcome) == Vector(PhaseHooks.Outcome.Cancelled),
        events.forall(_.document.isEmpty),
        sequence == Vector("direct-in", "direct-out")
      )
    },
    test("executes the outgoing phase on interrupt of effect") {
      for {
        count <- Ref.make(0)
        // Should execute both incoming and outgoing phases
        first  =
          PhaseHooks.execution(
            PhaseHandler((event: Event.Execution) => ZIO.succeed((event, ())))((_, _, _) => count.incrementAndGet.unit)
          )
        // Interrupts on the incoming phase triggering fork to halt
        second =
          PhaseHooks.execution(
            PhaseHandler((event: Event.Execution) => ZIO.interrupt.as((event, ())))((_, _, _) =>
              count.incrementAndGet.unit
            )
          )
        hooks  = first ++ second
        fiber <-
          hooks.execution.run(Event.Execution(Some("Interrupt")))(ZIO.unit)(PhaseHooks.Result.classifyExit).exit.fork
        exit  <- fiber.join
        runs  <- count.get
      } yield assertTrue(runs == 1) && assert(exit)(Assertion.isInterrupted)
    },
    test("runs the outgoing phase when the wrapped effect is interrupted from outside") {
      for {
        count   <- Ref.make(0)
        started <- Promise.make[Nothing, Unit]
        hooks    =
          PhaseHooks.execution(
            PhaseHandler((event: Event.Execution) => ZIO.succeed((event, ())))((_, _, _) => count.incrementAndGet.unit)
          )
        fiber   <- hooks.execution
                     .run(Event.Execution(Some("Interrupt")))(started.succeed(()) *> ZIO.never)(
                       PhaseHooks.Result.classifyExit
                     )
                     .fork
        _       <- started.await
        exit    <- fiber.interrupt
        runs    <- count.get
      } yield assertTrue(runs == 1) && assert(exit)(Assertion.isInterrupted)
    },
    test("an incoming phase can be interrupted") {
      for {
        started <- Promise.make[Nothing, Unit]
        hooks    =
          PhaseHooks.execution(
            PhaseHandler((event: Event.Execution) => started.succeed(()) *> ZIO.never.as((event, ())))((_, _, _) =>
              ZIO.unit
            )
          )
        fiber   <- hooks.execution.run(Event.Execution(Some("Interrupt")))(ZIO.unit)(PhaseHooks.Result.classifyExit).fork
        _       <- started.await
        exit    <- fiber.interrupt
      } yield assert(exit)(Assertion.isInterrupted)
    } @@ TestAspect.timeout(Duration.fromSeconds(10))
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential

  /**
   * Tags every outbound subgraph call with the subgraph it is addressed to.
   */
  private val taggingSubgraphCalls: PhaseHooks[Any] =
    PhaseHooks.subgraphCall(
      PhaseHandler.incoming(event =>
        ZIO.succeed(event.copy(headers = Header.Custom("x-gateway-hook", event.subgraph) :: event.headers))
      )
    )

  private def delaying(entered: Promise[Nothing, Unit]): PhaseHooks[Any] =
    PhaseHooks.execution(
      PhaseHandler.incomingDiscard(_ => entered.succeed(()).unit *> ZIO.sleep(Duration.fromSeconds(2)))
    )

  /**
   * Records the [[OperationEvent]] the request supplied through the mask callback. Both sides also append to `order`,
   * so a test can tell "the handler ran and produced no observation" apart from "the handler never ran".
   */
  private def observing(into: Ref[Vector[OperationEvent]], order: Ref[Vector[String]]): PhaseHooks[Any] =
    PhaseHooks.operation(
      PhaseHandler[Any, Event.Operation, Nothing, Unit, OperationEvent](event =>
        order.update(_ :+ "direct-in").as((event, ()))
      )((_, _, event) => into.update(_ :+ event) *> order.update(_ :+ "direct-out"))
    )

  /**
   * The same recorder behind a [[Scope]], to pin the invariant that the scope outlives the handler's own outgoing side.
   */
  private def observingScoped(into: Ref[Vector[OperationEvent]], order: Ref[Vector[String]]): PhaseHooks[Any] =
    PhaseHooks.operation(
      PhaseHandler.scoped[Any, Event.Operation, Nothing, OperationEvent](
        PhaseHandler[Scope, Event.Operation, Nothing, Unit, OperationEvent](event =>
          order.update(_ :+ "scoped-in") *>
            ZIO.addFinalizer(order.update(_ :+ "scope-closed")).as((event, ()))
        )((_, _, event) => into.update(_ :+ event) *> order.update(_ :+ "scoped-out"))
      )
    )

}
