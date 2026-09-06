package caliban.gateway

import caliban.gateway.GatewayTestSupport._
import caliban.gateway.PhaseHooks.{ AdmissionKind, CacheResult, Event }
import caliban.gateway.internal.OperationCache
import caliban.gateway.internal.OperationCache.Weighted
import caliban.parsing.adt.OperationType
import caliban.GraphQLRequest
import caliban.{ graphQL, RootResolver }
import caliban.schema.Schema.auto._
import sttp.model.Header
import zio.{ Duration, Promise, Ref, Scope, ZIO }
import zio.test.{ assert, assertTrue, Assertion, Spec, TestAspect, TestClock, TestEnvironment, ZIOSpecDefault }
import zio.stream.ZStream

object PhaseHooksSpec extends ZIOSpecDefault {

  final case class MetricQuery(value: String)
  final case class MetricSubscription(event: ZStream[Any, Throwable, Int])

  private val schema = "type Query { value: String! }"

  def spec: Spec[TestEnvironment with Scope, Any] = suite("PhaseHooksSpec")(
    test("idle subscriptions use dedicated lifetime and admission metrics") {
      for {
        opened           <- Promise.make[Nothing, Unit]
        source            = graphQL(
                              RootResolver(
                                queryResolver = Some(MetricQuery("ok")),
                                mutationResolver = Option.empty[Unit],
                                subscriptionResolver =
                                  Some(MetricSubscription(ZStream.fromZIO(opened.succeed(())) *> ZStream.never))
                              )
                            )
        runtime          <- (Gateway.compose(Subgraph.local("local", source)) @@ GatewayMetrics.hooks).interpreter
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
    test("subscription event counts come from duration metrics and finite work uses distinct admission kinds") {
      val source = graphQL(
        RootResolver(
          queryResolver = Some(MetricQuery("ok")),
          mutationResolver = Option.empty[Unit],
          subscriptionResolver = Some(MetricSubscription(ZStream(1, 2)))
        )
      )
      for {
        runtime        <- (Gateway.compose(Subgraph.local("local", source)) @@ GatewayMetrics.hooks).interpreter
        setupBefore    <- counter("caliban_gateway_admission_total", "kind", "subscription_setup")
        workBefore     <- counter("caliban_gateway_admission_total", "kind", "subscription_event")
        requestsBefore <- counter("caliban_gateway_admission_total", "kind", "request")
        eventsBefore   <- histogram("caliban_gateway_subscription_event_duration_seconds", "outcome" -> "success")
        events         <- runtime.executeStream(GraphQLRequest(query = Some("subscription { event }"))).runCollect
        setupAfter     <- counter("caliban_gateway_admission_total", "kind", "subscription_setup")
        workAfter      <- counter("caliban_gateway_admission_total", "kind", "subscription_event")
        requestsAfter  <- counter("caliban_gateway_admission_total", "kind", "request")
        eventsAfter    <- histogram("caliban_gateway_subscription_event_duration_seconds", "outcome" -> "success")
      } yield assertTrue(
        events.size == 2,
        setupAfter == setupBefore + 1d,
        workAfter == workBefore + 2d,
        requestsAfter == requestsBefore,
        eventsAfter == eventsBefore + 2L
      )
    },
    test("wraps orchestration and can transform remote headers") {
      for {
        recorded                <- recordEventsAndResults
        (events, results, hooks) = recorded
        remote                  <- stub("""{"data":{"value":"ok"}}""")
        runtime                 <- Gateway
                                     .compose(Subgraph.graphql("products", remote.endpoint, schema))
                                     .withPhaseHooks(hooks ++ taggingOutboundHeaders)
                                     .interpreter
        response                <-
          runtime.executeRequest(GraphQLRequest(query = Some("query Named { value }"), operationName = Some("Named")))
        observed                <- events.get
        completed               <- results.get
        headers                 <- remote.headers.get
      } yield assertTrue(
        response.errors.isEmpty,
        observed.headOption.contains(Event.Routing),
        observed.dropWhile(!_.isInstanceOf[Event.Request]).take(2) == Vector(
          Event.Request(Some("Named")),
          Event.Admission(AdmissionKind.Request)
        ),
        observed.contains(Event.CacheAccess(CacheResult.Miss)),
        observed.contains(Event.SubgraphCall("products", OperationType.Query)),
        observed.contains(Event.Admission(AdmissionKind.Subgraph)),
        observed.collect { case Event.Attempt(subgraph, number, _, _, _) => subgraph -> number } ==
          Vector("products" -> 0),
        observed.lastOption.contains(Event.Completion),
        completed.size == observed.size,
        completed.forall(_._2.outcome == PhaseHooks.Outcome.Success),
        headers.headOption.flatMap(_.get("x-gateway-wrapper")).contains("products")
      )
    },
    test("classifies intentional resolver rejections as request errors, not internal failures") {
      for {
        recorded           <- recordEventsAndResults
        (_, results, hooks) = recorded
        remote             <- stub("""{"data":{"value":"ok"}}""")
        runtime            <- (Gateway
                                .compose(Subgraph.graphql("remote", remote.endpoint, schema))
                                .withOperationResolver(
                                  OperationResolver[Any](_ =>
                                    ZIO.fail(OperationResolver.Rejection("Not found.", "PERSISTED_QUERY_NOT_FOUND"))
                                  )
                                )
                                .withPhaseHooks(hooks) @@ GatewayMetrics.hooks).interpreter
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
        sent               <- remote.requests.get
        routing             = completed.collect { case (Event.Routing, result) => result.outcome }
      } yield assertTrue(
        response.errors.map(_.msg) == List("Not found."),
        routing == Vector(PhaseHooks.Outcome.RequestError),
        completed.lastOption.exists(_._2.outcome == PhaseHooks.Outcome.RequestError),
        !completed.exists(_._2.outcome == PhaseHooks.Outcome.InternalError),
        after == before + 1L,
        sent.isEmpty
      )
    },
    test("counts request wrapper work toward the runtime deadline") {
      for {
        entered                 <- Promise.make[Nothing, Unit]
        recorded                <- recordEventsAndResults
        (events, results, hooks) = recorded
        remote                  <- stub("""{"data":{"value":"ok"}}""")
        runtime                 <- Gateway
                                     .compose(Subgraph.graphql("products", remote.endpoint, schema))
                                     .withConfig(_.withRequestTimeout(Duration.fromSeconds(1)))
                                     .withPhaseHooks(delaying(entered) ++ hooks)
                                     .interpreter
        fiber                   <- runtime.execute("{ value }").fork
        _                       <- entered.await
        _                       <- TestClock.adjust(Duration.fromSeconds(2))
        response                <- fiber.join
        requests                <- remote.requests.get
        observed                <- events.get
        completed               <- results.get
      } yield assertTrue(
        response.errors.map(_.msg) == List("Gateway request timed out."),
        requests.isEmpty,
        observed.lastOption.contains(Event.Completion),
        completed.lastOption.exists(_._2.outcome == PhaseHooks.Outcome.Timeout)
      )
    },
    test("records cache outcomes through the metrics wrapper") {
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
    test("records request and local execution metrics without local admission") {
      for {
        started        <- Promise.make[Nothing, Unit]
        runtime        <- (Gateway
                            .compose(Subgraph.local("local", localGraph(started.succeed(()).unit *> ZIO.never)))
                            .withConfig(_.withRequestTimeout(Duration.fromSeconds(1))) @@ GatewayMetrics.hooks).interpreter
        requestBefore  <- counter("caliban_gateway_admission_total", "kind", "request")
        subgraphBefore <- counter("caliban_gateway_admission_total", "kind", "subgraph")
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
        requestAfter   <- counter("caliban_gateway_admission_total", "kind", "request")
        subgraphAfter  <- counter("caliban_gateway_admission_total", "kind", "subgraph")
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
        requestAfter == requestBefore + 1.0,
        subgraphAfter == subgraphBefore,
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
        remote   <- stub("""{"data":{"value":"ok"}}""")
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
    test("observes a timed-out request without a document") {
      for {
        entered  <- Promise.make[Nothing, Unit]
        observed <- Ref.make(Vector.empty[OperationEvent])
        order    <- Ref.make(Vector.empty[String])
        remote   <- stub("""{"data":{"value":"ok"}}""")
        runtime  <- Gateway
                      .compose(Subgraph.graphql("products", remote.endpoint, schema))
                      .withConfig(_.withRequestTimeout(Duration.fromSeconds(1)))
                      .withPhaseHooks(delaying(entered) ++ observing(observed, order))
                      .interpreter
        fiber    <- runtime.execute("{ value }").fork
        _        <- entered.await
        _        <- TestClock.adjust(Duration.fromSeconds(2))
        response <- fiber.join
        events   <- observed.get
        sequence <- order.get
        requests <- remote.requests.get
      } yield assertTrue(
        response.errors.map(_.msg) == List("Gateway request timed out."),
        events.map(_.outcome) == Vector(PhaseHooks.Outcome.Timeout),
        events.forall(event => event.document.isEmpty && event.executionRequest.isEmpty),
        events.forall(_.operationType.isEmpty),
        sequence == Vector("direct-in", "direct-out"),
        requests.isEmpty
      )
    },
    test("observes an interrupted request as cancelled") {
      for {
        started  <- Promise.make[Nothing, Unit]
        observed <- Ref.make(Vector.empty[OperationEvent])
        order    <- Ref.make(Vector.empty[String])
        runtime  <- Gateway
                      .compose(Subgraph.local("local", localGraph(started.succeed(()).unit *> ZIO.never)))
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
    test("observes a failed preparation with the optional fields empty") {
      for {
        observed <- Ref.make(Vector.empty[OperationEvent])
        order    <- Ref.make(Vector.empty[String])
        remote   <- stub("""{"data":{"value":"ok"}}""")
        runtime  <- Gateway
                      .compose(Subgraph.graphql("remote", remote.endpoint, schema))
                      .withOperationResolver(
                        OperationResolver[Any](_ =>
                          ZIO.fail(OperationResolver.Rejection("Not found.", "PERSISTED_QUERY_NOT_FOUND"))
                        )
                      )
                      .withPhaseHooks(observing(observed, order))
                      .interpreter
        response <- runtime.executeRequest(GraphQLRequest())
        events   <- observed.get
        sequence <- order.get
        sent     <- remote.requests.get
      } yield assertTrue(
        response.errors.map(_.msg) == List("Not found."),
        events.map(_.outcome) == Vector(PhaseHooks.Outcome.RequestError),
        events.flatMap(_.errors.map(_.msg)) == Vector("Not found."),
        events.forall(event => event.document.isEmpty && event.executionRequest.isEmpty && event.operationType.isEmpty),
        sequence == Vector("direct-in", "direct-out"),
        sent.isEmpty
      )
    },
    test("executes the outgoing phase on interrupt of effect") {
      for {
        count <- Ref.make(0)
        // Should execute both incoming and outgoing phases
        first  = PhaseHooks.request(
                   PhaseHandler((ev: Event.Request) => ZIO.succeed(ev -> ()))((_, _, _) => count.incrementAndGet.unit)
                 )
        // Interrupts on the incoming phase triggering fork to halt
        second =
          PhaseHooks.request(
            PhaseHandler((ev: Event.Request) => ZIO.interrupt.as(ev -> ()))((_, _, _) => count.incrementAndGet.unit)
          )
        hooks  = first ++ second
        f     <- hooks.request.run(Event.Request(Some("Interrupt")))(ZIO.unit)(PhaseHooks.Result.classifyExit).exit.fork
        exit  <- f.join
        c     <- count.get
      } yield assertTrue(c == 1) && assert(exit)(Assertion.isInterrupted)
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential

  /**
   * Tags every outbound subgraph call with the subgraph it is addressed to.
   */
  private val taggingOutboundHeaders: PhaseHooks[Any] =
    PhaseHooks.outboundHeaders(
      PhaseHandler.incoming(ev =>
        ZIO.succeed(ev.copy(headers = Header("x-gateway-wrapper", ev.subgraph) :: ev.headers))
      )
    )

  private def delaying(entered: Promise[Nothing, Unit]): PhaseHooks[Any] =
    PhaseHooks.request(
      PhaseHandler.incomingDiscard(_ => entered.succeed(()).unit *> ZIO.sleep(Duration.fromSeconds(2)))
    )

  /**
   * Records the [[OperationEvent]] the request supplied through the mask callback. Both sides also append to `order`,
   * so a test can tell "the handler ran and produced no observation" apart from "the handler never ran".
   */
  private def observing(into: Ref[Vector[OperationEvent]], order: Ref[Vector[String]]): PhaseHooks[Any] =
    PhaseHooks.observeOperation(
      PhaseHandler[Any, Event.ObserveOperation, Nothing, Unit, OperationEvent](ev =>
        order.update(_ :+ "direct-in").as((ev, ()))
      )((_, _, event) => into.update(_ :+ event) *> order.update(_ :+ "direct-out"))
    )

  /**
   * The same recorder behind a [[Scope]], to pin the invariant that the scope outlives the handler's own outgoing side.
   */
  private def observingScoped(into: Ref[Vector[OperationEvent]], order: Ref[Vector[String]]): PhaseHooks[Any] =
    PhaseHooks.observeOperation(
      PhaseHandler.scoped[Any, Event.ObserveOperation, Nothing, OperationEvent](
        PhaseHandler[Scope, Event.ObserveOperation, Nothing, Unit, OperationEvent](ev =>
          order.update(_ :+ "scoped-in") *>
            ZIO.addFinalizer(order.update(_ :+ "scope-closed")).as((ev, ()))
        )((_, _, event) => into.update(_ :+ event) *> order.update(_ :+ "scoped-out"))
      )
    )

}
