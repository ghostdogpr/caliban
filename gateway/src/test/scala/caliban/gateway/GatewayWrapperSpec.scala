package caliban.gateway

import caliban.gateway.GatewayTestSupport._
import caliban.gateway.GatewayWrapper.{ AdmissionKind, CacheResult, Event }
import caliban.gateway.internal.OperationCache
import caliban.gateway.internal.OperationCache.Weighted
import caliban.parsing.adt.OperationType
import caliban.GraphQLRequest
import caliban.{ graphQL, RootResolver }
import caliban.schema.Schema.auto._
import sttp.model.Header
import zio.metrics.Metric
import zio.{ Duration, Exit, Promise, Ref, Scope, Trace, UIO, URIO, ZIO }
import zio.test.{ assertTrue, Spec, TestAspect, TestClock, TestEnvironment, ZIOSpecDefault }
import zio.stream.ZStream

object GatewayWrapperSpec extends ZIOSpecDefault {

  final case class MetricQuery(value: String)
  final case class MetricSubscription(event: ZStream[Any, Throwable, Int])

  private val schema = "type Query { value: String! }"

  def spec: Spec[TestEnvironment with Scope, Any] = suite("GatewayWrapper")(
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
        runtime          <- (Gateway.compose(Subgraph.local("local", source)) @@ GatewayMetrics.wrapper).interpreter
        requestsBefore   <- counter("caliban_gateway_requests_total", "outcome", "success")
        admittedBefore   <- counter("caliban_gateway_subscription_admission_total", "result", "accepted")
        terminatedBefore <- counter("caliban_gateway_subscription_terminations_total", "reason", "cancelled")
        fiber            <-
          runtime.executeStream(GraphQLRequest(query = Some("subscription { event }"))).runDrain.forkScoped
        _                <- opened.await
        active           <- gauge("caliban_gateway_subscriptions_active")
        requests         <- gauge("caliban_gateway_requests_active")
        requestPermits   <- gauge("caliban_gateway_admission_active", "kind", "request")
        setup            <- gauge("caliban_gateway_admission_active", "kind", "subscription_setup")
        event            <- gauge("caliban_gateway_admission_active", "kind", "subscription_event")
        _                <- fiber.interrupt
        after            <- gauge("caliban_gateway_subscriptions_active")
        admittedAfter    <- counter("caliban_gateway_subscription_admission_total", "result", "accepted")
        requestsAfter    <- counter("caliban_gateway_requests_total", "outcome", "success")
        terminatedAfter  <- counter("caliban_gateway_subscription_terminations_total", "reason", "cancelled")
      } yield assertTrue(
        active == 1d,
        requests == 0d,
        requestPermits == 0d,
        setup == 0d,
        event == 0d,
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
        runtime        <- (Gateway.compose(Subgraph.local("local", source)) @@ GatewayMetrics.wrapper).interpreter
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
        events    <- Ref.make(Vector.empty[GatewayWrapper.Event])
        results   <- Ref.make(Vector.empty[GatewayWrapper.Result])
        wrapper    = recording(events, results)
        remote    <- stub("""{"data":{"value":"ok"}}""")
        runtime   <- (Gateway.compose(Subgraph.graphql("products", remote.endpoint, schema)) @@ wrapper).interpreter
        response  <-
          runtime.executeRequest(GraphQLRequest(query = Some("query Named { value }"), operationName = Some("Named")))
        observed  <- events.get
        completed <- results.get
        headers   <- remote.headers.get
      } yield assertTrue(
        response.errors.isEmpty,
        observed.headOption.contains(Event.Routing),
        observed.dropWhile(!_.isInstanceOf[Event.Request]).take(3) == Vector(
          Event.Request(Some("Named")),
          Event.AdmissionWait(AdmissionKind.Request),
          Event.Admission(AdmissionKind.Request)
        ),
        observed.contains(Event.CacheAccess(CacheResult.Miss)),
        observed.contains(Event.SubgraphCall("products", OperationType.Query)),
        observed.contains(Event.AdmissionWait(AdmissionKind.Subgraph)),
        observed.contains(Event.Admission(AdmissionKind.Subgraph)),
        observed.collect { case Event.Attempt(subgraph, number, _, _, _) => subgraph -> number } ==
          Vector("products" -> 0),
        observed.lastOption.contains(Event.Completion),
        completed.size == observed.size,
        completed.forall(_.outcome == GatewayWrapper.Outcome.Success),
        headers.headOption.flatMap(_.get("x-gateway-wrapper")).contains("products")
      )
    },
    test("classifies intentional resolver rejections as request errors, not internal failures") {
      for {
        results   <- Ref.make(Vector.empty[(GatewayWrapper.Event, GatewayWrapper.Result)])
        wrapper    = new GatewayWrapper[Any] {
                       def wrap[R0, E, A](event: GatewayWrapper.Event)(effect: ZIO[R0, E, A])(
                         result: Exit[E, A] => GatewayWrapper.Result
                       )(implicit trace: Trace): ZIO[R0, E, A] =
                         effect.onExit(exit => results.update(_ :+ (event -> result(exit))))
                     }
        remote    <- stub("""{"data":{"value":"ok"}}""")
        runtime   <- (Gateway
                       .compose(Subgraph.graphql("remote", remote.endpoint, schema))
                       .withOperationResolver(
                         OperationResolver[Any](_ =>
                           ZIO.fail(OperationResolver.Rejection("Not found.", "PERSISTED_QUERY_NOT_FOUND"))
                         )
                       ) @@ (wrapper |+| GatewayMetrics.wrapper)).interpreter
        before    <- histogram(
                       "caliban_gateway_request_duration_seconds",
                       "outcome"        -> "request_error",
                       "operation_type" -> "unknown"
                     )
        response  <- runtime.executeRequest(GraphQLRequest())
        after     <- histogram(
                       "caliban_gateway_request_duration_seconds",
                       "outcome"        -> "request_error",
                       "operation_type" -> "unknown"
                     )
        completed <- results.get
        sent      <- remote.requests.get
        routing    = completed.collect { case (Event.Routing, result) => result.outcome }
      } yield assertTrue(
        response.errors.map(_.msg) == List("Not found."),
        routing == Vector(GatewayWrapper.Outcome.RequestError),
        completed.lastOption.exists(_._2.outcome == GatewayWrapper.Outcome.RequestError),
        !completed.exists(_._2.outcome == GatewayWrapper.Outcome.InternalError),
        after == before + 1L,
        sent.isEmpty
      )
    },
    test("counts request wrapper work toward the runtime deadline") {
      for {
        entered   <- Promise.make[Nothing, Unit]
        events    <- Ref.make(Vector.empty[GatewayWrapper.Event])
        results   <- Ref.make(Vector.empty[GatewayWrapper.Result])
        remote    <- stub("""{"data":{"value":"ok"}}""")
        wrapper    = delaying(entered) |+| recording(events, results)
        runtime   <- (Gateway
                       .compose(Subgraph.graphql("products", remote.endpoint, schema))
                       .withConfig(_.withRequestTimeout(Duration.fromSeconds(1))) @@ wrapper).interpreter
        fiber     <- runtime.execute("{ value }").fork
        _         <- entered.await
        _         <- TestClock.adjust(Duration.fromSeconds(2))
        response  <- fiber.join
        requests  <- remote.requests.get
        observed  <- events.get
        completed <- results.get
      } yield assertTrue(
        response.errors.map(_.msg) == List("Gateway request timed out."),
        requests.isEmpty,
        observed.contains(Event.RequestOverdue),
        observed.lastOption.contains(Event.Completion),
        completed.lastOption.exists(_.outcome == GatewayWrapper.Outcome.Timeout)
      )
    },
    test("records cache outcomes through the metrics wrapper") {
      for {
        cache      <- OperationCache.make[String, Nothing, Int, Any](16, GatewayMetrics.wrapper)
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
    test("decrements the admission waiting gauge when waiting is interrupted") {
      val waiting = "caliban_gateway_admission_waiting"

      for {
        entered <- Promise.make[Nothing, Unit]
        before  <- gauge(waiting, "kind", "request")
        fiber   <- GatewayMetrics.wrapper
                     .wrap(Event.AdmissionWait(AdmissionKind.Request))(
                       entered.succeed(()).unit *> ZIO.never
                     )(_ => GatewayWrapper.Result(GatewayWrapper.Outcome.Cancelled))
                     .fork
        _       <- entered.await
        active  <- gauge(waiting, "kind", "request")
        _       <- fiber.interrupt
        after   <- gauge(waiting, "kind", "request")
      } yield assertTrue(active == before + 1.0, after == before)
    },
    test("records request, admission, subgraph, and overdue metrics through the wrapper") {
      for {
        started        <- Promise.make[Nothing, Unit]
        runtime        <- (Gateway
                            .compose(Subgraph.local("local", localGraph(started.succeed(()).unit *> ZIO.never)))
                            .withConfig(_.withRequestTimeout(Duration.fromSeconds(1))) @@ GatewayMetrics.wrapper).interpreter
        requestBefore  <- counter("caliban_gateway_admission_total", "kind", "request")
        subgraphBefore <- counter("caliban_gateway_admission_total", "kind", "subgraph")
        requestsBefore <- counter("caliban_gateway_requests_total", "outcome", "error")
        callsBefore    <- counter("caliban_gateway_subgraph_calls_total", "subgraph", "local")
        overdueBefore  <- Metric.counter("caliban_gateway_overdue_requests_total").value.map(_.count)
        durationBefore <- histogram(
                            "caliban_gateway_request_duration_seconds",
                            "outcome"        -> "timeout",
                            "operation_type" -> "unknown"
                          )
        responseFiber  <- runtime.execute("{ value }").fork
        _              <- started.await
        requestsActive <- gauge("caliban_gateway_requests_active")
        requestActive  <- gauge("caliban_gateway_admission_active", "kind", "request")
        subgraphActive <- gauge("caliban_gateway_admission_active", "kind", "subgraph")
        _              <- TestClock.adjust(Duration.fromSeconds(1))
        response       <- responseFiber.join
        requestAfter   <- counter("caliban_gateway_admission_total", "kind", "request")
        subgraphAfter  <- counter("caliban_gateway_admission_total", "kind", "subgraph")
        requestsAfter  <- counter("caliban_gateway_requests_total", "outcome", "error")
        callsAfter     <- counter("caliban_gateway_subgraph_calls_total", "subgraph", "local")
        overdueAfter   <- Metric.counter("caliban_gateway_overdue_requests_total").value.map(_.count)
        durationAfter  <- histogram(
                            "caliban_gateway_request_duration_seconds",
                            "outcome"        -> "timeout",
                            "operation_type" -> "unknown"
                          )
        requestsDone   <- gauge("caliban_gateway_requests_active")
        requestDone    <- gauge("caliban_gateway_admission_active", "kind", "request")
        subgraphDone   <- gauge("caliban_gateway_admission_active", "kind", "subgraph")
      } yield assertTrue(
        response.errors.map(_.msg) == List("Gateway request timed out."),
        requestsActive == 1.0,
        requestActive == 1.0,
        subgraphActive == 1.0,
        requestAfter == requestBefore + 1.0,
        subgraphAfter == subgraphBefore + 1.0,
        requestsAfter == requestsBefore + 1.0,
        callsAfter == callsBefore + 1.0,
        overdueAfter == overdueBefore + 1.0,
        durationAfter == durationBefore + 1L,
        requestsDone == 0.0,
        requestDone == 0.0,
        subgraphDone == 0.0
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential

  private def recording(
    events: Ref[Vector[GatewayWrapper.Event]],
    results: Ref[Vector[GatewayWrapper.Result]]
  ): GatewayWrapper[Any] =
    new GatewayWrapper[Any] {
      def wrap[R0, E, A](event: GatewayWrapper.Event)(effect: ZIO[R0, E, A])(
        result: Exit[E, A] => GatewayWrapper.Result
      )(implicit trace: Trace): ZIO[R0, E, A] =
        events.update(_ :+ event) *> effect.onExit(exit => results.update(_ :+ result(exit)))

      override def outboundHeaders(subgraph: String, headers: List[Header])(implicit
        trace: Trace
      ): URIO[Any, List[Header]] =
        ZIO.succeed(Header("x-gateway-wrapper", subgraph) :: headers)
    }

  private def delaying(entered: Promise[Nothing, Unit]): GatewayWrapper[Any] =
    new GatewayWrapper[Any] {
      def wrap[R0, E, A](event: GatewayWrapper.Event)(effect: ZIO[R0, E, A])(
        result: Exit[E, A] => GatewayWrapper.Result
      )(implicit trace: Trace): ZIO[R0, E, A] =
        event match {
          case _: Event.Request => entered.succeed(()).unit *> ZIO.sleep(Duration.fromSeconds(2)) *> effect
          case _                => effect
        }
    }

}
