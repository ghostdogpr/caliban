package caliban.gateway

import caliban.gateway.GatewayTestSupport._
import caliban.gateway.GatewayWrapper.{ AdmissionKind, CacheResult, Event }
import caliban.gateway.internal.OperationCache
import caliban.gateway.internal.OperationCache.Weighted
import caliban.parsing.adt.OperationType
import caliban.GraphQLRequest
import sttp.model.Header
import zio.metrics.Metric
import zio.{ Duration, Exit, Promise, Ref, Scope, Trace, UIO, URIO, ZIO }
import zio.test.{ assertTrue, Spec, TestAspect, TestClock, TestEnvironment, ZIOSpecDefault }

object GatewayWrapperSpec extends ZIOSpecDefault {

  private val schema = "type Query { value: String! }"

  def spec: Spec[TestEnvironment with Scope, Any] = suite("GatewayWrapper")(
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
        observed.take(4) == Vector(
          Event.Request(Some("Named")),
          Event.AdmissionWait(AdmissionKind.Request),
          Event.Admission(AdmissionKind.Request),
          Event.Routing
        ),
        observed.contains(Event.CacheAccess(CacheResult.Miss)),
        observed.contains(Event.SourceCall("products", OperationType.Query)),
        observed.contains(Event.AdmissionWait(AdmissionKind.Source)),
        observed.contains(Event.Admission(AdmissionKind.Source)),
        observed.collect { case Event.Attempt(source, number, _, _, _) => source -> number } ==
          Vector("products" -> 0),
        observed.lastOption.contains(Event.Completion),
        completed.size == observed.size,
        completed.forall(_.outcome == GatewayWrapper.Outcome.Success),
        headers.headOption.flatMap(_.get("x-gateway-wrapper")).contains("products")
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
    test("records request, admission, source, and overdue metrics through the wrapper") {
      for {
        started        <- Promise.make[Nothing, Unit]
        runtime        <- (Gateway
                            .compose(Subgraph.local("local", localGraph(started.succeed(()).unit *> ZIO.never)))
                            .withConfig(_.withRequestTimeout(Duration.fromSeconds(1))) @@ GatewayMetrics.wrapper).interpreter
        requestBefore  <- counter("caliban_gateway_admission_total", "kind", "request")
        sourceBefore   <- counter("caliban_gateway_admission_total", "kind", "source")
        requestsBefore <- counter("caliban_gateway_requests_total", "outcome", "error")
        callsBefore    <- counter("caliban_gateway_source_calls_total", "source", "local")
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
        sourceActive   <- gauge("caliban_gateway_admission_active", "kind", "source")
        _              <- TestClock.adjust(Duration.fromSeconds(1))
        response       <- responseFiber.join
        requestAfter   <- counter("caliban_gateway_admission_total", "kind", "request")
        sourceAfter    <- counter("caliban_gateway_admission_total", "kind", "source")
        requestsAfter  <- counter("caliban_gateway_requests_total", "outcome", "error")
        callsAfter     <- counter("caliban_gateway_source_calls_total", "source", "local")
        overdueAfter   <- Metric.counter("caliban_gateway_overdue_requests_total").value.map(_.count)
        durationAfter  <- histogram(
                            "caliban_gateway_request_duration_seconds",
                            "outcome"        -> "timeout",
                            "operation_type" -> "unknown"
                          )
        requestsDone   <- gauge("caliban_gateway_requests_active")
        requestDone    <- gauge("caliban_gateway_admission_active", "kind", "request")
        sourceDone     <- gauge("caliban_gateway_admission_active", "kind", "source")
      } yield assertTrue(
        response.errors.map(_.msg) == List("Gateway request timed out."),
        requestsActive == 1.0,
        requestActive == 1.0,
        sourceActive == 1.0,
        requestAfter == requestBefore + 1.0,
        sourceAfter == sourceBefore + 1.0,
        requestsAfter == requestsBefore + 1.0,
        callsAfter == callsBefore + 1.0,
        overdueAfter == overdueBefore + 1.0,
        durationAfter == durationBefore + 1L,
        requestsDone == 0.0,
        requestDone == 0.0,
        sourceDone == 0.0
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

      override def outboundHeaders(source: String, headers: List[Header])(implicit
        trace: Trace
      ): URIO[Any, List[Header]] =
        ZIO.succeed(Header("x-gateway-wrapper", source) :: headers)
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
