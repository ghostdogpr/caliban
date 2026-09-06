package caliban.gateway

import caliban.GraphQLRequest
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.tracing.GatewayTracing
import caliban.tracing.TracingMock
import io.opentelemetry.api.common.AttributeKey
import io.opentelemetry.api.trace.{ SpanId, StatusCode }
import zio.Duration
import zio.http.{ Header, Status }
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.{ Promise, Scope, ZIO }
import zio.stream.ZStream
import zio.test.{ assertTrue, Spec, TestAspect, TestClock, TestEnvironment, ZIOSpecDefault }

import java.nio.charset.StandardCharsets.UTF_8

import scala.jdk.CollectionConverters._

object GatewayTracingSpec extends ZIOSpecDefault {

  private val schema = "type Query { value: String! }"

  def spec: Spec[TestEnvironment with Scope, Any] = suite("Gateway tracing")(
    test("subscription spans inherit supplied context and are independent without it") {
      val body =
        "event: next\ndata: {\"data\":{\"event\":1}}\n\nevent: next\ndata: {\"data\":{\"event\":2}}\n\nevent: complete\n\n"
      ZIO
        .foreach(List("none", "incoming", "ambient")) { context =>
          val traceId  = "4bf92f3577b34da6a3ce929d0e0e4736"
          val parentId = "00f067aa0ba902b7"
          val headers  =
            if (context == "incoming") List(Header.Custom("traceparent", s"00-$traceId-$parentId-01")) else Nil
          for {
            endpoint      <- streamingEndpoint(
                               ZStream.fromIterable(body.getBytes(UTF_8)),
                               mediaType = "text/event-stream"
                             )
            config         = RemoteGraphQLConfig.default.withSubscription(
                               RemoteSubscriptionConfig(transport = RemoteSubscriptionConfig.Sse())
                             )
            runtime       <-
              (Gateway.compose(
                Subgraph
                  .graphql("remote", endpoint, "type Query { value: String } type Subscription { event: Int }", config)
              ) @@ GatewayTracing.hooks).interpreter
            before        <- TracingMock.getFinishedSpans.map(_.size)
            consume        = runtime
                               .executeStream(GraphQLRequest(query = Some("subscription { event }")), headers)
                               .runCollect
            events        <- if (context == "ambient") ZIO.serviceWithZIO[Tracing](_.span("subscription-caller")(consume))
                             else consume
            spans         <- TracingMock.getFinishedSpans.map(_.drop(before))
            observed       = spans.filter(_.getName.startsWith("caliban.gateway.subscription."))
            caller         = spans.find(_.getName == "subscription-caller")
            requestSpan    = spans.find(_.getName == "caliban.gateway.request")
            setup          = observed.find(_.getName == "caliban.gateway.subscription.setup")
            eventSpans     = observed.filter(_.getName == "caliban.gateway.subscription.event")
            expectedParent = context match {
                               case "incoming" => Some(parentId)
                               case "ambient"  => caller.map(_.getSpanId)
                               case _          => Some(SpanId.getInvalid)
                             }
            expectedTrace  = if (context == "incoming") Some(traceId) else caller.map(_.getTraceId)
          } yield assertTrue(
            events.size == 2,
            observed.count(_.getName == "caliban.gateway.subscription.setup") == 1,
            eventSpans.size == 2,
            // The request span covers preparation and stream construction; the subscription outlives it.
            spans.count(_.getName == "caliban.gateway.request") == 1,
            observed.forall(span => !requestSpan.map(_.getSpanId).contains(span.getParentSpanId)),
            expectedParent.nonEmpty,
            observed.forall(span => expectedParent.contains(span.getParentSpanId)),
            if (context == "none") observed.map(_.getTraceId).distinct.size == 3
            else expectedTrace.nonEmpty && observed.forall(span => expectedTrace.contains(span.getTraceId)),
            setup.exists(span => eventSpans.forall(_.getStartEpochNanos >= span.getEndEpochNanos))
          )
        }
        .map(_.reduce(_ && _))
    },
    test("traces a remote request without recording raw GraphQL data and propagates W3C context") {
      for {
        remote        <- stub("""{"data":{"value":"ok"}}""")
        gateway        = (Gateway.compose(
                           Subgraph.graphql("products", remote.endpoint, schema)
                         ) @@ GatewayTracing.hooks: Gateway[Tracing])
        runtime       <- gateway.interpreter
        spansBefore   <- TracingMock.getFinishedSpans.map(_.size)
        response      <- ZIO.serviceWithZIO[Tracing](_.span("caller")(runtime.execute("query PublicName { value }")))
        sentHeaders   <- remote.headers.get
        spans         <- TracingMock.getFinishedSpans.map(_.drop(spansBefore))
        gatewaySpans   = spans.filter(_.getName.startsWith("caliban.gateway."))
        callerSpan     = spans.find(_.getName == "caller")
        requestSpan    = gatewaySpans.find(_.getName == "caliban.gateway.request")
        attributeNames = gatewaySpans.flatMap(_.getAttributes.asMap.keySet.asScala.map(_.getKey))
      } yield assertTrue(
        response.errors.isEmpty,
        gatewaySpans.map(_.getName).toSet == Set(
          "caliban.gateway.request",
          "caliban.gateway.routing",
          "caliban.gateway.subgraph",
          "caliban.gateway.subgraph.attempt",
          "caliban.gateway.completion"
        ),
        requestSpan.exists(span =>
          span.getAttributes.get(AttributeKey.stringKey("graphql.operation.type")) == "query" &&
            span.getAttributes.get(AttributeKey.stringKey("caliban.gateway.request.outcome")) == "success"
        ),
        requestSpan.map(_.getParentSpanId) == callerSpan.map(_.getSpanId),
        gatewaySpans
          .filter(span =>
            span.getName != "caliban.gateway.request" && span.getName != "caliban.gateway.subgraph.attempt"
          )
          .forall(span => requestSpan.map(_.getSpanId).contains(span.getParentSpanId)),
        gatewaySpans.find(_.getName == "caliban.gateway.subgraph.attempt").map(_.getParentSpanId) ==
          gatewaySpans.find(_.getName == "caliban.gateway.subgraph").map(_.getSpanId),
        gatewaySpans
          .find(_.getName == "caliban.gateway.subgraph")
          .exists(_.getKind == io.opentelemetry.api.trace.SpanKind.INTERNAL),
        gatewaySpans
          .find(_.getName == "caliban.gateway.subgraph.attempt")
          .exists(span =>
            span.getKind == io.opentelemetry.api.trace.SpanKind.CLIENT &&
              span.getAttributes.get(AttributeKey.longKey("http.response.status_code")) == 200L &&
              span.getAttributes.get(AttributeKey.longKey("http.request.resend_count")) == 0L
          ),
        sentHeaders.headOption.flatMap(_.get("traceparent")).exists(_.nonEmpty),
        !attributeNames.exists(name => name.contains("document") || name.contains("query") || name.contains("variable"))
      )
    },
    test("keeps preparation in the caller's trace when only a traceparent header arrives") {
      val traceId  = "4bf92f3577b34da6a3ce929d0e0e4736"
      val parentId = "00f067aa0ba902b7"

      for {
        remote      <- stub("""{"data":{"value":"ok"}}""")
        runtime     <- (Gateway.compose(
                         Subgraph.graphql("products", remote.endpoint, schema)
                       ) @@ GatewayTracing.hooks).interpreter
        spansBefore <- TracingMock.getFinishedSpans.map(_.size)
        response    <- runtime.executeRequest(
                         GraphQLRequest(query = Some("{ value }")),
                         List(Header("traceparent", s"00-$traceId-$parentId-01"))
                       )
        spans       <- TracingMock.getFinishedSpans.map(_.drop(spansBefore))
        gatewaySpans = spans.filter(_.getName.startsWith("caliban.gateway."))
        requestSpan  = gatewaySpans.find(_.getName == "caliban.gateway.request")
        routingSpan  = gatewaySpans.find(_.getName == "caliban.gateway.routing")
      } yield assertTrue(
        response.errors.isEmpty,
        routingSpan.nonEmpty,
        gatewaySpans.forall(_.getTraceId == traceId),
        requestSpan.map(_.getParentSpanId).contains(parentId),
        routingSpan.map(_.getParentSpanId) == requestSpan.map(_.getSpanId)
      )
    },
    test("traces retry attempts") {
      val config = RemoteGraphQLConfig.default.withExecution(_.withRetries(1, Duration.Zero))

      for {
        remote      <- stubWithStatuses(
                         Status.ServiceUnavailable -> "{}",
                         Status.Ok                 -> """{"data":{"value":"ok"}}"""
                       )
        runtime     <- (Gateway.compose(
                         Subgraph.graphql("products", remote.endpoint, schema, config)
                       ) @@ GatewayTracing.hooks).interpreter
        spansBefore <- TracingMock.getFinishedSpans.map(_.size)
        response    <- ZIO.serviceWithZIO[Tracing](_.span("caller")(runtime.execute("{ value }")))
        requests    <- remote.requests.get
        headers     <- remote.headers.get
        spans       <- TracingMock.getFinishedSpans.map(_.drop(spansBefore))
        retrySpan    = spans.find(_.getName == "caliban.gateway.retry")
        attempts     = spans
                         .filter(_.getName == "caliban.gateway.subgraph.attempt")
                         .sortBy(_.getAttributes.get(AttributeKey.longKey("http.request.resend_count")).longValue())
      } yield assertTrue(
        response.errors.isEmpty,
        requests.size == 2,
        retrySpan.nonEmpty,
        attempts.map(_.getAttributes.get(AttributeKey.longKey("http.request.resend_count")).longValue()) == List(
          0L,
          1L
        ),
        headers
          .lift(1)
          .flatMap(_.get("traceparent"))
          .flatMap(_.split('-').lift(2)) == attempts.lift(1).map(_.getSpanId)
      )
    },
    test("marks GraphQL error results without recording error messages") {
      for {
        remote      <- stub("""{"data":null,"errors":[{"message":"private failure"}]}""")
        runtime     <- (Gateway.compose(Subgraph.graphql("products", remote.endpoint, schema)) @@
                         (GatewayMetrics.hooks ++ GatewayTracing.hooks)).interpreter
        spansBefore <- TracingMock.getFinishedSpans.map(_.size)
        response    <- ZIO.serviceWithZIO[Tracing](_.span("caller")(runtime.execute("{ value }")))
        spans       <- TracingMock.getFinishedSpans.map(_.drop(spansBefore))
        observed     = spans.filter(span =>
                         Set(
                           "caliban.gateway.request",
                           "caliban.gateway.subgraph",
                           "caliban.gateway.subgraph.attempt"
                         ).contains(span.getName)
                       )
      } yield assertTrue(
        response.errors.nonEmpty,
        observed.size == 3,
        observed.forall(_.getStatus.getStatusCode == StatusCode.ERROR),
        observed.forall(_.getAttributes.get(AttributeKey.stringKey("error.type")) == "graphql_error"),
        !observed.exists(_.getEvents.asScala.exists(_.getAttributes.toString.contains("private failure")))
      )
    },
    test("keeps trace propagation outside in-flight query identity") {
      for {
        started   <- Promise.make[Nothing, Unit]
        release   <- Promise.make[Nothing, Unit]
        remote    <- stubWith(started.succeed(()).unit *> release.await, """{"data":{"value":"ok"}}""")
        runtime   <- (Gateway.compose(
                       Subgraph.graphql("products", remote.endpoint, schema)
                     ) @@ GatewayTracing.hooks).interpreter
        fibers    <- ZIO.foreach(1 to 2)(index =>
                       ZIO.serviceWithZIO[Tracing](_.span(s"caller-$index")(runtime.execute("{ value }"))).fork
                     )
        _         <- started.await
        _         <- TestClock.adjust(zio.Duration.Zero)
        before    <- remote.requests.get
        _         <- release.succeed(())
        responses <- ZIO.foreach(fibers)(_.join)
        after     <- remote.requests.get
      } yield assertTrue(before.size == 1, after.size == 1, responses.forall(_.errors.isEmpty))
    }
  ).provideSomeShared[Scope](testServer, stubIds, TracingMock.layer) @@ TestAspect.sequential

}
