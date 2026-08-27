package caliban.gateway

import caliban.gateway.GatewayTestSupport._
import caliban.gateway.tracing.GatewayTracing
import caliban.tracing.TracingMock
import io.opentelemetry.api.common.AttributeKey
import io.opentelemetry.api.trace.StatusCode
import zio.Duration
import zio.http.Status
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.{ Exit, Promise, Scope, Trace, ZIO }
import zio.test.{ assertTrue, Spec, TestAspect, TestEnvironment, ZIOSpecDefault }

import scala.jdk.CollectionConverters._

object GatewayTracingSpec extends ZIOSpecDefault {

  private val schema = "type Query { value: String! }"

  def spec: Spec[TestEnvironment with Scope, Any] = suite("Gateway tracing")(
    test("traces a remote request without recording raw GraphQL data and propagates W3C context") {
      for {
        remote        <- stub("""{"data":{"value":"ok"}}""")
        gateway        = (Gateway.compose(
                           Subgraph.graphql("products", remote.endpoint, schema)
                         ) @@ GatewayTracing.wrapper: Gateway[Tracing])
        runtime       <- gateway.interpreter
        response      <- ZIO.serviceWithZIO[Tracing](_.span("caller")(runtime.execute("query PublicName { value }")))
        sentHeaders   <- remote.headers.get
        spans         <- TracingMock.getFinishedSpans
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
    test("traces retry attempts") {
      val config = RemoteGraphQLConfig.default.withExecution(_.withRetries(1, Duration.Zero))

      for {
        remote      <- stubWithStatuses(
                         Status.ServiceUnavailable -> "{}",
                         Status.Ok                 -> """{"data":{"value":"ok"}}"""
                       )
        runtime     <- (Gateway.compose(
                         Subgraph.graphql("products", remote.endpoint, schema, config)
                       ) @@ GatewayTracing.wrapper).interpreter
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
        wrapper      = (GatewayMetrics.wrapper |+| GatewayTracing.wrapper: GatewayWrapper[Tracing])
        runtime     <- (Gateway.compose(Subgraph.graphql("products", remote.endpoint, schema)) @@ wrapper).interpreter
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
        joined    <- Promise.make[Nothing, Unit]
        remote    <- stubWith(started.succeed(()).unit *> release.await, """{"data":{"value":"ok"}}""")
        runtime   <- (Gateway.compose(
                       Subgraph.graphql("products", remote.endpoint, schema)
                     ) @@ (GatewayTracing.wrapper |+| deduplicationObserver(joined))).interpreter
        fibers    <- ZIO.foreach(1 to 2)(index =>
                       ZIO.serviceWithZIO[Tracing](_.span(s"caller-$index")(runtime.execute("{ value }"))).fork
                     )
        _         <- started.await
        _         <- joined.await
        before    <- remote.requests.get
        _         <- release.succeed(())
        responses <- ZIO.foreach(fibers)(_.join)
      } yield assertTrue(before.size == 1, responses.forall(_.errors.isEmpty))
    }
  ).provideSomeShared[Scope](testServer, stubIds, TracingMock.layer) @@ TestAspect.sequential

  private def deduplicationObserver(joined: Promise[Nothing, Unit]): GatewayWrapper[Any] =
    new GatewayWrapper[Any] {
      def wrap[R, E, A](event: GatewayWrapper.Event)(effect: ZIO[R, E, A])(
        result: Exit[E, A] => GatewayWrapper.Result
      )(implicit trace: Trace): ZIO[R, E, A] =
        event match {
          case GatewayWrapper.Event.Deduplication(GatewayWrapper.DeduplicationResult.Join) =>
            joined.succeed(()).unit *> effect
          case _                                                                           => effect
        }
    }
}
