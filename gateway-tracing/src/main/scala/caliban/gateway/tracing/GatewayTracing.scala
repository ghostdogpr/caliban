package caliban.gateway.tracing

import caliban.IncomingRequestHeaders
import caliban.gateway.PhaseHooks.{ Event, Outcome, Result }
import caliban.gateway.{ OperationEvent, PhaseHandler, PhaseHooks, RemoteGraphQLConfig }
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.api.trace.{ Span, SpanKind, StatusCode }
import io.opentelemetry.context.Context
import zio.http.Header
import zio.telemetry.opentelemetry.context.{ IncomingContextCarrier, OutgoingContextCarrier }
import zio.telemetry.opentelemetry.tracing.propagation.TraceContextPropagator
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.{ Trace, UIO, URIO, ZIO }

import scala.collection.mutable

/**
 * OpenTelemetry integration for a Caliban gateway.
 *
 * Attach [[hooks]] with `Gateway.compose(...) @@ GatewayTracing.hooks`
 */
object GatewayTracing {
  private val propagation       = TraceContextPropagator.default
  private val propagationFields = propagation.instance.fields()

  /**
   * Phase hooks that record a span for each phase of a gateway operation.
   *
   * Each operation gets a `caliban.gateway.request` span, which continues the client's trace when the request carries
   * a `traceparent` header. Preparation, completion, each subgraph call and each HTTP attempt get a child span, and
   * subgraph requests carry the trace context in their headers. Subscriptions get a span for their setup and for each
   * event.
   */
  val hooks: PhaseHooks[Tracing] =
    PhaseHooks.operation(
      spanning[Event.Operation, OperationEvent](
        contextual = true,
        "caliban.gateway.request",
        SpanKind.SERVER,
        _.result,
        event =>
          event.request.operationName.fold(Attributes.empty())(name =>
            Attributes.builder().put("graphql.operation.name", name).build()
          )
      )
    ) ++
      PhaseHooks.subscriptionSetup(
        spanning(contextual = true, "caliban.gateway.subscription.setup", SpanKind.SERVER, identity)
      ) ++
      // Reuse incoming or ambient context, not the finished setup span; without either, each event starts a trace.
      PhaseHooks.subscriptionEvent(
        spanning(contextual = true, "caliban.gateway.subscription.event", SpanKind.INTERNAL, identity)
      ) ++
      PhaseHooks.preparation(
        spanning(contextual = false, "caliban.gateway.preparation", SpanKind.INTERNAL, identity)
      ) ++
      PhaseHooks.subgraphCall(
        spanning[Event.SubgraphCall, Result](
          contextual = false,
          "caliban.gateway.subgraph",
          SpanKind.INTERNAL,
          identity,
          event =>
            Attributes
              .builder()
              .put("graphql.subgraph.name", event.subgraph)
              .put("graphql.operation.type", PhaseHooks.operationTypeLabel(event.operationType))
              .build()
        )
      ) ++
      PhaseHooks.attempt(
        spanning[Event.Attempt, Result](
          contextual = false,
          "caliban.gateway.subgraph.attempt",
          SpanKind.CLIENT,
          identity,
          event => {
            val attributes = Attributes
              .builder()
              .put("graphql.subgraph.name", event.subgraph)
              .put("http.request.method", event.method)
              .put("http.request.body.size", event.requestBytes)
            if (event.number > 0) attributes.put("http.request.resend_count", event.number.toLong)
            event.endpoint.host.foreach(attributes.put("server.address", _))
            event.endpoint.portOrDefault.foreach(port => attributes.put("server.port", port.toLong))
            attributes.build()
          },
          (event, span) => event.copy(headers = propagatedHeaders(event.headers, span))
        )
      ) ++
      PhaseHooks.completion(spanning(contextual = false, "caliban.gateway.completion", SpanKind.INTERNAL, identity))

  /**
   * Opens a span around one phase and records that phase's own outcome on it before the span closes.
   *
   * The span has to enclose the phase effect, which is what a [[PhaseHandler]] with both an incoming and an outgoing
   * side is for; the cheaper incoming-only handlers cannot express it.
   */
  private def spanning[Ev, Res](
    contextual: Boolean,
    name: String,
    kind: SpanKind,
    result: Res => Result,
    attributes: Ev => Attributes = (_: Ev) => Attributes.empty(),
    enter: (Ev, Span) => Ev = (event: Ev, _: Span) => event
  ): PhaseHandler[Tracing, Ev, Res] = {
    val outcomeName = s"$name.outcome"
    PhaseHandler { (event: Ev) =>
      open(contextual, name, kind, attributes(event)).map(opened => enter(event, opened._1) -> opened).uninterruptible
    } { case (_, (span, end), out) =>
      ZIO.succeed {
        val phase      = result(out)
        val attributes = Attributes
          .builder()
          .put("graphql.response.error.count", phase.errorCount.toLong)
          .put(outcomeName, phase.outcome.label)
        phase.operationType.foreach(t => attributes.put("graphql.operation.type", PhaseHooks.operationTypeLabel(t)))
        phase.statusCode.foreach(code => attributes.put("http.response.status_code", code.toLong))
        phase.responseBytes.foreach(attributes.put("http.response.body.size", _))
        if (phase.outcome != Outcome.Success) {
          attributes.put("error.type", phase.outcome.label)
          span.setStatus(StatusCode.ERROR)
        }
        span.setAllAttributes(attributes.build())
      }.ensuring(end).unit
    }
  }

  /**
   * Continues the caller's trace when the request carries one, and otherwise opens an ordinary span.
   */
  private def open(contextual: Boolean, name: String, kind: SpanKind, attributes: Attributes)(implicit
    trace: Trace
  ): URIO[Tracing, (Span, UIO[Any])] =
    for {
      tracing <- ZIO.service[Tracing]
      parent  <- if (contextual) IncomingRequestHeaders.get.map(incomingContext) else ZIO.none
      opened  <- parent.fold(tracing.spanUnsafe(name, kind, attributes))(
                   tracing.extractSpanUnsafe(propagation, _, name, kind, attributes)
                 )
    } yield opened

  private def incomingContext(
    headers: List[(String, String)]
  ): Option[IncomingContextCarrier[mutable.Map[String, String]]] = {
    val lowercased =
      mutable.Map(headers.map { case (name, value) => RemoteGraphQLConfig.lowercaseHeaderName(name) -> value }: _*)
    if (lowercased.contains("traceparent")) Some(IncomingContextCarrier.default(lowercased)) else None
  }

  /**
   * Replaces any client-supplied propagation headers with this span's own, so trace context never leaks in from the
   * caller and never participates in in-flight query identity.
   */
  private def propagatedHeaders(headers: List[Header], span: Span): List[Header] = {
    val carrier = OutgoingContextCarrier.default(mutable.LinkedHashMap.empty)
    propagation.instance.inject(span.storeInContext(Context.root()), carrier.kernel, carrier)
    headers.filterNot(header =>
      propagationFields.contains(RemoteGraphQLConfig.lowercaseHeaderName(header.headerName))
    ) :::
      carrier.kernel.iterator.map { case (name, value) => Header.Custom(name, value) }.toList
  }
}
