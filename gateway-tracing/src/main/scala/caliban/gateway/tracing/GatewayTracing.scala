package caliban.gateway.tracing

import caliban.IncomingRequestHeaders
import caliban.gateway.PhaseHooks.{ Event, Outcome, Result }
import caliban.gateway.{ OperationEvent, PhaseHandler, PhaseHooks, RemoteGraphQLConfig }
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.api.trace.{ SpanKind, StatusCode }
import zio.http.Header
import zio.telemetry.opentelemetry.context.{ IncomingContextCarrier, OutgoingContextCarrier }
import zio.telemetry.opentelemetry.tracing.propagation.TraceContextPropagator
import zio.telemetry.opentelemetry.tracing.{ StatusMapper, Tracing }
import zio.{ Scope, Trace, URIO, ZIO }

import scala.collection.mutable

/**
 * OpenTelemetry integration for a Caliban gateway.
 *
 * Attach [[hooks]] with `Gateway.compose(...) @@ GatewayTracing.hooks`
 */
object GatewayTracing {
  private val propagation = TraceContextPropagator.default

  /**
   * The spans this integration records, as phase hooks.
   *
   * The request-level SERVER span hangs off `observeOperation`, the outermost phase, so routing and the operation cache
   * fall inside it rather than beside it. Some hooks carry no span of their own and are left untouched: the request
   * phase itself, cache access, admission, subscription admission, termination and overflow, override labels and
   * outbound headers.
   */
  val hooks: PhaseHooks[Tracing] =
    PhaseHooks.observeOperation(
      spanningWith[Event.ObserveOperation, OperationEvent](
        contextual = true,
        "caliban.gateway.request",
        SpanKind.SERVER,
        event =>
          event.request.operationName.fold(Attributes.empty())(name =>
            Attributes.builder().put("graphql.operation.name", name).build()
          ),
        event => Result(event.outcome, event.operationType, event.errors.size)
      )
    ) ++
      PhaseHooks.subscriptionSetup(spanning(contextual = true, "caliban.gateway.subscription.setup")) ++
      // Reuse incoming or ambient context, not the finished setup span; without either, each event starts a trace.
      PhaseHooks.subscriptionEvent(
        spanning(contextual = true, "caliban.gateway.subscription.event", SpanKind.INTERNAL)
      ) ++
      PhaseHooks.routing(spanning(contextual = false, "caliban.gateway.routing", SpanKind.INTERNAL)) ++
      PhaseHooks.subgraphCall(
        spanning[Event.SubgraphCall](
          contextual = false,
          "caliban.gateway.subgraph",
          SpanKind.INTERNAL,
          event =>
            Attributes
              .builder()
              .put("graphql.subgraph.name", event.subgraph)
              .put("graphql.operation.type", PhaseHooks.operationTypeLabel(event.operationType))
              .build()
        )
      ) ++
      PhaseHooks.attempt(
        spanning[Event.Attempt](
          contextual = false,
          "caliban.gateway.subgraph.attempt",
          SpanKind.CLIENT,
          event => {
            val attributes = Attributes
              .builder()
              .put("graphql.subgraph.name", event.subgraph)
              .put("http.request.method", "POST")
              .put("http.request.body.size", event.requestBytes)
              .put("http.request.resend_count", event.number.toLong)
            event.serverAddress.foreach(attributes.put("server.address", _))
            event.serverPort.foreach(port => attributes.put("server.port", port.toLong))
            attributes.build()
          }
        )
      ) ++
      PhaseHooks.retry(
        spanning[Event.Retry](
          contextual = false,
          "caliban.gateway.retry",
          SpanKind.INTERNAL,
          event =>
            Attributes
              .builder()
              .put("graphql.subgraph.name", event.subgraph)
              .put("caliban.gateway.retry.attempt", event.attempt.toLong)
              .build()
        )
      ) ++
      PhaseHooks.completion(spanning(contextual = false, "caliban.gateway.completion", SpanKind.INTERNAL)) ++
      PhaseHooks.attemptHeaders(
        PhaseHandler.incoming(event => propagatedHeaders(event.headers).map(headers => event.copy(headers = headers)))
      )

  /** Opens a span around one phase whose outgoing value is already a [[PhaseHooks.Result]]. */
  private def spanning[Ev <: Event](
    contextual: Boolean,
    name: String,
    kind: SpanKind = SpanKind.SERVER,
    attributes: Ev => Attributes = (_: Ev) => Attributes.empty()
  ): PhaseHandler[Tracing, Ev, Nothing, Result] =
    spanningWith[Ev, Result](contextual, name, kind, attributes, (result: Result) => result)

  /**
   * Opens a span around one phase and records that phase's own outcome on it before the span closes.
   *
   * The span has to enclose the phase effect, which is what a [[PhaseHandler]] with both an incoming and an outgoing
   * side is for; the cheaper incoming-only handlers cannot express it. `result` adapts hooks that report something
   * other than a [[PhaseHooks.Result]], such as the [[OperationEvent]] of `observeOperation`.
   */
  private def spanningWith[Ev <: Event, Res](
    contextual: Boolean,
    name: String,
    kind: SpanKind,
    attributes: Ev => Attributes,
    result: Res => Result
  ): PhaseHandler[Tracing, Ev, Nothing, Res] =
    PhaseHandler.scoped(
      PhaseHandler((event: Ev) => traced(contextual, name, kind, attributes(event)).map(event -> _))(
        (event, _, out: Res) => complete(event, result(out))
      )
    )

  /**
   * Continues the caller's trace when the request carries one, and otherwise opens an ordinary span.
   */
  private def traced(contextual: Boolean, name: String, kind: SpanKind, attributes: Attributes)(implicit
    trace: Trace
  ): ZIO[Scope with Tracing, Nothing, Unit] =
    if (!contextual) span(name, kind, attributes)
    else
      IncomingRequestHeaders.get.flatMap { headers =>
        val lowercased =
          mutable.Map(headers.map { case (name, value) => RemoteGraphQLConfig.lowercaseHeaderName(name) -> value }: _*)
        if (lowercased.contains("traceparent")) continuedSpan(lowercased, name, kind, attributes)
        else span(name, kind, attributes)
      }

  private def continuedSpan(headers: mutable.Map[String, String], name: String, kind: SpanKind, attributes: Attributes)(
    implicit trace: Trace
  ): ZIO[Scope with Tracing, Nothing, Unit] =
    for {
      tracing       <- ZIO.service[Tracing]
      spanAndCloser <-
        tracing.extractSpanUnsafe(propagation, IncomingContextCarrier.default(headers), name, kind, attributes)
      (span, closer) = spanAndCloser
      _             <- ZIO.addFinalizerExit {
                         _.foldExit(
                           cause => {
                             span.setStatus(StatusCode.ERROR, cause.prettyPrint)
                             closer
                           },
                           _ => closer
                         )
                       }
    } yield ()

  private def span(name: String, kind: SpanKind, attributes: Attributes)(implicit
    trace: Trace
  ): ZIO[Scope with Tracing, Nothing, Unit] =
    ZIO.serviceWithZIO[Tracing](_.spanScoped(name, kind, attributes, failureStatus))

  private val failureStatus = StatusMapper.failureNoException[Any](_ => StatusCode.ERROR)

  private def complete(event: Event, result: Result)(implicit trace: Trace): URIO[Tracing, Unit] =
    ZIO.serviceWithZIO[Tracing] { tracing =>
      tracing.getCurrentSpanUnsafe.flatMap { span =>
        val attributes = Attributes.builder()

        event match {
          case Event.SubscriptionSetup | Event.SubscriptionEvent =>
            attributes
              .put("graphql.operation.type", "subscription")
              .put("graphql.response.error.count", result.errorCount.toLong)
              .put("caliban.gateway.subscription.outcome", result.outcome.label)
          case _: Event.ObserveOperation                         =>
            result.operationType.foreach(operationType =>
              attributes.put("graphql.operation.type", PhaseHooks.operationTypeLabel(operationType))
            )
            attributes
              .put("graphql.response.error.count", result.errorCount.toLong)
              .put("caliban.gateway.request.outcome", result.outcome.label)
          case _: Event.SubgraphCall                             =>
            attributes
              .put("graphql.response.error.count", result.errorCount.toLong)
              .put("caliban.gateway.subgraph.outcome", result.outcome.label)
          case _: Event.Attempt                                  =>
            result.statusCode.foreach(code => attributes.put("http.response.status_code", code.toLong))
            result.responseBytes.foreach(bytes => attributes.put("http.response.body.size", bytes))
            attributes.put("caliban.gateway.subgraph.attempt.outcome", result.outcome.label)
          case _                                                 => ()
        }

        if (result.outcome != Outcome.Success) {
          attributes.put("error.type", result.outcome.label)
          span.setStatus(StatusCode.ERROR)
        }

        ZIO.succeed(span.setAllAttributes(attributes.build()))
      }
    }

  /**
   * Replaces any client-supplied propagation headers with this span's own, so trace context never leaks in from the
   * caller and never participates in in-flight query identity.
   */
  private def propagatedHeaders(headers: List[Header])(implicit trace: Trace): URIO[Tracing, List[Header]] =
    for {
      tracing <- ZIO.service[Tracing]
      values  <- ZIO.succeed(mutable.LinkedHashMap.empty[String, String])
      carrier  = new OutgoingContextCarrier[mutable.LinkedHashMap[String, String]] {
                   val kernel: mutable.LinkedHashMap[String, String] = values

                   def set(
                     carrier: mutable.LinkedHashMap[String, String],
                     key: String,
                     value: String
                   ): Unit = carrier.update(RemoteGraphQLConfig.lowercaseHeaderName(key), value)
                 }
      _       <- tracing.injectSpan(propagation, carrier)
      names    = values.keySet
    } yield headers.filterNot(header => names.contains(RemoteGraphQLConfig.lowercaseHeaderName(header.headerName))) :::
      values.iterator.map { case (name, value) => Header.Custom(name, value) }.toList
}
