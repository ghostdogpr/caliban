package caliban.gateway.tracing

import caliban.IncomingRequestHeaders
import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayWrapper.{ Event, Outcome, Result }
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.api.trace.{ SpanKind, StatusCode }
import sttp.model.Header
import zio.telemetry.opentelemetry.context.{ IncomingContextCarrier, OutgoingContextCarrier }
import zio.telemetry.opentelemetry.tracing.{ StatusMapper, Tracing }
import zio.telemetry.opentelemetry.tracing.propagation.TraceContextPropagator
import zio.{ Exit, Trace, URIO, ZIO }

import java.util.Locale
import scala.collection.mutable

/**
 * OpenTelemetry integration for a Caliban gateway.
 */
object GatewayTracing {
  val wrapper: GatewayWrapper[Tracing] = new GatewayWrapper[Tracing] {
    private val propagation = TraceContextPropagator.default

    def wrap[R0 <: Tracing, E, A](event: GatewayWrapper.Event)(effect: ZIO[R0, E, A])(
      result: Exit[E, A] => Result
    )(implicit trace: Trace): ZIO[R0, E, A] = {
      def observed = effect.onExit(exit => complete(event, result(exit)))
      event match {
        case Event.SubscriptionSetup                                                                       => request(None, "caliban.gateway.subscription.setup")(observed)
        // Reuse incoming or ambient context, not the finished setup span; without either, each event starts a trace.
        case Event.SubscriptionEvent                                                                       => request(None, "caliban.gateway.subscription.event", SpanKind.INTERNAL)(observed)
        case _: Event.SubscriptionTerminated | _: Event.SubscriptionAdmission | Event.SubscriptionOverflow =>
          effect
        case Event.Request(operationName)                                                                  => request(operationName)(observed)
        case Event.Routing                                                                                 => span("caliban.gateway.routing", SpanKind.INTERNAL)(observed)
        case Event.SubgraphCall(subgraph, operationType)                                                   =>
          span(
            "caliban.gateway.subgraph",
            SpanKind.INTERNAL,
            Attributes
              .builder()
              .put("graphql.subgraph.name", subgraph)
              .put("graphql.operation.type", GatewayWrapper.operationTypeLabel(operationType))
              .build()
          )(observed)
        case attempt: Event.Attempt                                                                        =>
          val attributes = Attributes
            .builder()
            .put("graphql.subgraph.name", attempt.subgraph)
            .put("http.request.method", "POST")
            .put("http.request.body.size", attempt.requestBytes)
            .put("http.request.resend_count", attempt.number.toLong)
          attempt.serverAddress.foreach(attributes.put("server.address", _))
          attempt.serverPort.foreach(port => attributes.put("server.port", port.toLong))
          span("caliban.gateway.subgraph.attempt", SpanKind.CLIENT, attributes.build())(observed)
        case Event.Retry(subgraph, attempt)                                                                =>
          span(
            "caliban.gateway.retry",
            SpanKind.INTERNAL,
            Attributes
              .builder()
              .put("graphql.subgraph.name", subgraph)
              .put("caliban.gateway.retry.attempt", attempt.toLong)
              .build()
          )(observed)
        case Event.Completion                                                                              => span("caliban.gateway.completion", SpanKind.INTERNAL)(observed)
        case _: Event.CacheAccess | _: Event.AdmissionWait | _: Event.Admission | _: Event.Deduplication |
            Event.RequestOverdue =>
          effect
      }
    }

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
            case _: Event.Request                                  =>
              result.operationType.foreach(operationType =>
                attributes.put("graphql.operation.type", GatewayWrapper.operationTypeLabel(operationType))
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

    override def attemptHeaders(subgraph: String, attempt: Int, headers: List[Header])(implicit
      trace: Trace
    ): URIO[Tracing, List[Header]] =
      for {
        tracing <- ZIO.service[Tracing]
        values  <- ZIO.succeed(mutable.LinkedHashMap.empty[String, String])
        carrier  = new OutgoingContextCarrier[mutable.LinkedHashMap[String, String]] {
                     val kernel: mutable.LinkedHashMap[String, String] = values

                     def set(
                       carrier: mutable.LinkedHashMap[String, String],
                       key: String,
                       value: String
                     ): Unit = carrier.update(normalize(key), value)
                   }
        _       <- tracing.injectSpan(propagation, carrier)
        names    = values.keySet
      } yield headers.filterNot(header => names.contains(normalize(header.name))) :::
        values.iterator.map { case (name, value) => Header(name, value) }.toList

    private def request[R, E, A](
      operationName: Option[String],
      spanName: String = "caliban.gateway.request",
      kind: SpanKind = SpanKind.SERVER
    )(
      effect: ZIO[R, E, A]
    )(implicit
      trace: Trace
    ): ZIO[R with Tracing, E, A] =
      IncomingRequestHeaders.get.flatMap { headers =>
        val normalized = mutable.Map(headers.iterator.map { case (name, value) => normalize(name) -> value }.toSeq: _*)
        val attributes = operationName.fold(Attributes.empty())(name =>
          Attributes.builder().put("graphql.operation.name", name).build()
        )
        if (normalized.contains("traceparent")) {
          val carrier = IncomingContextCarrier.default(normalized)
          ZIO.serviceWithZIO[Tracing](
            _.extractSpan(
              propagation,
              carrier,
              spanName,
              kind,
              attributes,
              failureStatus
            )(effect)
          )
        } else span(spanName, kind, attributes)(effect)
      }

    private def span[R, E, A](
      name: String,
      kind: SpanKind,
      attributes: Attributes = Attributes.empty()
    )(effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R with Tracing, E, A] =
      ZIO.serviceWithZIO[Tracing](_.span(name, kind, attributes, failureStatus)(effect))

    private val failureStatus = StatusMapper.failureNoException[Any](_ => StatusCode.ERROR)

    private def normalize(value: String): String = value.toLowerCase(Locale.ROOT)
  }
}
