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
import zio.{ Exit, Trace, UIO, URIO, ZIO }

import java.util.Locale
import scala.collection.mutable

/**
 * OpenTelemetry integration for a Caliban gateway.
 */
object GatewayTracing {
  val wrapper: GatewayWrapper[Tracing] = new GatewayWrapper[Tracing] {
    private val propagation = TraceContextPropagator.default

    def wrap[R0, E, A](event: GatewayWrapper.Event)(effect: ZIO[R0, E, A])(
      result: Exit[E, A] => Result
    )(implicit trace: Trace): ZIO[Tracing with R0, E, A] = {
      val observed = effect.onExit(exit => complete(event, result(exit)))
      event match {
        case Event.Request(operationName)            => request(operationName)(observed)
        case Event.Routing                           => span("caliban.gateway.routing", SpanKind.INTERNAL)(observed)
        case Event.SourceCall(source, operationType) =>
          span(
            "caliban.gateway.source",
            SpanKind.INTERNAL,
            Attributes
              .builder()
              .put("graphql.subgraph.name", source)
              .put("graphql.operation.type", GatewayWrapper.operationTypeLabel(operationType))
              .build()
          )(observed)
        case attempt: Event.Attempt                  =>
          val attributes = Attributes
            .builder()
            .put("graphql.subgraph.name", attempt.source)
            .put("http.request.method", "POST")
            .put("http.request.body.size", attempt.requestBytes)
            .put("http.request.resend_count", attempt.number.toLong)
          attempt.serverAddress.foreach(attributes.put("server.address", _))
          attempt.serverPort.foreach(port => attributes.put("server.port", port.toLong))
          span("caliban.gateway.source.attempt", SpanKind.CLIENT, attributes.build())(observed)
        case Event.Retry(source, attempt)            =>
          span(
            "caliban.gateway.retry",
            SpanKind.INTERNAL,
            Attributes
              .builder()
              .put("graphql.subgraph.name", source)
              .put("caliban.gateway.retry.attempt", attempt.toLong)
              .build()
          )(observed)
        case Event.Completion                        => span("caliban.gateway.completion", SpanKind.INTERNAL)(observed)
        case _: Event.CacheAccess | _: Event.AdmissionWait | _: Event.Admission | _: Event.Deduplication |
            Event.RequestOverdue =>
          effect
      }
    }

    private def complete(event: Event, result: Result)(implicit trace: Trace): URIO[Tracing, Unit] =
      ZIO.serviceWithZIO[Tracing] { tracing =>
        val attributes = event match {
          case _: Event.Request    =>
            result.operationType.fold[UIO[Unit]](ZIO.unit)(operationType =>
              tracing.setAttribute("graphql.operation.type", GatewayWrapper.operationTypeLabel(operationType))
            ) *>
              tracing.setAttribute("graphql.response.error.count", result.errorCount.toLong) *>
              tracing.setAttribute("caliban.gateway.request.outcome", result.outcome.label)
          case _: Event.SourceCall =>
            tracing.setAttribute("graphql.response.error.count", result.errorCount.toLong) *>
              tracing.setAttribute("caliban.gateway.source.outcome", result.outcome.label)
          case _: Event.Attempt    =>
            result.statusCode.fold[UIO[Unit]](ZIO.unit)(code =>
              tracing.setAttribute("http.response.status_code", code.toLong)
            ) *>
              result.responseBytes.fold[UIO[Unit]](ZIO.unit)(bytes =>
                tracing.setAttribute("http.response.body.size", bytes)
              ) *>
              tracing.setAttribute("caliban.gateway.source.attempt.outcome", result.outcome.label)
          case _                   => ZIO.unit
        }

        attributes *>
          ZIO.whenDiscard(result.outcome != Outcome.Success) {
            tracing.setAttribute("error.type", result.outcome.label) *>
              tracing.getCurrentSpanUnsafe.flatMap(span => ZIO.succeed(span.setStatus(StatusCode.ERROR)))
          }
      }

    override def attemptHeaders(source: String, attempt: Int, headers: List[Header])(implicit
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

    private def request[R, E, A](operationName: Option[String])(effect: ZIO[R, E, A])(implicit
      trace: Trace
    ): ZIO[R with Tracing, E, A] =
      IncomingRequestHeaders.get.flatMap { headers =>
        val carrier    = IncomingContextCarrier.default(
          mutable.Map(headers.iterator.map { case (name, value) => normalize(name) -> value }.toSeq: _*)
        )
        val attributes = operationName.fold(Attributes.empty())(name =>
          Attributes.builder().put("graphql.operation.name", name).build()
        )
        ZIO.serviceWithZIO[Tracing](
          _.extractSpan(
            propagation,
            carrier,
            "caliban.gateway.request",
            SpanKind.SERVER,
            attributes,
            failureStatus
          )(effect)
        )
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
