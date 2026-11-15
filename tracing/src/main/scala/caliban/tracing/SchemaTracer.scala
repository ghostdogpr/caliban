package caliban.tracing

import caliban.InputValue.ObjectValue
import caliban.RemoteQuery
import caliban.rendering.DocumentRenderer
import caliban.Value.FloatValue.FloatNumber
import caliban.Value.IntValue.IntNumber
import caliban.Value.StringValue
import caliban.execution.ExecutionRequest
import caliban.parsing.adt.OperationType
import caliban.wrappers.Wrapper.ExecutionWrapper
import caliban.{ CalibanError, GraphQLResponse, IncomingRequestHeaders, InputValue, Value }
import io.opentelemetry.api.trace.propagation.W3CTraceContextPropagator
import io.opentelemetry.api.trace.{ Span, SpanKind }
import io.opentelemetry.context.Context
import io.opentelemetry.context.propagation.{ TextMapGetter, TextMapPropagator, TextMapSetter }
import zio._
import zio.telemetry.opentelemetry.context.IncomingContextCarrier
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.telemetry.opentelemetry.tracing.propagation.TraceContextPropagator

import java.util.Locale
import scala.collection.mutable

object SchemaTracer {
  val wrapper: ExecutionWrapper[Tracing] = new ExecutionWrapper[Tracing] {
    def wrap[R <: Tracing](
      f: ExecutionRequest => ZIO[R, Nothing, GraphQLResponse[CalibanError]]
    ): ExecutionRequest => ZIO[R, Nothing, GraphQLResponse[CalibanError]] =
      request => {
        val parentField = request.field.fields.headOption.map(_.name)

        // skip introspection queries
        if (parentField.contains("__schema")) f(request)
        else
          ZIO.serviceWithZIO[Tracing] { tracer =>
            val name   = spanName(request)
            val traced = tracer.setAttribute("document", graphQLQuery(request)) *> f(request)
            tracer.getCurrentContextUnsafe.flatMap { current =>
              if (Span.fromContext(current).getSpanContext.isValid) tracer.span(name, SpanKind.INTERNAL)(traced)
              else
                IncomingRequestHeaders.get.flatMap { headers =>
                  val carrier = mutable.Map.empty[String, String]
                  headers.foreach { case (k, v) =>
                    val key = k.toLowerCase(Locale.ROOT)
                    carrier(key) = carrier.get(key).fold(v)(_ + "," + v)
                  }
                  tracer.extractSpan(
                    continuing(current),
                    IncomingContextCarrier.default(carrier),
                    name,
                    SpanKind.INTERNAL
                  )(traced)
                }
            }
          }
      }
  }

  // extractSpan extracts against Context.root(), which would drop the current baggage
  private def continuing(current: Context): TraceContextPropagator =
    new TraceContextPropagator {
      private val w3c                 = W3CTraceContextPropagator.getInstance()
      val instance: TextMapPropagator = new TextMapPropagator {
        def fields(): java.util.Collection[String]                                      = w3c.fields()
        def inject[C](context: Context, carrier: C, setter: TextMapSetter[C]): Unit     =
          w3c.inject(context, carrier, setter)
        def extract[C](context: Context, carrier: C, getter: TextMapGetter[C]): Context =
          w3c.extract(current, carrier, getter)
      }
    }

  private def spanName(request: ExecutionRequest): String = {
    val operationTypeString = request.operationType match {
      case OperationType.Query        => "query"
      case OperationType.Mutation     => "mutation"
      case OperationType.Subscription => "subscription"
    }

    // The span name MUST be of the format <graphql.operation.type> <graphql.operation.name>
    // provided that graphql.operation.type and graphql.operation.name are available. If
    // graphql.operation.name is not available, the span SHOULD be named <graphql.operation.type>.
    // When <graphql.operation.type> is not available, GraphQL Operation MAY be used as span name.
    Seq(Some(operationTypeString), request.operationName).flatten.mkString(" ")
  }

  private def graphQLQuery(request: ExecutionRequest): String =
    DocumentRenderer.renderCompact(RemoteQuery.operationDocument(request.operationType, request.field, maskArguments))

  private[tracing] def maskArguments(args: Map[String, InputValue]): Map[String, InputValue] =
    args.map { case (k, v) => (k, maskValue(v)) }.toMap

  private def maskValue(v: InputValue): InputValue =
    v match {
      case _: ObjectValue               => ObjectValue(Map.empty)
      case InputValue.ListValue(values) => InputValue.ListValue(values.map(maskValue))
      case _: StringValue               => StringValue("")
      case _: Value.IntValue            => IntNumber(0)
      case _: Value.FloatValue          => FloatNumber(0f)
      case _: Value.BooleanValue        => Value.BooleanValue(false)
      case _: Value.EnumValue           => Value.EnumValue("__REDACTED")
      case x                            => x
    }
}
