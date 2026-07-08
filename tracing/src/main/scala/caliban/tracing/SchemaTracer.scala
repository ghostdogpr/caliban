package caliban.tracing

import caliban.InputValue.ObjectValue
import caliban.RemoteQuery
import caliban.Value.FloatValue.FloatNumber
import caliban.Value.IntValue.IntNumber
import caliban.Value.StringValue
import caliban.execution.{ ExecutionRequest, Field }
import caliban.parsing.adt.OperationType
import caliban.wrappers.Wrapper.ExecutionWrapper
import caliban.{ CalibanError, GraphQLResponse, InputValue, Value }
import io.opentelemetry.api.trace.SpanKind
import zio._
import zio.telemetry.opentelemetry.tracing.Tracing

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
          ZIO.serviceWithZIO[Tracing](tracer =>
            tracer.span(
              spanName(request),
              SpanKind.INTERNAL
            ) {
              ZIO.foreachDiscard(attributes(request.field)) { case (k, v) =>
                tracer.setAttribute(k, v)
              } *> f(request)
            }
          )
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

  private def attributes[T, R](
    field: Field
  ) = List("document" -> graphQLQuery(field))

  private def graphQLQuery(field: Field): String =
    RemoteQuery.apply(maskField(field)).toGraphQLRequest.query.getOrElse("")

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

  private def maskField(f: Field): Field =
    f.copy(
      arguments = maskArguments(f.arguments),
      fields = f.fields.map(maskField)
    )
}
