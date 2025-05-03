package caliban.tracing

import caliban.{CalibanError, ResponseValue}
import caliban.execution.FieldInfo
import caliban.wrappers.Wrapper.FieldWrapper
import io.opentelemetry.api.trace.StatusCode
import zio._
import zio.query.ZQuery
import zio.telemetry.opentelemetry.trace.{StatusMapper, Tracer}

object FieldTracer {
  val wrapper = new FieldWrapper[Tracer] {
    def wrap[R <: Tracer](
                           query: ZQuery[R, CalibanError.ExecutionError, ResponseValue],
                           info: FieldInfo
                         ): ZQuery[R, CalibanError.ExecutionError, ResponseValue] =
      ZQuery.fromZIO(ZIO.service[Tracer]).flatMap { tracer =>
        // Use ZQuery.foldCauseQuery to handle success and failure cases
        query.foldCauseQuery(
          cause => {
            val status = cause.failureOption
              .flatMap(StatusMapper.default.failure.lift)
              .fold(StatusCode.ERROR)(_.statusCode)

            // Create a new query that logs the error and then fails with the original cause
            ZQuery.fromZIO(
              ZIO.logError(s"Error in field ${info.name}: ${cause.prettyPrint} (Status: $status)")
            ) *> ZQuery.failCause(cause)
          },
          value => ZQuery.succeed(value)
        )
      }
  }
}
