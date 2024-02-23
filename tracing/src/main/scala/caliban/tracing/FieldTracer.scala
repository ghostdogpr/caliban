package caliban.tracing

import caliban.execution.FieldInfo
import caliban.wrappers.Wrapper.FieldWrapper
import caliban.{ CalibanError, ResponseValue }
import io.opentelemetry.api.trace.StatusCode
import zio._
import zio.query.ZQuery
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.telemetry.opentelemetry.tracing.{ StatusMapper, Tracing }

object FieldTracer {
  val wrapper = new FieldWrapper[Tracing] {
    def wrap[R <: Tracing](
      query: ZQuery[R, CalibanError.ExecutionError, ResponseValue],
      info: FieldInfo
    ): ZQuery[R, CalibanError.ExecutionError, ResponseValue] =
      ZQuery.acquireReleaseWith(
        ZIO.serviceWithZIO[Tracing](_.spanUnsafe(info.name))
      ) { case (span, end) => end } { case (span, _) =>
        query.foldCauseQuery(
          cause => {
            val status =
              cause.failureOption.flatMap(StatusMapper.default.failure.lift).fold(StatusCode.ERROR)(_.statusCode)
            ZQuery.fromZIO(ZIO.succeed(span.setStatus(status, cause.prettyPrint))) *> ZQuery.failCause(cause)
          },
          value => ZQuery.succeed(value)
        )
      }
  }
}
