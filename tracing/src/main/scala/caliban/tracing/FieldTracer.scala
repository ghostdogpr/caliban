package caliban.tracing

import caliban.{ CalibanError, ResponseValue }
import caliban.execution.FieldInfo
import caliban.wrappers.Wrapper.FieldWrapper
import io.opentelemetry.api.trace.StatusCode
import zio._
import zio.query.ZQuery
import zio.telemetry.opentelemetry.tracing.{ StatusMapper, Tracing }

object FieldTracer {
  val wrapper = new FieldWrapper[Tracing] {
    def wrap[R <: Tracing](
      query: ZQuery[R, CalibanError.ExecutionError, ResponseValue],
      info: FieldInfo
    ): ZQuery[R, CalibanError.ExecutionError, ResponseValue] =
      ZQuery.acquireReleaseWith(
        ZIO.serviceWithZIO[Tracing](_.spanUnsafe(info.name))
      ) { case (_, end) => end } { case (span, _) =>
        query.foldCauseQuery(
          cause =>
            ZQuery.failCause {
              val status =
                cause.failureOption.flatMap(StatusMapper.default.failure.lift).fold(StatusCode.ERROR)(_.statusCode)
              span.setStatus(status, cause.prettyPrint)
              cause
            },
          value => ZQuery.succeed(value)
        )
      }
  }
}
