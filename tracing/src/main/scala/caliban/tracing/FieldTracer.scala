package caliban.tracing

import caliban.CalibanError
import caliban.ResponseValue
import caliban.execution.FieldInfo
import io.opentelemetry.api.trace.StatusCode
import caliban.wrappers.Wrapper.FieldWrapper
import zio._
import zio.query.{ QueryAspect, ZQuery }
import zio.query._
import zio.telemetry.opentelemetry.tracing.{ ErrorMapper, Tracing }

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
            val status = cause.failureOption.flatMap(ErrorMapper.default.body.lift).getOrElse(StatusCode.ERROR)
            ZQuery.fromZIO(ZIO.succeed(span.setStatus(status, cause.prettyPrint))) *> ZQuery.failCause(cause)
          },
          value => ZQuery.succeed(value)
        )
      }
  }
}
