package caliban.tracing

import caliban.CalibanError
import caliban.ResponseValue
import caliban.execution.FieldInfo
import caliban.wrappers.Wrapper.FieldWrapper
import zio._
import zio.query.DataSourceAspect
import zio.query.ZQuery
import zio.query._
import zio.telemetry.opentelemetry.tracing.Tracing

object FieldTracer {
  val wrapper = new FieldWrapper[Tracing] {
    def wrap[R <: Tracing](
      query: ZQuery[R, CalibanError.ExecutionError, ResponseValue],
      info: FieldInfo
    ): ZQuery[R, CalibanError.ExecutionError, ResponseValue] =
      query @@ traced(info.name)
  }

  private def traced[R <: Tracing](name: String) = new DataSourceAspect[R] {
    def apply[R1 <: R, A](dataSource: DataSource[R1, A]): DataSource[R1, A] =
      new DataSource[R1, A] {
        val identifier = s"${dataSource.identifier} @@ traced"

        def runAll(requests: Chunk[Chunk[A]])(implicit trace: zio.Trace): ZIO[R1, Nothing, CompletedRequestMap] =
          ZIO.serviceWithZIO[Tracing](
            _.span(name)(dataSource.runAll(requests))
          )
      }
  }
}
