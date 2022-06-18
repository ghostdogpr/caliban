package caliban.reporting

import zio._

/**
 * Manages schema reporting on start up, can be provided a set of ReportingRefs which will then be periodically
 * sent to apollo
 */
trait ReportingDaemon {

  /**
   * Registers a schema reference for upload. Returns a managed which can be used to manage the lifespan of the
   * reporting.
   */
  def register(ref: SchemaReportingRef[_])(implicit trace: Trace): ZIO[Scope, Nothing, Unit]
}

object ReportingDaemon {

  def register(ref: SchemaReportingRef[_]): ZIO[ReportingDaemon with Scope, Nothing, Unit] =
    ZIO.serviceWithZIO[ReportingDaemon](_.register(ref))

  def live: ZLayer[SchemaReporter, Nothing, ReportingDaemon] = ZLayer(make)

  def make: ZIO[SchemaReporter, Nothing, ReportingDaemon] =
    ZIO.service[SchemaReporter].map { reporter =>
      new ReportingDaemon {

        override def register(ref: SchemaReportingRef[_])(implicit trace: Trace): ZIO[Scope, Nothing, Unit] = {
          def loop(withCoreSchema: Boolean): ZIO[Any, Nothing, Unit] =
            reporter
              .report(ref, withCoreSchema)
              .foldZIO(
                {
                  case ReportingError.SchemaError(_, _, message, _)  =>
                    ZIO.logError(s"Schema reporting failed for ${ref.graphRef}: $message")
                  case ReportingError.ClientError(error)             =>
                    ZIO.logWarningCause(
                      s"Schema reporting for ${ref.graphRef} failed because of a client error ${error.getMessage}. This is likely a defect, halting retries",
                      Cause.fail(error)
                    )
                  case ReportingError.RetryableError(innerThrowable) =>
                    ZIO.logWarningCause(
                      s"Schema reporting encountered an error: ${innerThrowable.getMessage}, retrying in 20 seconds",
                      Cause.fail(innerThrowable)
                    ) *> loop(false).delay(20.seconds)
                },
                resp => loop(resp.withCoreSchema).delay(resp.in)
              )
          loop(false).forkScoped
        }.unit
      }
    }
}

case class ReportingResponse(
  withCoreSchema: Boolean,
  in: Duration
)

sealed trait ReportingError

object ReportingError {
  case class SchemaError(withCoreSchema: Boolean, code: String, message: String, inSeconds: Duration)
      extends ReportingError
  case class ClientError(innerThrowable: Throwable)    extends ReportingError
  case class RetryableError(innerThrowable: Throwable) extends ReportingError

}
