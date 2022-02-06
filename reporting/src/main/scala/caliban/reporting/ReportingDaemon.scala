package caliban.reporting

import zio.clock.Clock
import zio.duration.{ durationInt, Duration }
import zio.{ Has, UManaged, URManaged, ZIO, ZLayer, ZManaged }

/**
 * Manages schema reporting on start up, can be provided a set of ReportingRefs which will then be periodically
 * sent to apollo
 */
trait ReportingDaemon {

  /**
   * Registers a schema reference for upload. Returns a managed which can be used to manage the lifespan of the
   * reporting.
   */
  def register(ref: SchemaReportingRef[_]): UManaged[Unit]
}

object ReportingDaemon {

  def register(ref: SchemaReportingRef[_]): URManaged[Has[ReportingDaemon], Unit] =
    ZManaged.serviceWithManaged(_.register(ref))

  def live: ZLayer[Clock with Has[SchemaReporter], Nothing, Has[ReportingDaemon]] =
    make.toLayer

  def make: ZManaged[Clock with Has[SchemaReporter], Nothing, ReportingDaemon] =
    ZManaged.suspend {
      for {
        clock    <- ZManaged.environment[Clock]
        reporter <- ZManaged.service[SchemaReporter]
        daemon    = new ReportingDaemon {

                      override def register(ref: SchemaReportingRef[_]): UManaged[Unit] = {
                        def loop(withCoreSchema: Boolean): ZIO[Clock, Nothing, Unit] =
                          reporter
                            .report(ref, withCoreSchema)
                            .foldM(
                              {
                                case ReportingError.SchemaError(_, _, message, _)  =>
                                  ZIO.debug(s"Schema reporting failed for ${ref.graphRef}: $message")
                                case ReportingError.ClientError(error)             =>
                                  ZIO.debug(
                                    s"Schema reporting for ${ref.graphRef} failed because of a client error ${error.getMessage}. This is likely a defect, halting retries"
                                  )
                                case ReportingError.RetryableError(innerThrowable) =>
                                  ZIO.debug(
                                    s"Schema reporting encountered an error: ${innerThrowable.getMessage}, retrying in 20 seconds"
                                  ) *> loop(false).delay(20.seconds)
                              },
                              resp => loop(resp.withCoreSchema).delay(resp.in)
                            )
                        loop(false).forkManaged
                      }.unit.provide(clock)
                    }
      } yield daemon
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
