package caliban.reporting

import zio.clock.Clock
import zio.duration.{ durationInt, Duration }
import zio.{ Has, UIO, ZIO, ZLayer, ZManaged }

/**
 * Manages schema reporting on start up, can be provided a set of ReportingRefs which will then be periodically
 * sent to apollo
 */
trait ReportingDaemon {

  /**
   * Returns the list of refs that are being tracked by the daemon
   */
  def refs: UIO[List[SchemaReportingRef]]
}

object ReportingDaemon {

  def layer(
    ref: SchemaReportingRef,
    rest: SchemaReportingRef*
  ): ZLayer[Clock with Has[SchemaReporter], Nothing, Has[ReportingDaemon]] =
    make(ref, rest: _*).toLayer

  def make(
    ref: SchemaReportingRef,
    rest: SchemaReportingRef*
  ): ZManaged[Clock with Has[SchemaReporter], Nothing, ReportingDaemon] =
    ZManaged.suspend {
      val schemaRefs = ref :: rest.toList
      for {
        reporter <- ZManaged.service[SchemaReporter]
        _        <- ZIO
                      .foreachPar_(schemaRefs) { ref =>
                        def loop(withCoreSchema: Boolean): ZIO[Clock, Nothing, Unit] =
                          reporter
                            .report(ref, withCoreSchema)
                            .foldM(
                              {
                                case ReportingError.SchemaError(_, _, message, _)  =>
                                  ZIO.debug(s"Schema reporting failed for ${ref.graphRef}: $message")
                                case ReportingError.ClientError(error)             =>
                                  ZIO.debug(
                                    s"Schema reporting for ${ref.graphRef} failed because of a client error ${error.getMessage}. This is likely a defect, will not retry"
                                  )
                                case ReportingError.RetryableError(innerThrowable) =>
                                  ZIO.debug(
                                    s"Schema reporting encountered an error: ${innerThrowable.getMessage}, retrying in 20 seconds"
                                  ) *> loop(false).delay(20.seconds)
                              },
                              resp => loop(resp.withCoreSchema).delay(resp.in)
                            )
                        loop(false)
                      }
                      .forkManaged
      } yield new ReportingDaemon {

        /**
         * Returns the list of refs that are being tracked by the daemon
         */
        override def refs: UIO[List[SchemaReportingRef]] = UIO.succeed(schemaRefs)
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
