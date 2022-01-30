package caliban.reporting

import zio.test.mock
import zio.test.mock.Mock
import zio.{ Has, IO, URLayer, ZIO }

object MockSchemaReporter extends Mock[Has[SchemaReporter]] {
  object Report extends Effect[(SchemaReportingRef, Boolean), ReportingError, ReportingResponse]

  val compose: URLayer[Has[mock.Proxy], Has[SchemaReporter]] = ZIO
    .service[mock.Proxy]
    .map(proxy =>
      new SchemaReporter {
        override def report(ref: SchemaReportingRef, withCoreSchema: Boolean): IO[ReportingError, ReportingResponse] =
          proxy(Report, ref, withCoreSchema)
      }
    )
    .toLayer

}
