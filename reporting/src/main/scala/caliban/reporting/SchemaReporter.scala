package caliban.reporting

import caliban.client.CalibanClientError.CommunicationError
import caliban.reporting.ReportingError.{ ClientError, RetryableError }
import sttp.client3.UriContext
import sttp.client3.asynchttpclient.zio.SttpClient
import sttp.client3.asynchttpclient.zio.SttpClient.Service
import zio.duration.durationInt
import zio.{ Has, IO, Tag, ZIO, ZLayer }
import zio.system.System

trait SchemaReporter {
  def report[A](ref: SchemaReportingRef[A], withCoreSchema: Boolean): IO[ReportingError, ReportingResponse]
}

object SchemaReporter {

  def make(accessToken: String): ZIO[Has[SttpClient.Service], Nothing, SchemaReporter] = (for {
    client <- ZIO.service[SttpClient.Service]
  } yield new SchemaReporter {
    import caliban.reporting.client.{ Mutation, ReportSchemaError, ReportSchemaResponse, SchemaReport }

    private final val REPORTING_URL = "https://schema-reporting.api.apollographql.com"

    private val onReportSchemaError = (
      ReportSchemaError.withCoreSchema ~
        ReportSchemaError.code.map(_.value) ~
        ReportSchemaError.message ~
        ReportSchemaError.inSeconds.map(_.seconds)
    ).mapN(ReportingError.SchemaError)

    private val onReportSchemaResponse = (
      ReportSchemaResponse.withCoreSchema ~
        ReportSchemaResponse.inSeconds.map(_.seconds)
    ).mapN(ReportingResponse)

    private def reportSchemaMutation(coreSchema: Option[String], report: SchemaReport) =
      Mutation.reportSchema(coreSchema, report)(
        onReportSchemaError = onReportSchemaError.map(Left(_)),
        onReportSchemaResponse = onReportSchemaResponse.map(Right(_))
      )

    override def report[A](ref: SchemaReportingRef[A], withCoreSchema: Boolean): IO[ReportingError, ReportingResponse] =
      ref.coreSchema.get.flatMap { coreSchema =>
        Util.hashSchema(coreSchema).flatMap { hash =>
          client
            .send(
              reportSchemaMutation(
                coreSchema = if (withCoreSchema) Some(coreSchema) else None,
                SchemaReport(
                  bootId = ref.bootId.toString,
                  coreSchemaHash = hash,
                  graphRef = ref.graphRef,
                  libraryVersion = ref.libraryVersion,
                  platform = ref.platform,
                  runtimeVersion = ref.runtimeVersion,
                  serverId = ref.serverId,
                  userVersion = ref.userVersion
                )
              )
                .toRequest(uri"$REPORTING_URL/api/graphql", useVariables = true)
                .header("X-API-Key", accessToken)
            )
        }
      }
        .mapError(error => RetryableError(error))
        .flatMap { response =>
          response.body match {
            case Left(error @ CommunicationError(_, _)) => ZIO.fail(RetryableError(error))
            case Left(error)                            => ZIO.fail(ClientError(error))
            case Right(Some(Left(error)))               => ZIO.fail(error)
            case Right(Some(Right(value)))              => ZIO.succeed(value)
            // Should never come here
            case Right(None)                            =>
              ZIO.dieMessage("You should never see this error, as it indicates there is something wrong with Caliban!")
          }
        }
  })

  def fromConfig[R: Tag](
    f: R => String
  ): ZLayer[Has[R] with Has[SttpClient.Service], Nothing, Has[SchemaReporter]] =
    fromConfigZIO((r: R) => ZIO.succeed(f(r)))

  def fromConfigZIO[R: Tag, E](
    f: R => IO[E, String]
  ): ZLayer[Has[SttpClient.Service] with Has[R], E, Has[SchemaReporter]] =
    (for {
      accessToken <- ZIO.serviceWith[R](f)
      reporter    <- make(accessToken)
    } yield reporter).toLayer

  def fromDefaultConfig: ZLayer[Has[SttpClient.Service] with System, Throwable, Has[SchemaReporter]] =
    fromConfigZIO[System.Service, Throwable](
      _.env("APOLLO_KEY")
        .someOrFail(
          new Exception(
            "No environment variable found for `APOLLO_KEY`. You must define this value with your token in order to use the default configuration."
          )
        )
    )
}
