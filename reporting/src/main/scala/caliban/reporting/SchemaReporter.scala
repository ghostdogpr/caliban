package caliban.reporting

import caliban.client.CalibanClientError.CommunicationError
import caliban.reporting.ReportingError.{ ClientError, RetryableError }
import sttp.client3.SttpBackend
import sttp.client3.UriContext
import zio._

trait SchemaReporter {
  def report[A](ref: SchemaReportingRef[A], withCoreSchema: Boolean): IO[ReportingError, ReportingResponse]
}

object SchemaReporter {

  def make(accessToken: String): ZIO[SttpClient, Nothing, SchemaReporter] = for {
    client <- ZIO.service[SttpClient]
  } yield new SchemaReporter {
    import caliban.reporting.client.{ Mutation, ReportSchemaError, ReportSchemaResponse, SchemaReport }

    private final val REPORTING_URL = "https://schema-reporting.api.apollographql.com"

    private val onReportSchemaError = (
      ReportSchemaError.withCoreSchema ~
        ReportSchemaError.code.map(_.value) ~
        ReportSchemaError.message ~
        ReportSchemaError.inSeconds.map(_.seconds)
    ).mapN(ReportingError.SchemaError(_, _, _, _))

    private val onReportSchemaResponse = (
      ReportSchemaResponse.withCoreSchema ~
        ReportSchemaResponse.inSeconds.map(_.seconds)
    ).mapN(ReportingResponse.apply(_, _))

    private def reportSchemaMutation(coreSchema: Option[String], report: SchemaReport) =
      Mutation.reportSchema(coreSchema, report)(
        onReportSchemaError = onReportSchemaError.map(Left(_)),
        onReportSchemaResponse = onReportSchemaResponse.map(Right(_))
      )

    override def report[A](ref: SchemaReportingRef[A], withCoreSchema: Boolean): IO[ReportingError, ReportingResponse] =
      ref.coreSchema.get.flatMap { coreSchema =>
        val renderedSchema = ref.renderSchema(coreSchema)
        Util.hashSchema(renderedSchema).flatMap { hash =>
          client
            .send(
              reportSchemaMutation(
                coreSchema = if (withCoreSchema) Some(renderedSchema) else None,
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
  }

  def fromConfig[R: Tag](
    f: R => String
  ): ZLayer[R with SttpClient, Nothing, SchemaReporter] =
    fromConfigZIO[R, Nothing]((r: R) => ZIO.succeed(f(r)))

  def fromConfigZIO[R: Tag, E](
    f: R => IO[E, String]
  ): ZLayer[SttpClient with R, E, SchemaReporter] = ZLayer {
    for {
      accessToken <- ZIO.serviceWithZIO[R](f)
      reporter    <- make(accessToken)
    } yield reporter
  }

  def fromDefaultConfig: ZLayer[SttpClient with System, Throwable, SchemaReporter] =
    fromConfigZIO[System, Throwable](
      _.env("APOLLO_KEY")
        .someOrFail(
          new Exception(
            "No environment variable found for `APOLLO_KEY`. You must define this value with your token in order to use the default configuration."
          )
        )
    )
}
