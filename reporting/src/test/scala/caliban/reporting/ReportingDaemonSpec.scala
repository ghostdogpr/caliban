package caliban.reporting

import caliban.client.CalibanClientError.CommunicationError
import caliban.reporting.ReportingError.{ ClientError, RetryableError }
import caliban.schema.auto._
import caliban.{ GraphQL, RootResolver }
import zio.test.{ assertTrue, TestClock, ZIOSpecDefault }
import zio._

object ReportingDaemonSpec extends ZIOSpecDefault {
  case class User(id: Int, name: Option[String])
  case class Query(
    a: String,
    user: UIO[User]
  )

  val api = GraphQL.graphQL(
    RootResolver(
      Query(
        "hello",
        ZIO.succeed(User(42, Some("bar")))
      )
    )
  )

  case class Invocation(ref: SchemaReportingRef[_], withSchema: Boolean)

  class FakeSchemaReporter private (
    _invocations: Ref[List[Invocation]],
    respond: Ref[(SchemaReportingRef[_], Boolean, List[Invocation]) => IO[ReportingError, ReportingResponse]]
  ) extends SchemaReporter {
    override def report[A](
      ref: SchemaReportingRef[A],
      withCoreSchema: Boolean
    ): IO[ReportingError, ReportingResponse] = for {
      r    <- respond.get
      resp <- _invocations.modify(inv => (r(ref, withCoreSchema, inv), Invocation(ref, withCoreSchema) :: inv)).flatten
    } yield resp

    def invocations: UIO[List[Invocation]] = _invocations.get

    def whenReport(
      f: (SchemaReportingRef[_], Boolean, List[Invocation]) => IO[ReportingError, ReportingResponse]
    ): UIO[Unit] =
      respond.set(f)
  }

  object FakeSchemaReporter {

    def whenReport(
      f: (SchemaReportingRef[_], Boolean, List[Invocation]) => IO[ReportingError, ReportingResponse]
    ): URIO[FakeSchemaReporter, Unit] =
      ZIO.serviceWithZIO(_.whenReport(f))

    val defaultResponse: (SchemaReportingRef[_], Boolean, List[Invocation]) => IO[ReportingError, ReportingResponse] =
      (_, _, _) => ZIO.succeed(ReportingResponse(false, 0.seconds))

    def live(
      respond: (SchemaReportingRef[_], Boolean, List[Invocation]) => IO[ReportingError, ReportingResponse] =
        defaultResponse
    ): ZLayer[Any, Nothing, FakeSchemaReporter with SchemaReporter] = ZLayer.fromZIOEnvironment {
      for {
        _invocations <- Ref.make[List[Invocation]](Nil)
        r            <- Ref.make(respond)
      } yield {
        val reporter = new FakeSchemaReporter(_invocations, r)

        ZEnvironment[FakeSchemaReporter, SchemaReporter](reporter, reporter)
      }
    }

    def invocations: URIO[FakeSchemaReporter, List[Invocation]] =
      ZIO.serviceWithZIO(_.invocations)

  }

  def fakeReport(effect: IO[ReportingError, ReportingResponse]) =
    FakeSchemaReporter.whenReport((_, _, _) => effect)

  val schemaRef = SchemaReportingRef.make(api, "test@test")

  // Scenarios
  // Call returns a non-2xx response (should retry after 20 seconds)
  // Call returns an error (should stop retrying and just print an error)
  // Call returns a 200 requesting the schema on the next call (without delay)
  // Call returns a 200 without requesting schema on the next call (with delay).

  override def spec = suite("SchemaReporting")(
    test("non-2xx response should retry after 20 seconds")(
      for {
        ref   <- schemaRef
        latch <- Promise.make[Nothing, Unit]
        _     <- fakeReport(ZIO.fail(RetryableError(CommunicationError("404"))))
        _     <- ZIO.scoped(ReportingDaemon.register(ref) *> latch.await).fork
        _     <- TestClock.adjust(1.minute)
        _     <- latch.succeed(())
        c     <- FakeSchemaReporter.invocations
      } yield assertTrue(c.size == 4)
    ),
    test("Receiving a schema error should halt the process")(
      for {
        ref   <- schemaRef
        latch <- Promise.make[Nothing, Unit]
        _     <- fakeReport(ZIO.fail(ClientError(CommunicationError("404"))))
        _     <- ZIO.scoped(ReportingDaemon.register(ref) *> latch.await).fork
        _     <- TestClock.adjust(1.minute) *> latch.succeed(())
        c     <- FakeSchemaReporter.invocations
      } yield assertTrue(c.size == 1)
    ),
    test("If true was received then next request should send the schema")(for {
      ref   <- schemaRef
      latch <- Promise.make[Nothing, Unit]
      _     <- FakeSchemaReporter.whenReport {
                 case (_, _, prev) if prev.isEmpty => ZIO.succeed(ReportingResponse(true, 10.seconds))
                 case _                            => ZIO.succeed(ReportingResponse(false, 60.seconds))
               }
      _     <- ZIO.scoped(ReportingDaemon.register(ref) *> latch.await).fork
      c1    <- TestClock.adjust(5.seconds) *> FakeSchemaReporter.invocations
      _     <- TestClock.adjust(1.minute) *> latch.succeed(())
      c2    <- FakeSchemaReporter.invocations
    } yield assertTrue(c1.size == 1) && assertTrue(c2.size == 2)),
    test("If false was received the next payload shouldn't include the schema")(for {
      ref   <- schemaRef
      latch <- Promise.make[Nothing, Unit]
      _     <- FakeSchemaReporter.whenReport {
                 case (_, _, prev) if prev.isEmpty => ZIO.succeed(ReportingResponse(false, 10.seconds))
                 case _                            => ZIO.succeed(ReportingResponse(false, 60.seconds))
               }
      _     <- ZIO.scoped(ReportingDaemon.register(ref) *> latch.await).fork
      c1    <- TestClock.adjust(5.seconds) *> FakeSchemaReporter.invocations
      _     <- TestClock.adjust(1.minute) *> latch.succeed(())
      c2    <- FakeSchemaReporter.invocations
    } yield assertTrue(c1.size == 1) && assertTrue(c2.size == 2))
  ).provideLayer(FakeSchemaReporter.live() >+> ReportingDaemon.live)
}
