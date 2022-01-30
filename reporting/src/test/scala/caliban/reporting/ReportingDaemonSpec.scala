package caliban.reporting

import caliban.client.CalibanClientError.CommunicationError
import caliban.reporting.ReportingError.{ ClientError, RetryableError }
import caliban.{ GraphQL, RootResolver }
import zio.duration.durationInt
import zio.test.Assertion._
import zio.test.environment.TestClock
import zio.test.mock._
import zio.test.{ assertTrue, Assertion, DefaultRunnableSpec, ZSpec }
import zio.{ IO, Promise, Ref, UIO }

object ReportingDaemonSpec extends DefaultRunnableSpec {
  case class User(id: Int, name: Option[String])
  case class Query(
    a: String,
    user: UIO[User]
  )

  val api = GraphQL.graphQL(
    RootResolver(
      Query(
        "hello",
        UIO.succeed(User(42, Some("bar")))
      )
    )
  )

  def tuple2[A, B](a: Assertion[A], b: Assertion[B]): Assertion[(A, B)] =
    hasField[(A, B), A]("_1", _._1, a) &&
      hasField[(A, B), B]("_2", _._2, b)

  def failReport(effect: IO[ReportingError, Nothing]) =
    MockSchemaReporter.Report(anything, Expectation.failureM(_ => effect))

  def succeedReport(effect: UIO[ReportingResponse], assertion: Assertion[(SchemaReportingRef, Boolean)] = anything) =
    MockSchemaReporter.Report(assertion, Expectation.valueM(_ => effect))

  val schemaRef = SchemaReportingRef.make(api, "test@test")

  // Scenarios
  // Call returns a non-2xx response (should retry after 20 seconds)
  // Call returns an error (should stop retrying and just print an error)
  // Call returns a 200 requesting the schema on the next call (without delay)
  // Call returns a 200 without requesting schema on the next call (with delay).

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("SchemaReporting")(
    testM("non-2xx response should retry after 20 seconds")(
      for {
        ref     <- schemaRef
        counter <- Ref.make(0)
        latch   <- Promise.make[Nothing, Unit]
        mockEnv  = failReport((counter.update(_ + 1) as RetryableError(CommunicationError("404"))).flip).atLeast(1)
        daemon   = ReportingDaemon.make(ref).provideCustomLayer(mockEnv)
        _       <- daemon.use_(latch.await).fork
        _       <- TestClock.adjust(1.minute)
        _       <- latch.succeed(())
        c       <- counter.get
      } yield assertTrue(c == 4)
    ),
    testM("Receiving a schema error should halt the process")(
      for {
        ref     <- schemaRef
        counter <- Ref.make(0)
        latch   <- Promise.make[Nothing, Unit]
        mockEnv  = failReport((counter.update(_ + 1) as ClientError(CommunicationError("404"))).flip).atLeast(1)
        daemon   = ReportingDaemon.make(ref).provideCustomLayer(mockEnv)
        _       <- daemon.use_(latch.await).fork
        _       <- TestClock.adjust(1.minute) *> latch.succeed(())
        c       <- counter.get
      } yield assertTrue(c == 1)
    ),
    testM("If true then next request should send the schema")(for {
      ref     <- schemaRef
      counter <- Ref.make(0)
      latch   <- Promise.make[Nothing, Unit]
      mockEnv  = succeedReport(
                   counter.update(_ + 1) as ReportingResponse(true, 10.seconds),
                   tuple2(equalTo(ref), isFalse)
                 ) ++
                   succeedReport(
                     counter.update(_ + 1) as ReportingResponse(false, 60.seconds),
                     tuple2(equalTo(ref), isTrue)
                   )
      daemon   = ReportingDaemon.make(ref).provideCustomLayer(mockEnv)
      _       <- daemon.use_(latch.await).fork
      c1      <- TestClock.adjust(5.seconds) *> counter.get
      _       <- TestClock.adjust(1.minute) *> latch.succeed(())
      c2      <- counter.get
    } yield assertTrue(c1 == 1) && assertTrue(c2 == 2)),
    testM("If false it received the next payload shouldn't include the schema")(for {
      ref     <- schemaRef
      counter <- Ref.make(0)
      latch   <- Promise.make[Nothing, Unit]
      mockEnv  = succeedReport(
                   counter.update(_ + 1) as ReportingResponse(false, 10.seconds),
                   tuple2(equalTo(ref), isFalse)
                 ) ++
                   succeedReport(
                     counter.update(_ + 1) as ReportingResponse(false, 60.seconds),
                     tuple2(equalTo(ref), isFalse)
                   )
      daemon   = ReportingDaemon.make(ref).provideCustomLayer(mockEnv)
      _       <- daemon.use_(latch.await).fork
      c1      <- TestClock.adjust(5.seconds) *> counter.get
      _       <- TestClock.adjust(1.minute) *> latch.succeed(())
      c2      <- counter.get
    } yield assertTrue(c1 == 1) && assertTrue(c2 == 2))
  )
}
