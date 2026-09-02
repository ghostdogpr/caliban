package caliban.gateway

import caliban.GraphQLResponseContext
import caliban.GraphQLResponseContext.{ Outcome, ServerFailure }
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.GatewayExecutionControl
import sttp.model.Uri
import zio._
import zio.http.{ Handler, Method, Response, Routes, Server, Status }
import zio.test._

object RuntimeLifecycleSpec extends ZIOSpecDefault {

  private def makeControl(
    scope: Scope.Closeable,
    requestTimeout: Duration = 1.second,
    drainTimeout: Duration = 1.second,
    requestLimit: Int = 1
  ): UIO[GatewayExecutionControl[Any]] =
    scope.extend(
      GatewayExecutionControl.make(
        requestLimit,
        GatewaySubscriptionConfig(),
        GatewayWrapper.empty,
        requestTimeout,
        drainTimeout
      )
    )

  private def awaitDrain(control: GatewayExecutionControl[Any]): UIO[Unit] =
    control.reserve.flatMap {
      case Some(lease) => control.release(lease).as(false)
      case None        => ZIO.succeed(true)
    }.repeatUntil(identity).unit

  private def awaitDrain(runtime: caliban.gateway.internal.GatewayInterpreterImpl[Any]): UIO[Unit] =
    runtime.reserve.flatMap {
      case Some(lease) => lease.release.as(false)
      case None        => ZIO.succeed(true)
    }.repeatUntil(identity).unit

  private def retryEndpoint(calls: Ref[Int]): ZIO[Server with Ref[Int], Nothing, Uri] =
    postEndpoint("runtime-lifecycle-retry")(_ => calls.update(_ + 1).as(Response.status(Status.ServiceUnavailable)))

  def spec = suite("RuntimeLifecycleSpec")(
    test("executes a reserved request when draining starts before execution") {
      for {
        scope   <- Scope.make
        runtime <- scope.extend(Gateway.compose(Subgraph.local("local", localGraph(ZIO.succeed("accepted")))).build)
        request <- runtime.reserve.someOrFailException
        closing <- scope.close(Exit.unit).fork
        _       <- awaitDrain(runtime)
        result  <- request.execute("{ value }").ensuring(request.release)
        _       <- closing.join
      } yield assertTrue(
        result.errors.isEmpty,
        field(result.data, "value").contains(caliban.Value.StringValue("accepted"))
      )
    },
    test("releases a reservation cancelled before request execution") {
      for {
        scope    <- Scope.make
        runtime  <- scope.extend(Gateway.compose(Subgraph.local("local", localGraph(ZIO.never))).build)
        request  <- runtime.reserve.someOrFailException
        closing  <- scope.close(Exit.unit).fork
        _        <- awaitDrain(runtime)
        _        <- request.release
        _        <- closing.join
        rejected <- runtime.reserve
      } yield assertTrue(rejected.isEmpty)
    },
    test("applies one deadline to operation resolution before source execution") {
      for {
        resolving   <- Promise.make[Nothing, Unit]
        interrupted <- Promise.make[Nothing, Unit]
        sourceCalls <- Ref.make(0)
        resolver     = OperationResolver.uncached[Any](_ =>
                         (resolving.succeed(()).unit *> ZIO.never).onInterrupt(interrupted.succeed(()).unit)
                       )
        runtime     <- Gateway
                         .compose(Subgraph.local("local", localGraph(sourceCalls.update(_ + 1).as("value"))))
                         .withOperationResolver(resolver)
                         .withConfig(_.withRequestTimeout(1.second))
                         .build
        fiber       <- runtime.executeRequest(caliban.GraphQLRequest()).fork
        _           <- resolving.await
        _           <- TestClock.adjust(1.second)
        response    <- fiber.join
        _           <- interrupted.await
        calls       <- sourceCalls.get
      } yield assertTrue(
        response.errors.map(_.msg) == List("Gateway request timed out."),
        calls == 0
      )
    },
    test("preserves caller interruption without fabricating a response") {
      for {
        started     <- Promise.make[Nothing, Unit]
        interrupted <- Promise.make[Nothing, Unit]
        runtime     <- Gateway
                         .compose(
                           Subgraph.local(
                             "local",
                             localGraph(
                               (started.succeed(()).unit *> ZIO.never).onInterrupt(interrupted.succeed(()).unit)
                             )
                           )
                         )
                         .withConfig(_.withRequestTimeout(1.hour))
                         .build
        fiber       <- runtime.execute("{ value }").fork
        _           <- started.await
        exit        <- fiber.interrupt
        _           <- interrupted.await
      } yield assertTrue(
        exit.isInterrupted
      )
    },
    test("includes request admission waits in the deadline") {
      for {
        scope        <- Scope.make
        control      <- makeControl(scope)
        started      <- Promise.make[Nothing, Unit]
        release      <- Promise.make[Nothing, Unit]
        first        <- control
                          .runRequest(started.succeed(()).unit *> ZIO.uninterruptible(release.await).as("first"))(
                            ZIO.succeed("timeout")
                          )(ZIO.interrupt)
                          .fork
        _            <- started.await
        second       <- control.runRequest(ZIO.succeed("second"))(ZIO.succeed("timeout"))(ZIO.interrupt).fork
        _            <- TestClock.adjust(1.second)
        secondResult <- second.join
        _            <- release.succeed(())
        firstResult  <- first.join
        _            <- scope.close(Exit.unit)
      } yield assertTrue(
        secondResult == "timeout",
        firstResult == "timeout"
      )
    },
    test("waits for timed-out uninterruptible completion and handoff work to exit") {
      for {
        scope      <- Scope.make
        control    <- makeControl(scope)
        completing <- Promise.make[Nothing, Unit]
        release    <- Promise.make[Nothing, Unit]
        fiber      <- control
                        .runRequest(completing.succeed(()).unit *> ZIO.uninterruptible(release.await).as("late"))(
                          ZIO.succeed("timeout")
                        )(ZIO.interrupt)
                        .fork
        _          <- completing.await
        _          <- TestClock.adjust(1.second)
        cancelling <- fiber.interrupt.fork
        pending    <- ZIO.yieldNow.repeatN(20) *> cancelling.poll
        _          <- release.succeed(())
        exit       <- cancelling.join
        _          <- scope.close(Exit.unit)
      } yield assertTrue(
        pending.isEmpty,
        exit.isInterrupted
      )
    },
    test("does not deliver a public response after the source result reaches the deadline handoff") {
      for {
        started  <- Promise.make[Nothing, Unit]
        release  <- Promise.make[Nothing, Unit]
        runtime  <- Gateway
                      .compose(
                        Subgraph.local(
                          "local",
                          localGraph(started.succeed(()).unit *> ZIO.uninterruptible(release.await).as("late"))
                        )
                      )
                      .withConfig(_.withRequestTimeout(1.second))
                      .build
        response <- runtime.execute("{ value }").fork.flatMap { fiber =>
                      started.await *> TestClock.adjust(1.second) *> release.succeed(()) *> fiber.join
                    }
      } yield assertTrue(
        response.data == caliban.Value.NullValue,
        response.errors.map(_.msg) == List("Gateway request timed out.")
      )
    },
    test("stops an unshared retry sequence at the request deadline") {
      val remoteConfig = RemoteGraphQLConfig.default.withExecution(
        _.withTimeout(1.hour)
          .withRetries(1, 10.seconds)
          .withInFlightQueryDeduplication(false)
      )
      for {
        calls    <- Ref.make(0)
        endpoint <- retryEndpoint(calls)
        runtime  <- Gateway
                      .compose(Subgraph.graphql("remote", endpoint, "type Query { value: String }", remoteConfig))
                      .withConfig(_.withRequestTimeout(1.second))
                      .build
        fiber    <- runtime.execute("{ value }").fork
        _        <- calls.get.repeatUntil(_ == 1)
        _        <- ZIO.yieldNow
        _        <- TestClock.adjust(1.second)
        response <- fiber.join
        total    <- calls.get
      } yield assertTrue(
        response.errors.map(_.msg) == List("Gateway request timed out."),
        total == 1
      )
    },
    test("drains accepted work before closing and rejects racing admissions") {
      for {
        scope    <- Scope.make
        control  <- makeControl(scope, requestTimeout = 1.hour, drainTimeout = 1.hour)
        started  <- Promise.make[Nothing, Unit]
        release  <- Promise.make[Nothing, Unit]
        accepted <- control
                      .runRequest(started.succeed(()).unit *> release.await.as("done"))(ZIO.succeed("timeout"))(
                        ZIO.interrupt
                      )
                      .fork
        _        <- started.await
        closing  <- scope.close(Exit.unit).fork
        _        <- awaitDrain(control)
        rejected <- control.runRequest(ZIO.succeed("late"))(ZIO.succeed("timeout"))(ZIO.succeed("rejected"))
        _        <- release.succeed(())
        result   <- accepted.join
        _        <- closing.join
      } yield assertTrue(
        rejected == "rejected",
        result == "done"
      )
    },
    test("returns a service-unavailable response to requests arriving while draining") {
      for {
        scope    <- Scope.make
        started  <- Promise.make[Nothing, Unit]
        release  <- Promise.make[Nothing, Unit]
        runtime  <-
          scope.extend(
            Gateway
              .compose(Subgraph.local("local", localGraph(started.succeed(()).unit *> release.await.as("done"))))
              .withConfig(_.withRequestTimeout(1.hour).withDrainTimeout(1.hour))
              .build
          )
        accepted <- runtime.execute("{ value }").fork
        _        <- started.await
        closing  <- scope.close(Exit.unit).fork
        _        <- awaitDrain(runtime)
        rejected <- GraphQLResponseContext.capture(runtime.execute("{ value }"))
        _        <- release.succeed(())
        _        <- accepted.join
        _        <- closing.join
      } yield assertTrue(
        rejected.value.errors.map(_.msg) == List("Gateway is shutting down."),
        rejected.outcome == Outcome.ServerError(ServerFailure.Unavailable)
      )
    },
    test("interrupts cooperative work after the drain timeout without detaching it") {
      for {
        scope       <- Scope.make
        control     <- makeControl(scope, requestTimeout = 1.hour, drainTimeout = 1.second)
        started     <- Promise.make[Nothing, Unit]
        interrupted <- Promise.make[Nothing, Unit]
        running     <- control
                         .runRequest(
                           (started.succeed(()).unit *> ZIO.never).onInterrupt(interrupted.succeed(()).unit)
                         )(ZIO.interrupt)(ZIO.interrupt)
                         .fork
        _           <- started.await
        closing     <- scope.close(Exit.unit).fork
        _           <- awaitDrain(control)
        _           <- TestClock.adjust(1.second)
        _           <- interrupted.await
        exit        <- running.await
        _           <- closing.join
      } yield assertTrue(
        exit.isInterrupted
      )
    },
    test("preserves forced shutdown after a request deadline while uninterruptible work remains overdue") {
      for {
        scope   <- Scope.make
        started <- Promise.make[Nothing, Unit]
        release <- Promise.make[Nothing, Unit]
        runtime <- scope.extend(
                     Gateway
                       .compose(
                         Subgraph.local(
                           "local",
                           localGraph(started.succeed(()).unit *> ZIO.uninterruptible(release.await).as("late"))
                         )
                       )
                       .withConfig(
                         _.withRequestTimeout(1.second)
                           .withDrainTimeout(1.second)
                       )
                       .build
                   )
        request <- runtime.execute("{ value }").fork
        _       <- started.await
        _       <- TestClock.adjust(1.second)
        closing <- scope.close(Exit.unit).fork
        _       <- awaitDrain(runtime)
        _       <- TestClock.adjust(1.second)
        pending <- request.poll
        _       <- release.succeed(())
        exit    <- request.await
        _       <- closing.join
      } yield assertTrue(
        pending.isEmpty,
        exit.isInterrupted
      )
    },
    test("gives forced scope shutdown precedence over a simultaneous request deadline") {
      for {
        scope   <- Scope.make
        started <- Promise.make[Nothing, Unit]
        runtime <- scope.extend(
                     Gateway
                       .compose(Subgraph.local("local", localGraph(started.succeed(()).unit *> ZIO.never)))
                       .withConfig(
                         _.withRequestTimeout(1.second)
                           .withDrainTimeout(1.second)
                       )
                       .build
                   )
        request <- runtime.execute("{ value }").fork
        _       <- started.await
        closing <- scope.close(Exit.unit).fork
        _       <- awaitDrain(runtime)
        _       <- TestClock.adjust(1.second)
        exit    <- request.await
        _       <- closing.join
      } yield assertTrue(
        exit.isInterrupted
      )
    },
    test("rejects non-finite request and drain deadlines at build time") {
      for {
        exit <- Gateway
                  .compose(Subgraph.local("local", localGraph(ZIO.succeed("value"))))
                  .withConfig(
                    _.withRequestTimeout(Duration.Zero)
                      .withDrainTimeout(Duration.Infinity)
                  )
                  .build
                  .exit
      } yield assertTrue(
        buildDiagnostics(exit) == List(
          "Gateway request timeout must be finite and positive.",
          "Gateway drain timeout must be finite and positive."
        )
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
