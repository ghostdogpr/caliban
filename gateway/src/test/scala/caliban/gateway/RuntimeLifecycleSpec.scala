package caliban.gateway

import caliban.GraphQLResponseContext
import caliban.GraphQLResponseContext.{ Outcome, ServerFailure }
import caliban.gateway.GatewayRuntime.LifecycleState
import caliban.gateway.GatewayTestSupport._
import caliban.gateway.internal.RuntimeControl
import sttp.model.Uri
import zio._
import zio.http.{ Handler, Method, Response, Routes, Server, Status }
import zio.test._

object RuntimeLifecycleSpec extends ZIOSpecDefault {

  private val operationCacheStatus =
    GatewayRuntime.OperationCacheStatus(1L, 0L, 0, 0L, 0L, 0L, 0)

  private def makeControl(
    scope: Scope.Closeable,
    requestTimeout: Duration = 1.second,
    drainTimeout: Duration = 1.second,
    requestLimit: Int = 1
  ): UIO[RuntimeControl] =
    scope.extend(RuntimeControl.make(requestLimit, Map.empty, requestTimeout, drainTimeout))

  private def waitForControl(
    control: RuntimeControl
  )(predicate: GatewayRuntime.Status => Boolean): UIO[GatewayRuntime.Status] =
    (ZIO.yieldNow *> control.status(operationCacheStatus)).repeatUntil(predicate)

  private def retryEndpoint(calls: Ref[Int]): ZIO[Server with Ref[Int], Nothing, Uri] =
    postEndpoint("runtime-lifecycle-retry")(_ => calls.update(_ + 1).as(Response.status(Status.ServiceUnavailable)))

  def spec = suite("RuntimeLifecycleSpec")(
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
        status      <- runtime.status
      } yield assertTrue(
        response.errors.map(_.msg) == List("Gateway request timed out."),
        calls == 0,
        status.lifecycle.active == 0,
        status.lifecycle.overdue == 0
      )
    },
    test("includes request and source permit waits in the deadline") {
      for {
        sourceStarted <- Promise.make[Nothing, Unit]
        release       <- Promise.make[Nothing, Unit]
        calls         <- Ref.make(0)
        effect         = calls.updateAndGet(_ + 1) *> sourceStarted.succeed(()).unit *>
                           ZIO.uninterruptible(release.await).as("value")
        runtime       <- Gateway
                           .compose(Subgraph.local("local", localGraph(effect)))
                           .withConfig(
                             _.withMaxConcurrentRequests(2)
                               .withMaxConcurrentLocalCalls(1)
                               .withRequestTimeout(1.second)
                           )
                           .build
        first         <- runtime.execute("{ value }").fork
        _             <- sourceStarted.await
        second        <- runtime.execute("{ value }").fork
        queued        <- waitForStatus(runtime)(_.sources.get("local").exists(_.waiting == 1))
        _             <- TestClock.adjust(1.second)
        secondResult  <- second.join
        overdue       <- waitForStatus(runtime)(_.lifecycle.overdue == 1)
        sourceCalls   <- calls.get
        _             <- release.succeed(())
        firstResult   <- first.join
        done          <- runtime.status
      } yield assertTrue(
        queued.lifecycle.active == 2,
        secondResult.errors.map(_.msg) == List("Gateway request timed out."),
        sourceCalls == 1,
        overdue.lifecycle.active == 1,
        firstResult.errors.map(_.msg) == List("Gateway request timed out."),
        done.lifecycle.active == 0,
        done.lifecycle.overdue == 0
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
        status      <- runtime.status
      } yield assertTrue(
        exit.isInterrupted,
        status.lifecycle.active == 0,
        status.lifecycle.overdue == 0
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
        queued       <- waitForControl(control)(_.requests.waiting == 1)
        _            <- TestClock.adjust(1.second)
        secondResult <- second.join
        overdue      <- waitForControl(control)(_.lifecycle.overdue == 1)
        _            <- release.succeed(())
        firstResult  <- first.join
        done         <- control.status(operationCacheStatus)
        _            <- scope.close(Exit.unit)
      } yield assertTrue(
        queued.lifecycle.active == 2,
        secondResult == "timeout",
        overdue.lifecycle.active == 1,
        firstResult == "timeout",
        done.lifecycle.active == 0
      )
    },
    test("keeps timed-out uninterruptible completion and handoff work visible until it exits") {
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
        overdue    <- waitForControl(control)(_.lifecycle.overdue == 1)
        cancelling <- fiber.interrupt.fork
        pending    <- cancelling.poll
        _          <- release.succeed(())
        exit       <- cancelling.join
        done       <- control.status(operationCacheStatus)
        _          <- scope.close(Exit.unit)
      } yield assertTrue(
        overdue.lifecycle.active == 1,
        pending.isEmpty,
        exit.isInterrupted,
        done.lifecycle.active == 0,
        done.lifecycle.overdue == 0
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
    test("stops a replay-safe retry sequence at the request deadline") {
      val remoteConfig = RemoteGraphQLConfig.default.withExecution(
        _.withTimeout(1.hour)
          .withRetries(1, 10.seconds)
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
        draining <- waitForControl(control)(_.lifecycle.state == LifecycleState.Draining)
        rejected <- control.runRequest(ZIO.succeed("late"))(ZIO.succeed("timeout"))(ZIO.succeed("rejected"))
        _        <- release.succeed(())
        result   <- accepted.join
        _        <- closing.join
        closed   <- control.status(operationCacheStatus)
      } yield assertTrue(
        draining.lifecycle.active == 1,
        rejected == "rejected",
        result == "done",
        closed.lifecycle.state == LifecycleState.Closed,
        closed.lifecycle.active == 0
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
        _        <- waitForStatus(runtime)(_.lifecycle.state == LifecycleState.Draining)
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
        _           <- waitForControl(control)(_.lifecycle.state == LifecycleState.Draining)
        _           <- TestClock.adjust(1.second)
        _           <- interrupted.await
        exit        <- running.await
        _           <- closing.join
        closed      <- control.status(operationCacheStatus)
      } yield assertTrue(
        exit.isInterrupted,
        closed.lifecycle.state == LifecycleState.Closed,
        closed.lifecycle.active == 0,
        closed.lifecycle.overdue == 0
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
        overdue <- waitForStatus(runtime)(_.lifecycle.overdue == 1)
        closing <- scope.close(Exit.unit).fork
        _       <- waitForStatus(runtime)(_.lifecycle.state == LifecycleState.Draining)
        _       <- TestClock.adjust(1.second)
        pending <- request.poll
        _       <- release.succeed(())
        exit    <- request.await
        _       <- closing.join
        closed  <- runtime.status
      } yield assertTrue(
        overdue.lifecycle.active == 1,
        pending.isEmpty,
        exit.isInterrupted,
        closed.lifecycle.state == LifecycleState.Closed,
        closed.lifecycle.active == 0,
        closed.lifecycle.overdue == 0
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
        _       <- waitForStatus(runtime)(_.lifecycle.state == LifecycleState.Draining)
        _       <- TestClock.adjust(1.second)
        exit    <- request.await
        _       <- closing.join
        closed  <- runtime.status
      } yield assertTrue(
        exit.isInterrupted,
        closed.lifecycle.state == LifecycleState.Closed,
        closed.lifecycle.active == 0,
        closed.lifecycle.overdue == 0
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
        exit.causeOption
          .flatMap(_.failureOption)
          .map(_.diagnostics)
          .contains(
            List(
              "Gateway request timeout must be finite and positive.",
              "Gateway drain timeout must be finite and positive."
            )
          )
      )
    }
  ).provideSomeShared[Scope](testServer, stubIds) @@ TestAspect.sequential
}
