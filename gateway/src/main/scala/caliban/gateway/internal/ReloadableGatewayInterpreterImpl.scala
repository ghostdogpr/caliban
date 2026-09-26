package caliban.gateway.internal

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import caliban.gateway._
import caliban.gateway.internal.GatewayInterpreterImpl.{ requestShutdownError, shutdownResponse }
import zio._

private[gateway] final class ReloadableGatewayInterpreterImpl[R] private (
  acquire: IO[GatewayBuildError, Gateway.Snapshot[R]],
  pollInterval: Duration,
  jitter: Double,
  drainTimeout: Duration,
  state: Ref[ReloadableGatewayInterpreterImpl.State[R]],
  http: GatewayHttpClient
) extends ReloadableGatewayInterpreter[R] {
  import ReloadableGatewayInterpreterImpl._

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    use(_.check(query))(ZIO.fail(requestShutdownError))

  def explain(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, String] =
    use(_.explain(request))(ZIO.fail(requestShutdownError))

  def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    use(_.executeRequest(request))(shutdownResponse)

  def lastReloadFailure(implicit trace: Trace): UIO[Option[String]] = state.get.map(_.lastFailure)

  private def use[R0, E, A](f: GatewayInterpreterImpl[R] => ZIO[R0, E, A])(
    rejected: => ZIO[R0, E, A]
  )(implicit trace: Trace): ZIO[R0, E, A] =
    ZIO.acquireReleaseWith(reserve)(ZIO.foreachDiscard(_)(_.release))(_.fold(rejected)(f))

  private def reserve(implicit trace: Trace): UIO[Option[GatewayInterpreterImpl[R]]] =
    state.get.flatMap { current =>
      if (current.closing) ZIO.none
      else
        current.active.interpreter.reserve.flatMap {
          // Publication may have retired the selected generation. No request work has run yet.
          case None                         => ZIO.suspendSucceed(reserve)
          case reserved @ Some(interpreter) =>
            // A successful lease can race shutdown before that generation starts draining.
            state.get.flatMap(latest => if (latest.closing) interpreter.release.as(None) else ZIO.succeed(reserved))
        }
    }

  private def refreshLoop(implicit trace: Trace): UIO[Unit] =
    state.get.flatMap { current =>
      if (current.closing) ZIO.unit
      else pollDelay.flatMap(Clock.sleep(_)) *> refresh *> ZIO.suspendSucceed(refreshLoop)
    }

  private def pollDelay(implicit trace: Trace): UIO[Duration] =
    Random.nextDouble.map { random =>
      val factor = 1.0 + (2.0 * random - 1.0) * jitter
      Duration.fromNanos(math.max(1L, (pollInterval.toNanos.toDouble * factor).toLong))
    }

  private def refresh(implicit trace: Trace): UIO[Unit] =
    acquire.flatMap { snapshot =>
      state.get.flatMap { current =>
        if (current.closing) ZIO.unit
        else if (snapshot.fingerprints == current.active.fingerprints) clearFailure
        else replace(snapshot)
      }
    }.catchAll(error => recordFailure(Some(error))).catchAllDefect(_ => recordFailure(None))

  private def clearFailure(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      if (current.closing) false        -> current
      else current.lastFailure.nonEmpty -> current.copy(lastFailure = None)
    }.flatMap(recovered => ZIO.logInfo(RecoveredMessage).when(recovered).unit)

  private def replace(snapshot: Gateway.Snapshot[R])(implicit trace: Trace): IO[GatewayBuildError, Unit] =
    ZIO.uninterruptibleMask { restore =>
      Scope.make.flatMap { candidate =>
        Gateway
          .buildIn(candidate, restore)(snapshot.gateway.buildInterpreter(http))
          .flatMap(activate(snapshot, candidate, _))
      }
    }

  private def activate(
    snapshot: Gateway.Snapshot[R],
    candidate: Scope.Closeable,
    interpreter: GatewayInterpreterImpl[R]
  )(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      if (current.closing) Option.empty[(Generation[R], Boolean)] -> current
      else
        Some((current.active, current.lastFailure.nonEmpty))      -> current.copy(
          active = Generation(current.active.id + 1L, snapshot.fingerprints, interpreter, candidate),
          retiring = true,
          lastFailure = None
        )
    }.flatMap {
      case None                   => candidate.close(Exit.unit)
      case Some((old, recovered)) =>
        (ZIO.logInfo(RecoveredMessage).when(recovered) *>
          ZIO.logInfo(s"Gateway activated generation ${old.id + 1L}.")).ensuring(retire(old))
    }

  private def retire(old: Generation[R])(implicit trace: Trace): UIO[Unit] =
    for {
      _       <- old.interpreter.retireSubscriptions
      watcher <- (Clock.sleep(drainTimeout) *>
                   ZIO.logWarning(
                     s"Gateway generation ${old.id} exceeded its drain timeout; further refreshes are paused."
                   )).interruptible.fork
      _       <- old.scope.close(Exit.unit).ensuring(watcher.interrupt)
      _       <- state.update(_.copy(retiring = false))
    } yield ()

  private def recordFailure(error: Option[GatewayBuildError])(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      if (current.closing) Option.empty[String] -> current
      else {
        val reason = error.fold("Unexpected refresh failure.")(safeReason)
        (if (current.lastFailure.contains(reason)) None else Some(reason)) -> current.copy(lastFailure = Some(reason))
      }
    }.flatMap(changed =>
      ZIO.foreachDiscard(changed)(reason =>
        ZIO.logWarning(s"Gateway schema refresh failed: $reason Keeping the active generation.")
      )
    )

  private def close(worker: Fiber.Runtime[Nothing, Unit])(implicit trace: Trace): UIO[Unit] =
    (for {
      closing          <- state.modify { current =>
                            (current.active.scope, current.retiring) -> current.copy(closing = true)
                          }
      (scope, retiring) = closing
      // Retirement owns the old drain timer: interrupting it could abandon the drain.
      // The active generation closes concurrently with that existing retirement.
      _                <- scope.close(Exit.unit).zipPar(if (retiring) worker.await else worker.interrupt)
    } yield ()).uninterruptible
}

private[gateway] object ReloadableGatewayInterpreterImpl {
  private final val RecoveredMessage = "Gateway schema refresh recovered."

  def make[R](
    acquire: IO[GatewayBuildError, Gateway.Snapshot[R]],
    pollInterval: Duration,
    jitter: Double,
    drainTimeout: Duration,
    http: GatewayHttpClient
  )(implicit trace: Trace): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] =
    ZIO.uninterruptibleMask { restore =>
      for {
        snapshot     <- restore(acquire)
        initialScope <- Scope.make
        interpreter  <- Gateway.buildIn(initialScope, restore)(snapshot.gateway.buildInterpreter(http))
        state        <- Ref.make(
                          State(
                            Generation(1L, snapshot.fingerprints, interpreter, initialScope),
                            retiring = false,
                            closing = false,
                            lastFailure = None
                          )
                        )
        runtime       =
          new ReloadableGatewayInterpreterImpl(acquire, pollInterval, jitter, drainTimeout, state, http)
        worker       <- runtime.refreshLoop.interruptible.forkDaemon
        _            <- ZIO.addFinalizer(runtime.close(worker))
      } yield runtime
    }

  private final case class Generation[-R](
    id: Long,
    fingerprints: List[String],
    interpreter: GatewayInterpreterImpl[R],
    scope: Scope.Closeable
  )

  private final case class State[-R](
    active: Generation[R],
    retiring: Boolean,
    closing: Boolean,
    lastFailure: Option[String]
  )

  private def safeReason(error: GatewayBuildError): String = error match {
    case _: GatewayBuildError.InvalidConfiguration          => "Invalid gateway configuration."
    case _: GatewayBuildError.TransportInitializationFailed => "Unable to initialize schema transport."
    case _: GatewayBuildError.SubgraphLoadingFailed         => "Unable to load subgraph schemas."
    case _: GatewayBuildError.SchemaCompositionFailed       => "Subgraph schemas could not be composed."
    case _: GatewayBuildError.SupergraphAcquisitionFailed   => "Unable to load supergraph."
    case _: GatewayBuildError.SupergraphDecompositionFailed => "Unable to decompose supergraph into subgraphs."
  }

}
