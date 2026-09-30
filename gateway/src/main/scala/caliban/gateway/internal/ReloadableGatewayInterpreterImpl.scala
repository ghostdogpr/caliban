package caliban.gateway.internal

import caliban.gateway._
import caliban.gateway.internal.GatewayInterpreterImpl.{ Admitting, View }
import zio._
import zio.stm.{ STM, TRef, USTM }

private[gateway] final class ReloadableGatewayInterpreterImpl[R] private (
  acquire: IO[GatewayBuildError, Gateway.Snapshot[R]],
  delay: Double => Duration,
  drainTimeout: Duration,
  state: TRef[ReloadableGatewayInterpreterImpl.State[R]]
) extends Admitting[R]
    with ReloadableGatewayInterpreter[R] {
  import ReloadableGatewayInterpreterImpl._

  def lastReloadFailure(implicit trace: Trace): UIO[Option[String]] = state.get.commit.map(_.lastFailure)

  // Selection, the closing check and the reservation commit together.
  def admit(startedAt: Long): USTM[View[R]] =
    state.get.flatMap { current =>
      val interpreter = current.active.interpreter
      if (current.closing) STM.succeed(interpreter.rejected) else interpreter.admit(startedAt)
    }

  private def refreshLoop(implicit trace: Trace): UIO[Unit] =
    state.get.commit.flatMap { current =>
      if (current.closing) ZIO.unit
      else Random.nextDouble.flatMap(random => Clock.sleep(delay(random))) *> refresh *> ZIO.suspendSucceed(refreshLoop)
    }

  private def refresh(implicit trace: Trace): UIO[Unit] =
    acquire.flatMap { snapshot =>
      state.get.commit.flatMap { current =>
        if (snapshot.fingerprints == current.active.fingerprints) setFailure(None)
        else ZIO.uninterruptibleMask(Generation.open(current.active.id + 1L, snapshot, _).flatMap(activate))
      }
    }.catchAll(error => setFailure(Some(safeReason(error))))
      .catchAllDefect(_ => setFailure(Some("Unexpected refresh failure.")))

  private def activate(next: Generation[R])(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      if (current.closing) next.scope.close(Exit.unit) -> current
      else
        (setFailure(None) *> ZIO.logInfo(s"Gateway activated generation ${next.id}."))
          .ensuring(retire(current.active))            -> current.copy(active = next, retiring = true)
    }.commit.flatten

  private def retire(old: Generation[R])(implicit trace: Trace): UIO[Unit] =
    for {
      _       <- old.interpreter.retireSubscriptions
      watcher <- (Clock.sleep(drainTimeout) *>
                   ZIO.logWarning(
                     s"Gateway generation ${old.id} exceeded its drain timeout; further refreshes are paused."
                   )).interruptible.fork
      _       <- old.scope.close(Exit.unit).ensuring(watcher.interrupt)
      _       <- state.update(_.copy(retiring = false)).commit
    } yield ()

  private def setFailure(failure: Option[String])(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      if (current.closing || current.lastFailure == failure) ZIO.unit -> current
      else
        failure.fold(ZIO.logInfo("Gateway schema refresh recovered."))(reason =>
          ZIO.logWarning(s"Gateway schema refresh failed: $reason Keeping the active generation.")
        )                                                             -> current.copy(lastFailure = failure)
    }.commit.flatten

  private def close(worker: Fiber.Runtime[Nothing, Unit])(implicit trace: Trace): UIO[Unit] =
    (for {
      current <- state.getAndUpdate(_.copy(closing = true)).commit
      // Retirement owns the old drain timer: interrupting it could abandon the drain.
      // The active generation closes concurrently with that existing retirement.
      _       <- current.active.scope.close(Exit.unit).zipPar(if (current.retiring) worker.await else worker.interrupt)
    } yield ()).uninterruptible
}

private[gateway] object ReloadableGatewayInterpreterImpl {
  def make[R](
    acquire: IO[GatewayBuildError, Gateway.Snapshot[R]],
    delay: Double => Duration,
    drainTimeout: Duration
  )(implicit trace: Trace): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] =
    ZIO.uninterruptibleMask { restore =>
      for {
        snapshot <- restore(acquire)
        initial  <- Generation.open(1L, snapshot, restore)
        state    <- TRef.make(State(initial, retiring = false, closing = false, lastFailure = None)).commit
        runtime   = new ReloadableGatewayInterpreterImpl(acquire, delay, drainTimeout, state)
        worker   <- runtime.refreshLoop.interruptible.forkDaemon
        _        <- ZIO.addFinalizer(runtime.close(worker))
      } yield runtime
    }

  private final case class Generation[-R](
    id: Long,
    fingerprints: List[String],
    interpreter: GatewayInterpreterImpl[R],
    scope: Scope.Closeable
  )

  private object Generation {
    def open[R](id: Long, snapshot: Gateway.Snapshot[R], restore: ZIO.InterruptibilityRestorer)(implicit
      trace: Trace
    ): IO[GatewayBuildError, Generation[R]] =
      Scope.make.flatMap(scope =>
        Gateway.buildIn(scope, restore)(snapshot.build).map(Generation(id, snapshot.fingerprints, _, scope))
      )
  }

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
