package caliban.gateway.internal

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import caliban.gateway._
import caliban.gateway.internal.GatewayInterpreterImpl.requestShutdownError
import zio._

private[gateway] final class ReloadableGatewayInterpreterImpl[R] private (
  acquire: IO[GatewayBuildError, Gateway.Snapshot[R]],
  pollInterval: Duration,
  jitter: Double,
  drainTimeout: Duration,
  state: Ref[ReloadableGatewayInterpreterImpl.State[R]]
) extends ReloadableGatewayInterpreter[R] {
  import ReloadableGatewayInterpreterImpl._

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    use(_.check(query))(ZIO.fail(requestShutdownError))

  def explain(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, String] =
    use(_.explain(request))(ZIO.fail(requestShutdownError))

  def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    use(_.executeRequest(request))(shutdownResponse)

  private def use[R0, E, A](f: GatewayInterpreterImpl[R] => ZIO[R0, E, A])(
    rejected: => ZIO[R0, E, A]
  )(implicit trace: Trace): ZIO[R0, E, A] =
    ZIO.uninterruptibleMask { restore =>
      reserve.flatMap {
        case Some(reserved) => restore(ZIO.suspendSucceed(f(reserved))).ensuring(reserved.release)
        case None           => restore(rejected)
      }
    }

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

  def lastReloadFailure(implicit trace: Trace): UIO[Option[String]] = state.get.map(_.lastFailure)

  private def loop(implicit trace: Trace): UIO[Unit] =
    state.get.flatMap { current =>
      if (current.closing) ZIO.unit
      else {
        val delay =
          if (jitter == 0.0) ZIO.succeed(pollInterval)
          else
            Random.nextDouble.map { random =>
              val factor = 1.0 + (2.0 * random - 1.0) * jitter
              Duration.fromNanos(math.max(1L, (pollInterval.toNanos.toDouble * factor).toLong))
            }
        delay.flatMap(Clock.sleep(_)) *> cycle *> ZIO.suspendSucceed(loop)
      }
    }

  private def cycle(implicit trace: Trace): UIO[Unit] =
    (for {
      run <- state.get.map(current => !current.closing && !current.retiring && current.candidate.isEmpty)
      _   <- ZIO.when(run) {
               acquire.flatMap { snapshot =>
                 state.get.flatMap { current =>
                   if (current.closing) ZIO.unit
                   else if (snapshot.fingerprints == current.active.fingerprints) unchanged
                   else replace(snapshot)
                 }
               }
             }
    } yield ()).catchAll(error => failed(Some(error))).catchAllCause { cause =>
      if (cause.isInterrupted) ZIO.refailCause(cause)
      else failed(None)
    }

  private def unchanged(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      if (current.closing) false        -> current
      else current.lastFailure.nonEmpty -> current.copy(lastFailure = None)
    }.flatMap(recovered => ZIO.logInfo("Gateway schema refresh recovered.").when(recovered).unit)

  private def replace(snapshot: Gateway.Snapshot[R])(implicit trace: Trace): IO[GatewayBuildError, Unit] =
    ZIO.uninterruptibleMask { restore =>
      for {
        candidate <- Scope.make
        accepted  <- state.modify { current =>
                       if (current.closing) false -> current
                       else true                  -> current.copy(candidate = Some(candidate))
                     }
        _         <- if (!accepted) candidate.close(Exit.unit)
                     else
                       restore(candidate.extend(snapshot.gateway.buildInterpreter))
                         .onError(cause => candidate.close(Exit.failCause(cause)) *> clearCandidate(candidate))
                         .flatMap { interpreter =>
                           state.modify { current =>
                             if (current.closing) Option.empty[(Generation[R], Boolean)] -> current
                             else
                               Some((current.active, current.lastFailure.nonEmpty))      -> current.copy(
                                 active = Generation(current.active.id + 1L, snapshot.fingerprints, interpreter, candidate),
                                 retiring = true,
                                 candidate = None,
                                 lastFailure = None
                               )
                           }.flatMap {
                             case None                   => candidate.close(Exit.unit) *> clearCandidate(candidate)
                             case Some((old, recovered)) =>
                               (ZIO.logInfo("Gateway schema refresh recovered.").when(recovered) *>
                                 ZIO.logInfo(s"Gateway activated generation ${old.id + 1L}.")).ensuring(retire(old))
                           }
                         }
      } yield ()
    }

  private def clearCandidate(candidate: Scope.Closeable)(implicit trace: Trace): UIO[Unit] =
    state.update(current => if (current.candidate.contains(candidate)) current.copy(candidate = None) else current)

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

  private def failed(error: Option[GatewayBuildError])(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      if (current.closing) Option.empty[String] -> current
      else {
        val reason = error.fold("Unexpected refresh failure.")(safeReason)
        (if (current.lastFailure.contains(reason)) None else Some(reason)) -> current.copy(lastFailure = Some(reason))
      }
    }.flatMap(value =>
      ZIO.foreachDiscard(value)(reason =>
        ZIO.logWarning(s"Gateway schema refresh failed: $reason Keeping the active generation.")
      )
    )

  private def close(worker: Fiber.Runtime[Nothing, Unit])(implicit trace: Trace): UIO[Unit] =
    (for {
      owned <- state.modify { current =>
                 val owned = current.active.scope :: current.candidate.toList
                 (owned, current.retiring) -> current.copy(closing = true)
               }
      // Retirement owns the old drain timer: interrupting it could abandon the drain.
      // The active generation closes concurrently with that existing retirement.
      _     <- ZIO
                 .foreachParDiscard(owned._1)(_.close(Exit.unit))
                 .zipPar(if (owned._2) worker.await else worker.interrupt)
      _     <- state.update(_.copy(candidate = None))
    } yield ()).uninterruptible
}

private[gateway] object ReloadableGatewayInterpreterImpl {
  private final case class Generation[-R](
    id: Long,
    fingerprints: List[String],
    interpreter: GatewayInterpreterImpl[R],
    scope: Scope.Closeable
  )

  private final case class State[-R](
    active: Generation[R],
    retiring: Boolean,
    candidate: Option[Scope.Closeable],
    closing: Boolean,
    lastFailure: Option[String]
  )

  def make[R](
    acquire: IO[GatewayBuildError, Gateway.Snapshot[R]],
    pollInterval: Duration,
    jitter: Double,
    drainTimeout: Duration
  )(implicit trace: Trace): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] =
    ZIO.uninterruptibleMask { restore =>
      restore(acquire).flatMap { snapshot =>
        Scope.make.flatMap { initialScope =>
          (for {
            interpreter <- restore(initialScope.extend(snapshot.gateway.buildInterpreter))
            state       <- Ref.make(
                             State(
                               Generation(1L, snapshot.fingerprints, interpreter, initialScope),
                               retiring = false,
                               candidate = None,
                               closing = false,
                               lastFailure = None
                             )
                           )
            runtime      = new ReloadableGatewayInterpreterImpl(acquire, pollInterval, jitter, drainTimeout, state)
            worker      <- runtime.loop.interruptible.forkDaemon
            _           <- ZIO.addFinalizer(runtime.close(worker))
          } yield runtime).onError(cause => initialScope.close(Exit.failCause(cause)))
        }
      }
    }

  private def safeReason(error: GatewayBuildError): String = error match {
    case _: GatewayBuildError.InvalidConfiguration          => "Invalid gateway configuration."
    case _: GatewayBuildError.TransportInitializationFailed => "Unable to initialize schema transport."
    case _: GatewayBuildError.SubgraphLoadingFailed         => "Unable to load subgraph schemas."
    case _: GatewayBuildError.SchemaCompositionFailed       => "Subgraph schemas could not be composed."
    case _: GatewayBuildError.SupergraphAcquisitionFailed   => "Unable to load supergraph."
    case _: GatewayBuildError.SupergraphDecompositionFailed => "Unable to decompose supergraph into subgraphs."
  }

}
