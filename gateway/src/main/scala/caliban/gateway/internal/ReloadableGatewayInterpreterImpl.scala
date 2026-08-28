package caliban.gateway.internal

import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, GraphQLResponseContext }
import caliban.GraphQLResponseContext.ServerFailure
import caliban.gateway._
import caliban.gateway.GatewayInterpreter.{ AdmissionStatus, LifecycleState, LifecycleStatus }
import caliban.gateway.ReloadableGatewayInterpreter.{ Failure, FailureStage, Phase }
import caliban.gateway.internal.GatewayInterpreterImpl.{ requestShutdownError, requestShutdownResponse }
import zio._

import java.time.Instant

private[gateway] final class ReloadableGatewayInterpreterImpl[R] private (
  acquire: IO[GatewayBuildError, Gateway.Snapshot[R]],
  config: GatewayReloadConfig,
  drainTimeout: Duration,
  state: Ref[ReloadableGatewayInterpreterImpl.State[R]]
) extends ReloadableGatewayInterpreter[R] {
  import ReloadableGatewayInterpreterImpl._

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    use(_.check(query))(ZIO.fail(requestShutdownError))

  def explain(request: GraphQLRequest)(implicit trace: Trace): ZIO[R, CalibanError, String] =
    use(_.explain(request))(ZIO.fail(requestShutdownError))

  def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    use(_.executeRequest(request))(
      GraphQLResponseContext.markServerError(ServerFailure.Unavailable).as(requestShutdownResponse)
    )

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

  def reloadStatus(implicit trace: Trace): UIO[ReloadableGatewayInterpreter.Status] =
    state.get.flatMap { current =>
      current.active.status.zipWith(ZIO.foreach(current.retiring)(_.status)) { (active, retiring) =>
        ReloadableGatewayInterpreter.Status(
          current.phase,
          active,
          retiring,
          current.lastAttemptAt,
          current.lastSuccessfulCheckAt,
          current.lastFailure,
          current.retirementStartedAt,
          current.retirementOverdue
        )
      }
    }

  def status(implicit trace: Trace): UIO[GatewayInterpreter.Status] =
    reloadStatus.map { current =>
      val statuses  = current.active.status :: current.retiring.toList.map(_.status)
      val lifecycle = current.phase match {
        case Phase.Closing => LifecycleState.Draining
        case Phase.Closed  => LifecycleState.Closed
        case _             => LifecycleState.Running
      }
      GatewayInterpreter.Status(
        LifecycleStatus(
          lifecycle,
          sumCounts(statuses.map(_.lifecycle.active)),
          sumCounts(statuses.map(_.lifecycle.overdue))
        ),
        sumAdmission(statuses.map(_.requests)),
        statuses
          .flatMap(_.subgraphs.keys)
          .distinct
          .map { name =>
            name -> sumAdmission(statuses.flatMap(_.subgraphs.get(name)))
          }
          .toMap,
        current.active.status.operationCache
      )
    }

  private def loop(implicit trace: Trace): UIO[Unit] =
    state.get.flatMap { current =>
      if (current.closing) ZIO.unit
      else {
        val delay =
          if (config.jitter == 0.0) ZIO.succeed(config.pollInterval)
          else
            Random.nextDouble.map { random =>
              val factor = 1.0 + (2.0 * random - 1.0) * config.jitter
              Duration.fromNanos(math.max(1L, (config.pollInterval.toNanos.toDouble * factor).toLong))
            }
        delay.flatMap(Clock.sleep(_)) *> cycle *> ZIO.suspendSucceed(loop)
      }
    }

  private def cycle(implicit trace: Trace): UIO[Unit] =
    (for {
      now <- Clock.instant
      run <- state.modify { current =>
               if (current.closing || current.retiring.nonEmpty || current.candidate.nonEmpty) false -> current
               else true                                                                             -> current.copy(phase = Phase.Checking, lastAttemptAt = Some(now))
             }
      _   <- ZIO.when(run) {
               acquire.flatMap { snapshot =>
                 state.get.flatMap { current =>
                   if (current.closing) ZIO.unit
                   else if (snapshot.fingerprints == current.active.snapshot.fingerprints) unchanged
                   else replace(snapshot)
                 }
               }
             }
    } yield ()).catchAll(error => failed(Some(error))).catchAllCause { cause =>
      if (cause.isInterrupted) ZIO.refailCause(cause)
      else failed(None)
    }

  private def unchanged(implicit trace: Trace): UIO[Unit] =
    Clock.instant.flatMap { now =>
      state.modify { current =>
        if (current.closing) false     -> current
        else
          current.lastFailure.nonEmpty -> current.copy(
            phase = Phase.Idle,
            lastSuccessfulCheckAt = now,
            lastFailure = None
          )
      }.flatMap(recovered => ZIO.logInfo("Gateway schema refresh recovered.").when(recovered).unit)
    }

  private def replace(snapshot: Gateway.Snapshot[R])(implicit trace: Trace): IO[GatewayBuildError, Unit] =
    ZIO.uninterruptibleMask { restore =>
      for {
        candidate <- Scope.make
        accepted  <- state.modify { current =>
                       if (current.closing) false -> current
                       else true                  -> current.copy(phase = Phase.Building, candidate = Some(candidate))
                     }
        _         <- if (!accepted) candidate.close(Exit.unit)
                     else
                       restore(candidate.extend(snapshot.gateway.build))
                         .onError(cause => candidate.close(Exit.failCause(cause)) *> clearCandidate(candidate))
                         .flatMap { interpreter =>
                           Clock.instant.flatMap { now =>
                             state.modify { current =>
                               if (current.closing) Option.empty[(Generation[R], Boolean)] -> current
                               else
                                 Some((current.active, current.lastFailure.nonEmpty))      -> current.copy(
                                   active = Generation(current.active.id + 1L, now, snapshot, interpreter, candidate),
                                   retiring = Some(current.active),
                                   candidate = None,
                                   phase = Phase.Draining,
                                   lastSuccessfulCheckAt = now,
                                   lastFailure = None,
                                   retirementStartedAt = Some(now),
                                   retirementOverdue = false
                                 )
                             }.flatMap {
                               case None                   => candidate.close(Exit.unit) *> clearCandidate(candidate)
                               case Some((old, recovered)) =>
                                 (ZIO.logInfo("Gateway schema refresh recovered.").when(recovered) *>
                                   ZIO.logInfo(s"Gateway activated generation ${old.id + 1L}.")).ensuring(retire(old))
                             }
                           }
                         }
      } yield ()
    }

  private def clearCandidate(candidate: Scope.Closeable)(implicit trace: Trace): UIO[Unit] =
    state.update(current => if (current.candidate.contains(candidate)) current.copy(candidate = None) else current)

  private def retire(old: Generation[R])(implicit trace: Trace): UIO[Unit] =
    for {
      watcher <- (Clock.sleep(drainTimeout) *>
                   state.modify { current =>
                     val overdue = current.retiring.exists(_.id == old.id)
                     overdue -> (if (overdue) current.copy(retirementOverdue = true) else current)
                   }.flatMap(overdue =>
                     ZIO
                       .logWarning(
                         s"Gateway generation ${old.id} exceeded its drain timeout; further refreshes are paused."
                       )
                       .when(overdue)
                   )).interruptible.fork
      _       <- old.scope.close(Exit.unit).ensuring(watcher.interrupt)
      _       <- state.update(current =>
                   current.copy(
                     retiring = None,
                     phase = if (current.closing) current.phase else Phase.Idle,
                     retirementStartedAt = None,
                     retirementOverdue = false
                   )
                 )
    } yield ()

  private def failed(error: Option[GatewayBuildError])(implicit trace: Trace): UIO[Unit] =
    Clock.instant.flatMap { now =>
      state.modify { current =>
        if (current.closing) Option.empty[Failure] -> current
        else {
          val stage    =
            if (error.isEmpty) FailureStage.Internal
            else if (current.phase == Phase.Checking) FailureStage.Acquisition
            else FailureStage.Construction
          val failure  = Failure(
            stage,
            error.fold("Unexpected refresh failure.")(safeReason),
            error.toList.flatMap(safeSubgraphs).take(16),
            now
          )
          val repeated = current.lastFailure.exists(previous =>
            previous.stage == failure.stage && previous.reason == failure.reason && previous.subgraphs == failure.subgraphs
          )
          val phase    =
            if (current.retiring.nonEmpty) Phase.Draining
            else if (current.candidate.nonEmpty) Phase.Building
            else Phase.Idle
          (if (repeated) None else Some(failure)) -> current.copy(phase = phase, lastFailure = Some(failure))
        }
      }.flatMap(value =>
        ZIO.foreachDiscard(value)(failure =>
          ZIO.logWarning(
            s"Gateway schema refresh failed (${failure.stage}): ${failure.reason} Keeping the active generation."
          )
        )
      )
    }

  private def close(worker: Fiber.Runtime[Nothing, Unit])(implicit trace: Trace): UIO[Unit] =
    (for {
      owned <- state.modify { current =>
                 val owned = current.active.scope :: current.candidate.toList
                 (owned, current.retiring.nonEmpty) -> current.copy(phase = Phase.Closing)
               }
      // Retirement owns the old drain timer: interrupting it could abandon the drain.
      // The active generation closes concurrently with that existing retirement.
      _     <- ZIO
                 .foreachParDiscard(owned._1)(_.close(Exit.unit))
                 .zipPar(if (owned._2) worker.await else worker.interrupt)
      _     <- state.update(_.copy(phase = Phase.Closed, candidate = None))
    } yield ()).uninterruptible
}

private[gateway] object ReloadableGatewayInterpreterImpl {
  private final case class Generation[-R](
    id: Long,
    activatedAt: Instant,
    snapshot: Gateway.Snapshot[R],
    interpreter: GatewayInterpreterImpl[R],
    scope: Scope.Closeable
  ) {
    def status(implicit trace: Trace): UIO[ReloadableGatewayInterpreter.Generation] =
      interpreter.status.map(ReloadableGatewayInterpreter.Generation(id, activatedAt, _))
  }

  private final case class State[-R](
    active: Generation[R],
    retiring: Option[Generation[R]],
    candidate: Option[Scope.Closeable],
    phase: Phase,
    lastAttemptAt: Option[Instant],
    lastSuccessfulCheckAt: Instant,
    lastFailure: Option[Failure],
    retirementStartedAt: Option[Instant],
    retirementOverdue: Boolean
  ) {
    def closing: Boolean = phase == Phase.Closing || phase == Phase.Closed
  }

  def make[R](
    acquire: IO[GatewayBuildError, Gateway.Snapshot[R]],
    config: GatewayReloadConfig,
    drainTimeout: Duration
  )(implicit trace: Trace): ZIO[Scope, GatewayBuildError, ReloadableGatewayInterpreter[R]] =
    ZIO.uninterruptibleMask { restore =>
      restore(acquire).flatMap { snapshot =>
        Scope.make.flatMap { initialScope =>
          (for {
            interpreter <- restore(initialScope.extend(snapshot.gateway.build))
            now         <- Clock.instant
            state       <- Ref.make(
                             State(
                               Generation(1L, now, snapshot, interpreter, initialScope),
                               None,
                               None,
                               Phase.Idle,
                               None,
                               now,
                               None,
                               None,
                               retirementOverdue = false
                             )
                           )
            runtime      = new ReloadableGatewayInterpreterImpl(acquire, config, drainTimeout, state)
            worker      <- runtime.loop.interruptible.forkDaemon
            _           <- ZIO.addFinalizer(runtime.close(worker))
          } yield runtime).onError(cause => initialScope.close(Exit.failCause(cause)))
        }
      }
    }

  private def sumAdmission(values: List[AdmissionStatus]): AdmissionStatus =
    AdmissionStatus(sumCounts(values.map(_.limit)), sumCounts(values.map(_.active)), sumCounts(values.map(_.waiting)))

  private def sumCounts(values: List[Int]): Int = values.foldLeft(0L)(_ + _).min(Int.MaxValue.toLong).toInt

  private def safeReason(error: GatewayBuildError): String = error match {
    case _: GatewayBuildError.InvalidConfiguration          => "Invalid gateway configuration."
    case _: GatewayBuildError.TransportInitializationFailed => "Unable to initialize schema transport."
    case _: GatewayBuildError.SubgraphLoadingFailed         => "Unable to load subgraph schemas."
    case _: GatewayBuildError.CombinedFailures              => "Multiple gateway build stages failed."
    case _: GatewayBuildError.SchemaCompositionFailed       => "Subgraph schemas could not be composed."
    case _: GatewayBuildError.OperationPolicyRequired       => "The schema requires an operation policy."
  }

  private def safeSubgraphs(error: GatewayBuildError): List[String] = error match {
    case GatewayBuildError.SubgraphLoadingFailed(errors) =>
      errors.take(16).map(_.name.filterNot(_.isControl).take(80)).distinct
    case _                                               => Nil
  }
}
