package caliban.gateway.internal

import caliban._
import caliban.gateway._
import caliban.gateway.PhaseHooks.{ AdmissionKind, Event, Result }
import zio._
import zio.stream.ZStream

/**
 * Lifetime admission is independent of finite execution admission, including while pulling the source.
 */
private[gateway] final class SubscriptionControl[-R] private (
  config: GatewaySubscriptionConfig,
  admission: AdmissionGate[R],
  hooks: PhaseHooks[R],
  state: Ref[SubscriptionControl.State],
  drained: Promise[Nothing, Unit]
) {
  import SubscriptionControl._

  def stop(reason: CalibanError.ExecutionError)(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      val next = current.copy(stopped = current.stopped.orElse(Some(reason)))
      (next.active.values.toList, next.stopped.get) -> next
    }.flatMap { case (signals, stopped) =>
      ZIO.foreachDiscard(signals)(_.succeed(stopped)) *> drained.succeed(()).when(signals.isEmpty).unit
    }

  // Retain resources and admission slots until cleanup finishes.
  def close(implicit trace: Trace): UIO[Unit] = stop(SubscriptionTermination.Shutdown) *> drained.await

  def stream[R1 <: R](
    open: ZIO[R1 with Scope, Throwable, ZStream[Any, Throwable, Response]]
  )(
    process: Response => URIO[R1, Response]
  )(implicit trace: Trace): ZStream[R1, Throwable, Response] =
    ZStream.unwrapScoped[R1] {
      for {
        env             <- ZIO.environment[R1]
        registration    <- ZIO.uninterruptible(register(env))
        (signal, reason) = registration
        source          <- openSource[R1](open, signal, reason)
        buffer          <- SubscriptionBuffer.make[Response](config.bufferSize)
        _               <- forward(source, buffer, signal).forkScoped
      } yield events(buffer, signal, reason)(process)
    }

  private def register[R1 <: R](
    env: ZEnvironment[R1]
  )(implicit trace: Trace): ZIO[R1 with Scope, CalibanError.ExecutionError, (Signal, Ref[String])] =
    for {
      signal   <- Promise.make[Nothing, CalibanError.ExecutionError]
      token     = new Object
      started  <- Clock.nanoTime
      reason   <- Ref.make(CancelledReason)
      rejected <- state.modify { current =>
                    val rejection = current.stopped.orElse(
                      if (current.active.size >= config.maxActive) Some(SubscriptionTermination.Capacity)
                      else None
                    )
                    rejection -> (if (rejection.isEmpty) current.copy(active = current.active.updated(token, signal))
                                  else current)
                  }
      _        <- ZIO.foreachDiscard(rejected)(error =>
                    notify(Event.SubscriptionAdmission(false))(hooks.subscriptionAdmission) *> ZIO.fail(error)
                  )
      // Registered before source resources, so the slot is released last.
      _        <- ZIO.addFinalizer {
                    Clock.nanoTime.flatMap { ended =>
                      reason.get
                        .flatMap(why =>
                          notify(Event.SubscriptionTerminated(why, ended - started))(hooks.subscriptionTerminated)
                        )
                        .ensuring(state.modify { current =>
                          val next = current.copy(active = current.active - token)
                          (next.stopped.nonEmpty && next.active.isEmpty) -> next
                        }.flatMap(empty => drained.succeed(()).when(empty).unit))
                    }.provideEnvironment(env)
                  }
      _        <- notify(Event.SubscriptionAdmission(true))(hooks.subscriptionAdmission)
    } yield (signal, reason)

  private def openSource[R1 <: R](
    open: ZIO[R1 with Scope, Throwable, ZStream[Any, Throwable, Response]],
    signal: Signal,
    reason: Ref[String]
  )(implicit trace: Trace): ZIO[R1 with Scope, Throwable, ZStream[Any, Throwable, Response]] =
    ZIO.serviceWithZIO[Scope] { sourceScope =>
      admission
        .admitAs[R1 with Scope, Throwable, ZStream[Any, Throwable, Response]](AdmissionKind.SubscriptionSetup) {
          hooks.subscriptionSetup
            .run[R1 with Scope, Throwable, ZStream[Any, Throwable, Response]](Event.SubscriptionSetup)(
              sourceScope.extend[R1](open)
            )(
              Result.fromExit(_)(
                _ => Result(PhaseHooks.Outcome.Success),
                _ => Result(PhaseHooks.Outcome.TransportError)
              )
            )
        }
        .timeoutFail(SubscriptionTermination.SetupTimeout)(config.setupTimeout)
        .raceFirst(signal.await.flatMap(ZIO.fail(_)))
        .onExit {
          case Exit.Failure(cause) =>
            reason.set(
              if (cause.isInterrupted) CancelledReason
              else terminationReason(cause.failureOption.getOrElse(SubscriptionTermination.Source))
            )
          case _                   => ZIO.unit
        }
    }

  private def forward(source: ZStream[Any, Throwable, Response], buffer: SubscriptionBuffer[Response], signal: Signal)(
    implicit trace: Trace
  ): URIO[R, Unit] =
    source.runForeach { event =>
      buffer.offer(event).flatMap {
        case true  => ZIO.unit
        case false => ZIO.fail(SubscriptionTermination.Overflow)
      }
    }.catchAllCause(cause =>
      if (cause.isInterruptedOnly) ZIO.unit
      else
        ZIO.whenDiscard(cause.failureOption.exists(_ eq SubscriptionTermination.Overflow))(
          notify(Event.SubscriptionOverflow)(hooks.subscriptionOverflow)
        ) *> signal
          .succeed(cause.failureOption.fold(SubscriptionTermination.Source)(SubscriptionTermination.fromFailure))
          .unit
    ).ensuring(buffer.end)

  private def events[R1 <: R](buffer: SubscriptionBuffer[Response], signal: Signal, reason: Ref[String])(
    process: Response => URIO[R1, Response]
  )(implicit trace: Trace): ZStream[R1, Throwable, Response] =
    buffer.stream.mapZIO { event =>
      admission
        .admitAs[R1, Nothing, Response](AdmissionKind.SubscriptionEvent) {
          hooks.subscriptionEvent.run[R1, Nothing, Response](Event.SubscriptionEvent)(process(event))(
            Result.classifyResponse(_)
          )
        }
        .timeoutFail(SubscriptionTermination.EventTimeout)(config.eventTimeout)
    }
      .concat(
        ZStream
          .fromZIO(signal.poll.flatMap {
            case Some(value) => value.flatMap(ZIO.fail(_))
            case None        => reason.set(CompleteReason)
          })
          .drain
      )
      .interruptWhen(signal.await.flatMap(ZIO.fail(_)))
      .tapError(error => reason.set(terminationReason(error)))
      .ensuringWith(exit => if (exit.isInterrupted) reason.set(CancelledReason) else ZIO.unit)

  private def notify[R1 <: R, Ev <: Event](event: Ev)(handler: PhaseHandler[R1, Ev, Nothing, Result])(implicit
    trace: Trace
  ): URIO[R1, Unit] =
    handler.run(event)(ZIO.unit)(Result.classifyExit)

}

private[gateway] object SubscriptionControl {
  def make[R](
    config: GatewaySubscriptionConfig,
    admission: AdmissionGate[R],
    hooks: PhaseHooks[R]
  )(implicit trace: Trace): ZIO[Scope, Nothing, SubscriptionControl[R]] =
    for {
      state   <- Ref.make(State(None, Map.empty))
      drained <- Promise.make[Nothing, Unit]
      control  = new SubscriptionControl(config, admission, hooks, state, drained)
    } yield control

  private type Response = GraphQLResponse[CalibanError]
  private type Signal   = Promise[Nothing, CalibanError.ExecutionError]

  private final val CancelledReason = "cancelled"
  private final val CompleteReason  = "complete"

  private final case class State(stopped: Option[CalibanError.ExecutionError], active: Map[Object, Signal])

  private def terminationReason(error: Throwable): String =
    SubscriptionTermination.code(SubscriptionTermination.fromFailure(error))
}
