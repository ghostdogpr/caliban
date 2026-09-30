package caliban.gateway.internal

import caliban._
import caliban.gateway._
import caliban.gateway.PhaseHooks.{ Event, Result }
import caliban.parsing.adt.OperationType
import zio._
import zio.stm.{ STM, TQueue, TRef }
import zio.stream.ZStream

/**
 * Bounds subscription lifetimes and buffered events, and coordinates source cleanup during shutdown.
 */
private[gateway] final class SubscriptionControl[-R] private (
  config: GatewaySubscriptionConfig,
  hooks: PhaseHooks[R],
  state: TRef[SubscriptionControl.State]
) {
  import SubscriptionControl._

  def stop(reason: SubscriptionTermination.Error)(implicit trace: Trace): UIO[Unit] =
    state.modify { current =>
      val stopped = current.stopped.getOrElse(reason)
      ZIO.foreachDiscard(current.active)(_.fail(stopped)) -> current.copy(stopped = Some(stopped))
    }.commit.flatten

  // Retain resources and admission slots until cleanup finishes.
  private def close(implicit trace: Trace): UIO[Unit] =
    stop(SubscriptionTermination.Shutdown) *> state.get.retryUntil(_.active.isEmpty).unit.commit

  def stream[R1 <: R](
    open: ZIO[R1 with Scope, Throwable, ZStream[Any, Throwable, Response]]
  )(
    process: Response => URIO[R1, Response]
  )(implicit trace: Trace): ZStream[R1, Throwable, Response] =
    ZStream.unwrapScoped[R1] {
      for {
        signal <- ZIO.uninterruptible(register[R1])
        source <- openSource[R1](open, signal)
        buffer <- TQueue.unbounded[Option[Response]].commit
        _      <- forward(source, buffer, signal).forkScoped
      } yield events(buffer, signal)(process)
    }

  private def register[R1 <: R](implicit
    trace: Trace
  ): ZIO[R1 with Scope, CalibanError.ExecutionError, Signal] =
    for {
      signal  <- Promise.make[CalibanError.ExecutionError, Unit]
      started <- Clock.nanoTime
      _       <- state.modify { current =>
                   current.stopped.orElse(
                     if (current.active.size >= config.maxActive) Some(SubscriptionTermination.Capacity) else None
                   ) match {
                     case Some(error) =>
                       val rejected = hooks.subscriptionAdmission(Event.SubscriptionAdmission(false))
                       (rejected *> ZIO.fail(error)) -> current
                     case None        => ZIO.unit -> current.copy(active = current.active + signal)
                   }
                 }.commit.flatten
      // Registered before source resources, so the slot is released last.
      _       <- ZIO.addFinalizer {
                   Clock.nanoTime.flatMap { ended =>
                     signal.poll
                       .flatMap(
                         _.fold[UIO[String]](Exit.succeed(CancelledReason))(
                           _.fold(SubscriptionTermination.code, _ => CompleteReason)
                         )
                       )
                       .flatMap(why => hooks.subscriptionTerminated(Event.SubscriptionTerminated(why, ended - started)))
                       .ensuring(state.update(current => current.copy(active = current.active - signal)).commit)
                   }
                 }
      _       <- hooks.subscriptionAdmission(Event.SubscriptionAdmission(true))
    } yield signal

  private def openSource[R1 <: R](
    open: ZIO[R1 with Scope, Throwable, ZStream[Any, Throwable, Response]],
    signal: Signal
  )(implicit trace: Trace): ZIO[R1 with Scope, Throwable, ZStream[Any, Throwable, Response]] =
    ZIO.serviceWithZIO[Scope] { sourceScope =>
      hooks.subscriptionSetup
        .run[R1 with Scope, Throwable, ZStream[Any, Throwable, Response]](Event.SubscriptionSetup)(
          sourceScope.extend[R1](open)
        )(exit => subscribed(Result.classifyExit(PhaseHooks.Outcome.TransportError)(exit)))
        .timeoutFail(SubscriptionTermination.SetupTimeout)(config.setupTimeout)
        .raceFirst(signal.await *> ZIO.never)
        .mapError(SubscriptionTermination.fromFailure)
        .tapErrorCause(terminate(signal, _))
    }

  /**
   * Enqueuing an event never blocks the source reader. The end marker queues behind every event,
   * so completion cannot overtake a buffered event.
   */
  private def forward(source: ZStream[Any, Throwable, Response], buffer: TQueue[Option[Response]], signal: Signal)(
    implicit trace: Trace
  ): URIO[R, Unit] =
    // buffer.offer(None) drains buffered events on success; only a source failure interrupts the consumer immediately.
    source
      .runForeach(event =>
        buffer.size.flatMap { size =>
          if (size < config.bufferSize) buffer.offer(Some(event)) else STM.fail(SubscriptionTermination.Overflow)
        }.commit
      )
      .catchAllCause(terminate(signal, _))
      .ensuring(buffer.offer(None).commit)

  private def terminate(signal: Signal, cause: Cause[Throwable])(implicit trace: Trace): UIO[Unit] =
    signal
      .fail(cause.failureOption.map(SubscriptionTermination.fromFailure).getOrElse(SubscriptionTermination.Source))
      .unless(cause.isInterruptedOnly)
      .unit

  private def events[R1 <: R](buffer: TQueue[Option[Response]], signal: Signal)(
    process: Response => URIO[R1, Response]
  )(implicit trace: Trace): ZStream[R1, CalibanError.ExecutionError, Response] =
    ZStream
      .fromTQueue(buffer)
      .collectWhileSome
      .mapZIO { event =>
        hooks.subscriptionEvent
          .run[R1, Nothing, Response](Event.SubscriptionEvent)(process(event))(exit =>
            subscribed(Result.classifyResponse(exit))
          )
          .timeoutFail(SubscriptionTermination.EventTimeout)(config.eventTimeout)
      }
      .concat(ZStream.execute(signal.succeed(()) *> signal.await))
      .interruptWhen(signal.await)
      .tapError(signal.fail(_))

}

private[gateway] object SubscriptionControl {
  def make[R](
    config: GatewaySubscriptionConfig,
    hooks: PhaseHooks[R]
  )(implicit trace: Trace): ZIO[Scope, Nothing, SubscriptionControl[R]] =
    TRef
      .make(State(None, Set.empty))
      .commit
      .map(new SubscriptionControl(config, hooks, _))
      .tap(control => ZIO.addFinalizer(control.close))

  private type Response = GraphQLResponse[CalibanError]
  private type Signal   = Promise[CalibanError.ExecutionError, Unit]

  private def subscribed(result: Result): Result = result.copy(operationType = Some(OperationType.Subscription))

  private final val CancelledReason = "cancelled"
  private final val CompleteReason  = "complete"

  private final case class State(stopped: Option[SubscriptionTermination.Error], active: Set[Signal])
}
