package caliban.gateway.internal

import caliban._
import caliban.gateway._
import caliban.gateway.GatewayWrapper.{ AdmissionKind, Event, Result }
import zio._
import zio.stream.ZStream

/**
 * Lifetime admission is independent of finite execution admission, including while pulling the source.
 */
private[gateway] final class SubscriptionControl[-R] private (
  config: GatewaySubscriptionConfig,
  work: AdmissionGate[R],
  wrapper: GatewayWrapper[R],
  state: Ref[SubscriptionControl.State],
  drained: Promise[Nothing, Unit]
) {
  import SubscriptionControl._

  def stop(reason: CalibanError.ExecutionError)(implicit trace: Trace): UIO[Unit] =
    state.modify { value =>
      val next = value.copy(stopped = value.stopped.orElse(Some(reason)))
      (next.active.values.toList, next.stopped.get) -> next
    }.flatMap { case (signals, why) =>
      ZIO.foreachDiscard(signals)(_.succeed(why)) *> drained.succeed(()).when(signals.isEmpty).unit
    }

  // Retain resources and admission slots until cleanup finishes.
  def close(implicit trace: Trace): UIO[Unit] = stop(SubscriptionTermination.Shutdown) *> drained.await

  def stream[R1 <: R](
    open: ZIO[R1 with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]]
  )(
    process: GraphQLResponse[CalibanError] => URIO[R1, GraphQLResponse[CalibanError]]
  )(implicit trace: Trace): ZStream[R1, Throwable, GraphQLResponse[CalibanError]] =
    ZStream.unwrapScoped[R1] {
      for {
        env             <- ZIO.environment[R1]
        admittedState   <- ZIO.uninterruptible {
                             for {
                               signal   <- Promise.make[Nothing, CalibanError.ExecutionError]
                               token     = new Object
                               started  <- Clock.nanoTime
                               reason   <- Ref.make("cancelled")
                               admitted <- state.modify { value =>
                                             val rejection = value.stopped.orElse(
                                               if (value.active.size >= config.maxActive)
                                                 Some(SubscriptionTermination.Capacity)
                                               else None
                                             )
                                             rejection -> (if (rejection.isEmpty)
                                                             value.copy(active =
                                                               value.active
                                                                 .updated(token, signal)
                                                             )
                                                           else value)
                                           }
                               _        <- ZIO.foreachDiscard(admitted)(error =>
                                             notify(Event.SubscriptionAdmission(false)) *> ZIO.fail(error)
                                           )
                               // Registered before source resources, so the slot is released last.
                               _        <- ZIO.addFinalizer {
                                             Clock.nanoTime.flatMap { ended =>
                                               reason.get
                                                 .flatMap(why => notify(Event.SubscriptionTerminated(why, ended - started)))
                                                 .ensuring(state.modify { value =>
                                                   val next = value.copy(active = value.active - token)
                                                   (next.stopped.nonEmpty && next.active.isEmpty) -> next
                                                 }.flatMap(empty => drained.succeed(()).when(empty).unit))
                                             }.provideEnvironment(env)
                                           }
                               _        <- notify(Event.SubscriptionAdmission(true))
                             } yield (signal, reason)
                           }
        (signal, reason) = admittedState
        sourceScope     <- ZIO.service[Scope]
        source          <- work
                             .observedAs[R1 with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]](
                               AdmissionKind.SubscriptionSetup
                             ) {
                               wrapper.wrap[R1 with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]](
                                 Event.SubscriptionSetup
                               )(sourceScope.extend[R1](open))(
                                 Result.fromExit(_)(
                                   _ => Result(GatewayWrapper.Outcome.Success),
                                   _ => Result(GatewayWrapper.Outcome.TransportError)
                                 )
                               )
                             }
                             .timeoutFail(SubscriptionTermination.SetupTimeout)(config.setupTimeout)
                             .raceFirst(signal.await.flatMap(ZIO.fail(_)))
                             .onExit {
                               case Exit.Failure(cause) =>
                                 reason.set(
                                   if (cause.isInterrupted) "cancelled"
                                   else cause.failureOption.fold("source_error")(terminationReason)
                                 )
                               case _                   => ZIO.unit
                             }
        queue           <- SubscriptionBuffer.make[GraphQLResponse[CalibanError]](config.bufferSize)
        _               <- source.runForeach { event =>
                             queue.offer(event).flatMap {
                               case true  => ZIO.unit
                               case false => ZIO.fail(SubscriptionTermination.Overflow)
                             }
                           }.catchAllCause(cause =>
                             if (cause.isInterruptedOnly) ZIO.unit
                             else
                               ZIO.when(cause.failureOption.exists(_ eq SubscriptionTermination.Overflow))(
                                 notify(Event.SubscriptionOverflow)
                               ) *> signal
                                 .succeed(cause.failureOption match {
                                   case Some(e: CalibanError.ExecutionError) => e
                                   case _                                    => SubscriptionTermination.Source
                                 })
                                 .unit
                           ).ensuring(queue.end)
                             .forkScoped
        events           = queue.stream.mapZIO { event =>
                             work
                               .observedAs[R1, Nothing, GraphQLResponse[CalibanError]](
                                 AdmissionKind.SubscriptionEvent
                               ) {
                                 wrapper.wrap[R1, Nothing, GraphQLResponse[CalibanError]](Event.SubscriptionEvent)(
                                   process(event)
                                 )(Result.fromExit(_)(Result.fromResponse, _ => Result(GatewayWrapper.Outcome.InternalError)))
                               }
                               .timeoutFail(SubscriptionTermination.EventTimeout)(config.eventTimeout)
                           }
                             .concat(
                               ZStream
                                 .fromZIO(signal.poll.flatMap {
                                   case Some(value) => value.flatMap(ZIO.fail(_))
                                   case None        => reason.set("complete")
                                 })
                                 .drain
                             )
                             .interruptWhen(signal.await.flatMap(ZIO.fail(_)))
                             .tapError(error => reason.set(terminationReason(error)))
                             .ensuringWith(exit => if (exit.isInterrupted) reason.set("cancelled") else ZIO.unit)
      } yield events
    }

  private def notify(event: Event)(implicit trace: Trace): URIO[R, Unit] =
    if (wrapper.enabled) wrapper.wrap(event)(ZIO.unit)(Result.classifyExit) else ZIO.unit

}

private[gateway] object SubscriptionControl {
  private final case class State(
    stopped: Option[CalibanError.ExecutionError],
    active: Map[Object, Promise[Nothing, CalibanError.ExecutionError]]
  )
  private def terminationReason(error: Throwable): String = error match {
    case value: CalibanError.ExecutionError => SubscriptionTermination.code(value)
    case _                                  => "source_error"
  }
  def make[R](
    config: GatewaySubscriptionConfig,
    work: AdmissionGate[R],
    wrapper: GatewayWrapper[R]
  )(implicit trace: Trace): ZIO[Scope, Nothing, SubscriptionControl[R]] =
    for {
      state   <- Ref.make(State(None, Map.empty))
      drained <- Promise.make[Nothing, Unit]
      control  = new SubscriptionControl(config, work, wrapper, state, drained)
    } yield control
}
