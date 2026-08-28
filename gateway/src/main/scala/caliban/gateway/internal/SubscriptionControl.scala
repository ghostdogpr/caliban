package caliban.gateway.internal

import caliban._
import caliban.gateway._
import caliban.gateway.GatewayWrapper.{ AdmissionKind, Event, Result }
import caliban.interop.jsoniter.BoundedOutputStream
import com.github.plokhotnyuk.jsoniter_scala.core.writeToStream
import zio._
import zio.stream.ZStream
import java.time.Instant

/**
 * Lifetime admission is independent of finite execution admission, including while pulling the source.
 */
private[gateway] final class SubscriptionControl[-R] private (
  config: GatewaySubscriptionConfig,
  drainTimeout: Duration,
  work: GatewayExecutionControl,
  wrapper: GatewayWrapper[R],
  state: Ref[SubscriptionControl.State],
  drained: Promise[Nothing, Unit]
) {
  import SubscriptionControl._

  def status(implicit trace: Trace): UIO[GatewayInterpreter.SubscriptionStatus] =
    Clock.nanoTime.zipWith(state.get) { (now, value) =>
      GatewayInterpreter.SubscriptionStatus(
        config.maxActive,
        value.active.values.count(_.phase == Establishing),
        value.active.values.count(_.phase == Streaming),
        value.active.values.count(_.phase == Terminating),
        value.active.values.count(_.stoppedAt.exists(now - _ >= drainTimeout.toNanos))
      )
    }

  def stop(reason: CalibanError.ExecutionError)(implicit trace: Trace): UIO[Unit] =
    Clock.nanoTime.flatMap { now =>
      state.modify { value =>
        val next = value.copy(
          stopped = value.stopped.orElse(Some(reason)),
          active = value.active.map { case (id, entry) =>
            id -> entry.copy(phase = Terminating, stoppedAt = entry.stoppedAt.orElse(Some(now)))
          }
        )
        (next.active.values.toList, next.stopped.get) -> next
      }.flatMap { case (entries, why) =>
        ZIO.foreachDiscard(entries)(_.stop.succeed(why)) *> drained.succeed(()).when(entries.isEmpty).unit
      }
    }

  // Like finite requests after cancellation, retain resources until cleanup finishes; drainTimeout marks overdue work.
  def close(implicit trace: Trace): UIO[Unit] = stop(SubscriptionTermination.Shutdown) *> drained.await

  def stream[R1 <: R](
    expiresAt: Option[Instant]
  )(
    open: ZIO[R1 with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]]
  )(
    process: GraphQLResponse[CalibanError] => URIO[R1, GraphQLResponse[CalibanError]]
  )(implicit trace: Trace): ZStream[R1, Throwable, GraphQLResponse[CalibanError]] =
    ZStream.unwrapScoped[R1] {
      for {
        env                    <- ZIO.environment[R1]
        _                      <- expiresAt.fold[IO[CalibanError.ExecutionError, Unit]](ZIO.unit)(expiry =>
                                    Clock.instant.flatMap(now => ZIO.fail(SubscriptionTermination.Expired).when(!now.isBefore(expiry))).unit
                                  )
        admittedState          <- ZIO.uninterruptible {
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
                                                                        .updated(token, Entry(signal, Establishing, None))
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
                                    } yield (signal, token, reason)
                                  }
        (signal, token, reason) = admittedState
        _                      <- expiresAt.fold[ZIO[R1 with Scope, Nothing, Unit]](ZIO.unit)(expiry =>
                                    (Clock.instant.flatMap(now => Clock.sleep(Duration.fromJava(java.time.Duration.between(now, expiry)))) *>
                                      signal.succeed(SubscriptionTermination.Expired)).forkScoped.unit
                                  )
        _                      <- ZIO.foreachDiscard(config.maxLifetime)(duration =>
                                    (Clock.sleep(duration) *> signal.succeed(SubscriptionTermination.Lifetime)).forkScoped
                                  )
        sourceScope            <- ZIO.service[Scope]
        source                 <- work
                                    .subscriptionWork[R1 with Scope, Throwable, ZStream[Any, Throwable, GraphQLResponse[CalibanError]]](
                                      AdmissionKind.SubscriptionSetup,
                                      wrapper
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
                                        ) *>
                                          markTerminating(token)
                                      case _                   => ZIO.unit
                                    }
        _                      <- state.update(value =>
                                    if (value.stopped.nonEmpty) value
                                    else value.copy(active = value.active.updated(token, Entry(signal, Streaming, None)))
                                  )
        queue                  <- SubscriptionBuffer.make[GraphQLResponse[CalibanError]](config.bufferSize)
        _                      <- source.runForeach { event =>
                                    // Local sources have no transport bound; remote maxResponseBytes is a separate limit.
                                    checkEventSize(event) *>
                                      queue.offer(event).flatMap {
                                        case true  => ZIO.unit
                                        case false =>
                                          notify(Event.SubscriptionOverflow) *> signal.succeed(
                                            SubscriptionTermination.Overflow
                                          ) *> ZIO.interrupt
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
        events                  = queue.stream.mapZIO { event =>
                                    work
                                      .subscriptionWork[R1, Nothing, GraphQLResponse[CalibanError]](
                                        AdmissionKind.SubscriptionEvent,
                                        wrapper
                                      ) {
                                        wrapper.wrap[R1, Nothing, GraphQLResponse[CalibanError]](Event.SubscriptionEvent)(
                                          process(event)
                                        )(Result.fromExit(_)(Result.fromResponse, _ => Result(GatewayWrapper.Outcome.InternalError)))
                                      }
                                      .timeoutFail(SubscriptionTermination.EventTimeout)(config.eventTimeout)
                                      .flatMap { response =>
                                        checkEventSize(response).as(response)
                                      }
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
                                    .ensuringWith(exit =>
                                      (if (exit.isInterrupted) reason.set("cancelled") else ZIO.unit) *>
                                        markTerminating(token)
                                    )
      } yield events
    }

  private def notify(event: Event)(implicit trace: Trace): URIO[R, Unit] =
    if (wrapper.enabled) wrapper.wrap(event)(ZIO.unit)(Result.classifyExit) else ZIO.unit

  private def markTerminating(token: Object)(implicit trace: Trace): UIO[Unit] =
    Clock.nanoTime.flatMap(now =>
      state.update(value =>
        value.copy(active =
          value.active
            .get(token)
            .fold(value.active)(entry =>
              value.active
                .updated(token, entry.copy(phase = Terminating, stoppedAt = entry.stoppedAt.orElse(Some(now))))
            )
        )
      )
    )

  private def checkEventSize(
    response: GraphQLResponse[CalibanError]
  )(implicit trace: Trace): IO[CalibanError.ExecutionError, Unit] =
    ZIO.attempt {
      val output = new BoundedOutputStream(config.maxEventBytes)
      writeToStream(response.toResponseValue, output)
    }.mapError {
      case BoundedOutputStream.LimitExceeded => SubscriptionTermination.TooLarge
      case _                                 => SubscriptionTermination.Source
    }

}

private[gateway] object SubscriptionControl {
  private val Establishing = 0
  private val Streaming    = 1
  private val Terminating  = 2

  private final case class Entry(
    stop: Promise[Nothing, CalibanError.ExecutionError],
    phase: Int,
    stoppedAt: Option[Long]
  )
  private final case class State(stopped: Option[CalibanError.ExecutionError], active: Map[Object, Entry])
  private def terminationReason(error: Throwable): String = error match {
    case value: CalibanError.ExecutionError => SubscriptionTermination.code(value)
    case _                                  => "source_error"
  }
  def make[R](
    config: GatewaySubscriptionConfig,
    drainTimeout: Duration,
    work: GatewayExecutionControl,
    wrapper: GatewayWrapper[R]
  )(implicit trace: Trace): ZIO[Scope, Nothing, SubscriptionControl[R]] =
    for {
      state   <- Ref.make(State(None, Map.empty))
      drained <- Promise.make[Nothing, Unit]
      control  = new SubscriptionControl(config, drainTimeout, work, wrapper, state, drained)
      _       <- work.onSubscriptionClose(control.close)
    } yield control
}
