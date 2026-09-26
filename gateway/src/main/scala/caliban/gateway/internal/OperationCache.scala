package caliban.gateway.internal

import caliban.gateway.PhaseHooks
import caliban.gateway.PhaseHooks.{ CacheResult, Event, Result }
import zio.{ Exit, IO, Promise, Ref, Trace, UIO, ZIO }

import scala.annotation.tailrec
import scala.collection.immutable.Queue

private[gateway] final class OperationCache[K, E, V, R] private (
  maxWeight: Long,
  state: Ref[OperationCache.State[K, E, V]],
  hooks: PhaseHooks[R]
) {
  import OperationCache._

  def getOrCompute(key: K)(compute: => IO[E, Weighted[V]])(implicit trace: Trace): ZIO[R, E, V] =
    Promise.make[Nothing, Option[Exit[E, V]]].flatMap { fresh =>
      ZIO.uninterruptibleMask { restore =>
        state.modify { current =>
          val found = current.slots.get(key)
          found -> (if (found.isEmpty) current.copy(slots = current.slots.updated(key, Running(fresh))) else current)
        }.flatMap {
          case Some(Ready(value))     => restore(observe(CacheResult.Hit)(ZIO.succeed(value)))
          case Some(Running(promise)) =>
            restore(
              observe(CacheResult.Wait)(
                promise.await.flatMap {
                  case Some(exit) => exit
                  case None       => getOrCompute(key)(compute)
                }
              )
            )
          case None                   =>
            restore(
              observe(CacheResult.Miss)(
                ZIO
                  .suspendSucceed(compute)
                  .onExit(exit => settle(key, fresh, if (exit.isInterrupted) None else Some(exit)))
                  .map(_.value)
              )
            )
              .onError(_ => settle(key, fresh, None))
        }
      }
    }

  private def settle(key: K, promise: Promise[Nothing, Option[Exit[E, V]]], outcome: Option[Exit[E, Weighted[V]]])(
    implicit trace: Trace
  ): UIO[Unit] =
    state.update(_.settle(key, promise, outcome, maxWeight)) *> promise.succeed(outcome.map(_.mapExit(_.value))).unit

  private def observe(result: CacheResult)(effect: ZIO[R, E, V])(implicit trace: Trace): ZIO[R, E, V] =
    hooks.cacheAccess.run(Event.CacheAccess(result))(effect)(Result.classifyExit)
}

private[gateway] object OperationCache {

  final case class Weighted[+A](value: A, weight: Long)

  def make[K, E, V, R](maxWeight: Long, hooks: PhaseHooks[R])(implicit trace: Trace): UIO[OperationCache[K, E, V, R]] =
    Ref.make(State[K, E, V](Map.empty, Queue.empty, 0L)).map(new OperationCache(maxWeight, _, hooks))

  private sealed trait Slot[E, V]
  private final case class Ready[E, V](value: V)                                        extends Slot[E, V]
  private final case class Running[E, V](promise: Promise[Nothing, Option[Exit[E, V]]]) extends Slot[E, V]

  private final case class State[K, E, V](slots: Map[K, Slot[E, V]], order: Queue[Weighted[K]], totalWeight: Long) {

    def settle(
      key: K,
      promise: Promise[Nothing, Option[Exit[E, V]]],
      outcome: Option[Exit[E, Weighted[V]]],
      maxWeight: Long
    ): State[K, E, V] =
      slots.get(key) match {
        case Some(Running(`promise`)) =>
          outcome match {
            case Some(Exit.Success(Weighted(value, weight))) =>
              val entryWeight = math.max(1L, weight)
              if (entryWeight > maxWeight) copy(slots = slots - key)
              else
                State(
                  slots.updated(key, Ready(value)),
                  order.enqueue(Weighted(key, entryWeight)),
                  totalWeight + entryWeight
                ).evict(maxWeight)
            case _                                           => copy(slots = slots - key)
          }
        case _                        => this
      }

    @tailrec
    private def evict(maxWeight: Long): State[K, E, V] =
      if (totalWeight <= maxWeight) this
      else {
        val (oldest, remaining) = order.dequeue
        State(slots - oldest.value, remaining, totalWeight - oldest.weight).evict(maxWeight)
      }
  }
}
