package caliban.gateway.internal

import caliban.gateway.PhaseHooks
import caliban.gateway.PhaseHooks.{ CacheResult, Event, Result }
import zio.{ Exit, FiberId, Promise, Ref, Trace, UIO, ZIO }

import scala.annotation.tailrec
import scala.collection.immutable.Queue

private[gateway] final class OperationCache[K, E, V, -R] private (
  maxWeight: Long,
  state: Ref[OperationCache.State[K, E, V]],
  hooks: PhaseHooks[R]
) {
  import OperationCache._

  def getOrCompute[R0 <: R](key: K)(compute: => ZIO[R0, E, Weighted[V]])(implicit trace: Trace): ZIO[R0, E, V] =
    Promise.make[Nothing, Exit[E, V]].flatMap { fresh =>
      ZIO.uninterruptibleMask { restore =>
        state
          .modify[Decision[E, V]] { current =>
            current.entries.get(key) match {
              case Some(entry) => Decision.Hit[E, V](entry.value) -> current
              case None        =>
                current.inFlight.get(key) match {
                  case Some(existing) => Decision.Await[E, V](existing) -> current
                  case None           => Decision.Compute[E, V](fresh)  -> current.start(key, fresh)
                }
            }
          }
          .flatMap {
            case Decision.Hit(value)       => restore(observe(CacheResult.Hit)(ZIO.succeed(value)))
            case Decision.Await(promise)   =>
              restore(
                observe(CacheResult.Wait)(
                  promise.await.flatMap {
                    case Exit.Failure(cause) if cause.isInterrupted => getOrCompute(key)(compute)
                    case exit                                       => exit
                  }
                )
              )
            case Decision.Compute(promise) =>
              restore(observe(CacheResult.Miss)(complete(key, promise, compute))).onInterrupt(
                state.update(_.finish(key, promise)) *>
                  promise.succeed(Exit.interrupt(FiberId.None)).unit
              )
          }
      }
    }

  private def complete[R0](
    key: K,
    promise: Promise[Nothing, Exit[E, V]],
    compute: => ZIO[R0, E, Weighted[V]]
  )(implicit trace: Trace): ZIO[R0, E, V] =
    ZIO.uninterruptibleMask { restore =>
      restore(compute).exit.flatMap { exit =>
        val result: Exit[E, V] = exit.mapExit(_.value)
        state.update { current =>
          val withoutFlight = current.finish(key, promise)
          exit match {
            case Exit.Success(weighted) => withoutFlight.insert(key, weighted, maxWeight)
            case Exit.Failure(_)        => withoutFlight
          }
        } *> promise.succeed(result).unit *> (result: ZIO[Any, E, V])
      }
    }

  private def observe[R0 <: R, E0, A](
    result: CacheResult
  )(effect: ZIO[R0, E0, A])(implicit trace: Trace): ZIO[R0, E0, A] =
    hooks.cacheAccess.run(Event.CacheAccess(result))(effect)(Result.classifyExit)
}

private[gateway] object OperationCache {

  final case class Weighted[+A](value: A, weight: Long)

  def make[K, E, V, R](maxWeight: Long, hooks: PhaseHooks[R])(implicit trace: Trace): UIO[OperationCache[K, E, V, R]] =
    Ref.make(State.empty[K, E, V]).map(new OperationCache(maxWeight, _, hooks))

  private final case class State[K, E, V](
    entries: Map[K, Weighted[V]],
    insertionOrder: Queue[K],
    totalWeight: Long,
    inFlight: Map[K, Promise[Nothing, Exit[E, V]]]
  ) {

    def start(key: K, promise: Promise[Nothing, Exit[E, V]]): State[K, E, V] =
      copy(inFlight = inFlight.updated(key, promise))

    def finish(key: K, promise: Promise[Nothing, Exit[E, V]]): State[K, E, V] =
      if (inFlight.get(key).contains(promise)) copy(inFlight = inFlight - key)
      else this

    def insert(key: K, weighted: Weighted[V], maxWeight: Long): State[K, E, V] = {
      val entryWeight = math.max(1L, weighted.weight)
      if (entryWeight > maxWeight) this
      else
        evict(
          copy(
            entries = entries.updated(key, Weighted(weighted.value, entryWeight)),
            insertionOrder = insertionOrder.enqueue(key),
            totalWeight = totalWeight + entryWeight
          ),
          maxWeight
        )
    }

    @tailrec
    private def evict(current: State[K, E, V], maxWeight: Long): State[K, E, V] =
      if (current.totalWeight <= maxWeight) current
      else {
        val (key, remaining) = current.insertionOrder.dequeue
        evict(
          current.copy(
            entries = current.entries - key,
            insertionOrder = remaining,
            totalWeight = current.totalWeight - current.entries(key).weight
          ),
          maxWeight
        )
      }
  }

  private object State {
    def empty[K, E, V]: State[K, E, V] =
      State(Map.empty, Queue.empty, 0L, Map.empty)
  }

  private sealed trait Decision[E, V]
  private object Decision {
    final case class Hit[E, V](value: V)                                  extends Decision[E, V]
    final case class Await[E, V](promise: Promise[Nothing, Exit[E, V]])   extends Decision[E, V]
    final case class Compute[E, V](promise: Promise[Nothing, Exit[E, V]]) extends Decision[E, V]
  }
}
