package caliban.gateway.internal

import caliban.gateway.GatewayRuntime.OperationCacheStatus
import zio.{ Exit, Promise, Ref, Trace, UIO, ZIO }

import scala.annotation.tailrec
import scala.collection.immutable.Queue

private[gateway] final class OperationCache[K, E, V] private (
  maxWeight: Long,
  state: Ref[OperationCache.State[K, E, V]]
) {
  import OperationCache._

  def getOrCompute[R](key: K)(compute: => ZIO[R, E, Weighted[V]])(implicit trace: Trace): ZIO[R, E, V] =
    state.get.flatMap { current =>
      current.entries.get(key) match {
        case Some(entry) => state.update(_.recordHit).as(entry.value)
        case None        => miss(key)(compute)
      }
    }

  def status(implicit trace: Trace): UIO[OperationCacheStatus] =
    state.get.map { current =>
      OperationCacheStatus(
        maxWeight,
        current.weight,
        current.entries.size,
        current.hits,
        current.misses,
        current.evictions,
        current.inFlight.size
      )
    }

  private def miss[R](key: K)(compute: => ZIO[R, E, Weighted[V]])(implicit trace: Trace): ZIO[R, E, V] =
    Promise.make[Nothing, Exit[E, V]].flatMap { fresh =>
      state
        .modify[Decision[E, V]] { current =>
          current.entries.get(key) match {
            case Some(entry) => Decision.Hit[E, V](entry.value) -> current.recordHit
            case None        =>
              current.inFlight.get(key) match {
                case Some(existing) => Decision.Await[E, V](existing) -> current.recordMiss
                case None           => Decision.Compute[E, V](fresh)  -> current.start(key, fresh)
              }
          }
        }
        .flatMap {
          case Decision.Hit(value)       => ZIO.succeed(value)
          case Decision.Await(promise)   =>
            promise.await.flatMap {
              case Exit.Failure(cause) if cause.isInterrupted => getOrCompute(key)(compute)
              case exit                                       => ZIO.suspendSucceed[Any, E, V](exit)
            }
          case Decision.Compute(promise) => complete(key, promise, compute)
        }
    }

  private def complete[R](
    key: K,
    promise: Promise[Nothing, Exit[E, V]],
    compute: => ZIO[R, E, Weighted[V]]
  )(implicit trace: Trace): ZIO[R, E, V] =
    ZIO.uninterruptibleMask { restore =>
      restore(compute).exit.flatMap { exit =>
        val result: Exit[E, V] = exit match {
          case Exit.Success(weighted) => Exit.succeed(weighted.value)
          case Exit.Failure(cause)    => Exit.failCause(cause)
        }
        state.update { current =>
          val withoutFlight = current.finish(key)
          exit match {
            case Exit.Success(weighted) => withoutFlight.insert(key, weighted, maxWeight)
            case Exit.Failure(_)        => withoutFlight
          }
        } *> promise.succeed(result).unit *> ZIO.suspendSucceed[Any, E, V](result)
      }
    }
}

private[gateway] object OperationCache {

  final case class Weighted[+A](value: A, weight: Long)

  def make[K, E, V](maxWeight: Long)(implicit trace: Trace): UIO[OperationCache[K, E, V]] =
    Ref.make(State.empty[K, E, V]).map(new OperationCache(maxWeight, _))

  private final case class Entry[+V](value: V, weight: Long)

  private final case class State[K, E, V](
    entries: Map[K, Entry[V]],
    order: Queue[K],
    weight: Long,
    inFlight: Map[K, Promise[Nothing, Exit[E, V]]],
    hits: Long,
    misses: Long,
    evictions: Long
  ) {

    def recordHit: State[K, E, V] = copy(hits = hits + 1L)

    def recordMiss: State[K, E, V] = copy(misses = misses + 1L)

    def start(key: K, promise: Promise[Nothing, Exit[E, V]]): State[K, E, V] =
      copy(inFlight = inFlight.updated(key, promise), misses = misses + 1L)

    def finish(key: K): State[K, E, V] = copy(inFlight = inFlight - key)

    def insert(key: K, weighted: Weighted[V], maxWeight: Long): State[K, E, V] = {
      val entryWeight = math.max(1L, weighted.weight)
      if (entryWeight > maxWeight) this
      else
        evict(
          copy(
            entries = entries.updated(key, Entry(weighted.value, entryWeight)),
            order = order.enqueue(key),
            weight = weight + entryWeight
          ),
          maxWeight
        )
    }

    @tailrec
    private def evict(current: State[K, E, V], maxWeight: Long): State[K, E, V] =
      if (current.weight <= maxWeight) current
      else
        current.order.dequeueOption match {
          case Some((key, remaining)) =>
            current.entries.get(key) match {
              case Some(entry) =>
                evict(
                  current.copy(
                    entries = current.entries - key,
                    order = remaining,
                    weight = current.weight - entry.weight,
                    evictions = current.evictions + 1L
                  ),
                  maxWeight
                )
              case None        => evict(current.copy(order = remaining), maxWeight)
            }
          case None                   => current.copy(weight = 0L)
        }
  }

  private object State {
    def empty[K, E, V]: State[K, E, V] =
      State(Map.empty, Queue.empty, 0L, Map.empty, 0L, 0L, 0L)
  }

  private sealed trait Decision[E, V]
  private object Decision {
    final case class Hit[E, V](value: V)                                  extends Decision[E, V]
    final case class Await[E, V](promise: Promise[Nothing, Exit[E, V]])   extends Decision[E, V]
    final case class Compute[E, V](promise: Promise[Nothing, Exit[E, V]]) extends Decision[E, V]
  }
}
