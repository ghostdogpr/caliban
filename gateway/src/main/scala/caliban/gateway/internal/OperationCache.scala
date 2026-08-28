package caliban.gateway.internal

import caliban.gateway.GatewayWrapper
import caliban.gateway.GatewayWrapper.{ CacheResult, Event, Result }
import zio.{ Exit, Promise, Ref, Trace, UIO, ZIO }

import scala.annotation.tailrec
import scala.collection.immutable.Queue

private[gateway] final class OperationCache[K, E, V, -R] private (
  maxWeight: Long,
  state: Ref[OperationCache.State[K, E, V]],
  wrapper: GatewayWrapper[R]
) {
  import OperationCache._

  def getOrCompute[R0 <: R](key: K)(compute: => ZIO[R0, E, Weighted[V]])(implicit trace: Trace): ZIO[R0, E, V] =
    state.get.flatMap { current =>
      current.entries.get(key) match {
        case Some(entry) => hit(entry.value)
        case None        => miss(key)(compute)
      }
    }

  private def miss[R0 <: R](key: K)(compute: => ZIO[R0, E, Weighted[V]])(implicit trace: Trace): ZIO[R0, E, V] =
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
            case Decision.Hit(value)       => restore(hit(value))
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
                state.update(_.finish(key, promise)) *> promise.interrupt.unit
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
        val result: Exit[E, V] = exit match {
          case Exit.Success(weighted) => Exit.succeed(weighted.value)
          case Exit.Failure(cause)    => Exit.failCause(cause)
        }
        state.update { current =>
          val withoutFlight = current.finish(key, promise)
          exit match {
            case Exit.Success(weighted) => withoutFlight.insert(key, weighted, maxWeight)
            case Exit.Failure(_)        => withoutFlight
          }
        } *> promise.succeed(result).unit *> (result: ZIO[Any, E, V])
      }
    }

  private def hit(value: V)(implicit trace: Trace): ZIO[R, Nothing, V] =
    observe(CacheResult.Hit)(ZIO.succeed(value))

  private def observe[R0 <: R, E0, A](
    value: CacheResult
  )(effect: ZIO[R0, E0, A])(implicit trace: Trace): ZIO[R0, E0, A] =
    if (!wrapper.enabled) effect
    else
      wrapper.wrap(Event.CacheAccess(value))(effect)(
        Result.classifyExit
      )
}

private[gateway] object OperationCache {

  final case class Weighted[+A](value: A, weight: Long)

  def make[K, E, V, R](maxWeight: Long, wrapper: GatewayWrapper[R])(implicit
    trace: Trace
  ): UIO[OperationCache[K, E, V, R]] =
    Ref.make(State.empty[K, E, V]).map(new OperationCache(maxWeight, _, wrapper))

  private final case class Entry[+V](value: V, weight: Long)

  private final case class State[K, E, V](
    entries: Map[K, Entry[V]],
    order: Queue[K],
    weight: Long,
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
                    weight = current.weight - entry.weight
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
      State(Map.empty, Queue.empty, 0L, Map.empty)
  }

  private sealed trait Decision[E, V]
  private object Decision {
    final case class Hit[E, V](value: V)                                  extends Decision[E, V]
    final case class Await[E, V](promise: Promise[Nothing, Exit[E, V]])   extends Decision[E, V]
    final case class Compute[E, V](promise: Promise[Nothing, Exit[E, V]]) extends Decision[E, V]
  }
}
