package caliban.gateway.internal

import zio._
import zio.stm.{ STM, TQueue, TRef }
import zio.stream.ZStream

/**
 * Admission never blocks the source reader. Completion and dequeue are one transaction,
 * so a racing completion cannot steal an event from a losing queue.take fiber.
 */
private[gateway] final class SubscriptionBuffer[A] private (queue: TQueue[A], ended: TRef[Boolean]) {
  def offer(value: A)(implicit trace: Trace): UIO[Boolean]    = queue.offer(value).commit
  def end(implicit trace: Trace): UIO[Unit]                   = ended.set(true).commit
  def stream(implicit trace: Trace): ZStream[Any, Nothing, A] = ZStream.repeatZIOOption {
    queue.poll.flatMap {
      case Some(value) => STM.succeed(value)
      case None        => ended.get.flatMap[Any, Option[Nothing], A](done => if (done) STM.fail(None) else STM.retry)
    }.commit
  }
}

private[gateway] object SubscriptionBuffer {
  def make[A](capacity: Int)(implicit trace: Trace): UIO[SubscriptionBuffer[A]] =
    (for {
      queue <- TQueue.dropping[A](capacity)
      ended <- TRef.make(false)
    } yield new SubscriptionBuffer(queue, ended)).commit
}
