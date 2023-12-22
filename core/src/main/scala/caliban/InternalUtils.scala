package caliban

import zio.{ UIO, ZIO }

import java.util.concurrent.atomic.AtomicReference

private[caliban] object InternalUtils {

  def newAtomicRef[A](a: A): UIO[AtomicReference[A]] = ZIO.succeed(new AtomicReference(a))

  object syntax {
    implicit class AtomicRefOps[A](private val ref: AtomicReference[A]) extends AnyVal {
      def update(f: A => A): Unit = {
        var loop    = true
        var next: A = null.asInstanceOf[A]
        while (loop) {
          val current = ref.get
          next = f(current)
          loop = !ref.compareAndSet(current, next)
        }
        ()
      }
    }
  }
}
