package caliban

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private[caliban] object syntax {
  val NullFn: () => AnyRef = () => null

  implicit class EnrichedImmutableMapOps[K, V <: AnyRef](private val self: Map[K, V]) extends AnyVal {
    def getOrElseNull(key: K): V = self.getOrElse(key, NullFn()).asInstanceOf[V]
  }

  implicit class EnrichedHashMapOps[K, V <: AnyRef](private val self: mutable.HashMap[K, V]) extends AnyVal {
    def getOrElseNull(key: K): V = self.getOrElse(key, NullFn()).asInstanceOf[V]
  }

  implicit class EnrichedListOps[A](private val self: List[A]) extends AnyVal {

    /**
     * In Scala 3, foreach is not inlined which can lead to performance issues when used in hot paths.
     * In Scala 2 this method simply points to foreach, but in Scala 3 it is inlined to a while loop.
     */
    @inline def foreachOne(f: A => Unit): Unit = self.foreach(f)
  }

  // The implicit classes below are for methods that don't exist in Scala 2.12 so we add them as syntax methods instead
  implicit class EnrichedListBufferOps[A](private val lb: ListBuffer[A]) extends AnyVal {
    // This method doesn't exist in Scala 2.12 so we just use `.map` for it instead
    def addOne(elem: A): ListBuffer[A]            = lb += elem
    def addAll(elems: Iterable[A]): ListBuffer[A] = lb ++= elems
    def mapInPlace[B](f: A => B): ListBuffer[B]   = lb.map(f)
  }
}
