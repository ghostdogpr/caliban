package caliban

import scala.collection.mutable

private[caliban] object syntax {
  val NullFn: () => AnyRef = () => null

  implicit class EnrichedImmutableMapOps[K, V <: AnyRef](private val self: Map[K, V]) extends AnyVal {
    def getOrElseNull(key: K): V = self.getOrElse(key, NullFn()).asInstanceOf[V]
  }

  implicit class EnrichedHashMapOps[K, V <: AnyRef](private val self: mutable.HashMap[K, V]) extends AnyVal {
    def getOrElseNull(key: K): V = self.getOrElse(key, NullFn()).asInstanceOf[V]
  }
}
